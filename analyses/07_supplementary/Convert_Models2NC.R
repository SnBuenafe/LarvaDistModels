# DESCRIPTION: Sending NC files to SPC

# Load libraries
library(tidyverse)
library(terra)
library(here)
library(sf)
library(purrr)

# Define directories
model_output_dir <- here("data_output", "predictions")
output_dir <- here("analyses", "07_supplementary")

# Species
spp_list <- c("SKP", "YFT", "BET", "ALB")

convert_models <- function(spp) {
  
  seasons_list <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
  
  # Load model
  load_seasons <- function(season) {
    
    mod <- readRDS(here(model_output_dir, paste0(spp, "_", season, ".rds"))) %>% 
      as_tibble() %>% 
      select(cellID, model, geometry) %>% 
      rename(!!sym(paste0(spp, "_", season)) := model)
    
    return(mod)
    
  }
  
  model_list <- map(seasons_list, load_seasons) %>% 
    reduce(., full_join, by = c("cellID", "geometry")) %>% 
    select(cellID, starts_with(spp), geometry) %>%  # arrange columns
    st_as_sf() %>%  # convert to sf
    st_centroid() %>% 
    mutate(X_coords = round(st_coordinates(.)[,"X"], 1),
           Y_coords = round(st_coordinates(.)[,"Y"], 1)) %>% 
    st_drop_geometry() %>% 
    select(X_coords, Y_coords, everything()) %>% 
    as.data.frame()
  
  # Save as .csv
  write_csv(model_list, file = here(output_dir, paste0(spp, "_model_results.csv")))
  
  # Save the .nc
  model_full <- rast(model_list, crs = "EPSG:4326", type = "xyz")
  terra::writeCDF(model_full, filename = here(output_dir, paste0(spp, "_model_results.nc")), overwrite = TRUE)
  
  
}

# Execute purrr function

walk(spp_list, convert_models)



