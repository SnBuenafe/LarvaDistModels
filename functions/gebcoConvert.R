# Description: Convert bathymetry data (.TIFF) to sf (based on the grid provided)
# NOTE: This code is data-intensive. You need the raw files to run this code. See https://www.gebco.net/data_and_products/gridded_bathymetry_data/

# Load packages
# install.packages("pacman")
pacman::p_load(terra, purrr)

gebcoConvert <- function(grid) {
  
  path <- here::here(output_dir, "GEBCO")
  list <- list.files(path)
  x <- apply(outer(list, ".tif", stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(x == 1)
  
  grid_tibble <- grid %>% 
    tibble::as_tibble()
  grid_filled <- list()
  
  for(i in 1:length(file)) {
    
    gebco <- terra::rast(file.path(path, list[file[i]])) %>% 
      terra::aggregate(., fact = 10)
    
    # convert to sf object
    gebco_reclassified_sf <- gebco %>% 
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = FALSE) %>% 
      sf::st_as_sf(crs = lonlat) %>% 
      fSpatPlan_Convert2PacificCentered(., cCRS = cCRS)
    
    colnames(gebco_reclassified_sf)[1] <- "depth" # change column name of the first column
    
    time <- system.time(grid_filled[[i]] <- gebco_reclassified_sf %>% # interpolate to grid
                          sf::st_interpolate_aw(grid, extensive = FALSE) %>% 
                          tibble::as_tibble() %>% 
                          dplyr::left_join(grid_tibble, ., by = "geometry"))
    
    print(time)
    
    print("Interpolated")
  }
  
  # rename columns
  for(i in 1:8) {
    name = paste0("depth", i)
    grid_filled[[i]] %<>% 
      dplyr::rename(!!sym(name) := depth) %>% 
      dplyr::select(-ocean)
  }
  
  joined <- purrr::reduce(grid_filled, dplyr::left_join, by = c("cellID", "geometry")) # join all 8 files
  
  joined %<>% 
    dplyr::mutate(meanDepth = rowMeans(joined[, 3:10], na.rm = TRUE))
  
  joined_sf <- joined %>% # converting into sf
    sf::st_as_sf(sf_column_name = "geometry") %>% 
    dplyr::select(cellID, meanDepth, geometry)
  
  saveRDS(joined_sf, here::here(output_dir, paste0("gebco", ".rds")))
  
  return(joined_sf)
  
}
