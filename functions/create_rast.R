# DESCRIPTION: Create raster files for model outputs

create_rast <- function(spp_list = c("yft", "skp", "alb", "swo", "blum", 
                                     "fri", "bet", "bft", "sau", "sail", 
                                     "sbft", "slt", "shos", "strm", "lesc"),
                        season) {
  
  nc <- rast(grid, ncols = 360, nrow = 180) # create empty raster
  
  empt <- list() # create empty list
  
  
  for(i in 1:length(spp_list)) {
    
    # Load model outputs
    model <- readRDS(here::here(preds_dir, paste(toupper(spp_list[i]), paste0(season, ".rds"), sep = "_"))) %>% 
      dplyr::as_tibble() %>% 
      dplyr::select(cellID, model)
    
    # Load predictors for that month
    # preds <- read_csv(here::here(input_dir, paste(toupper(spp_list[i]), paste0(season, ".csv"), sep = "_")), show_col_types = FALSE) %>% 
      preds <- readRDS(here::here(input_dir, paste(toupper(spp_list[i]), paste0(season, ".rds"), sep = "_"))) %>% 
      dplyr::select(cellID, longitude, latitude)
    
    # Create SpatVect
    mod <- dplyr::left_join(preds, model, by = "cellID") %>% 
      dplyr::mutate(longitude = ifelse((longitude > 180 & longitude <= 360), yes = longitude - 360, no = longitude)) %>% # convert longitude back to -180 to +180
      dplyr::select(-cellID) %>% 
      dplyr::filter(!is.na(model)) %>% # only select points with model values
      terra::vect(., geom = c("longitude", "latitude"), crs = lonlat) # convert to SpatVect
    
    empt[[i]] <- rasterize(mod, nc, "model")
    
    names(empt[[i]]) <- spp_list[i]
    
  }
  
  return(empt)
}
