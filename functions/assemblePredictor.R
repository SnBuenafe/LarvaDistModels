# DESCRIPTION: Assemble the predictors

assemblePredictor <- function(spp_list, # species list
                              pred_label # predictor label
                              ) {
  
  season_list <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
  
  predictor <- list()
  for(j in 1:length(season_list)) {
    
    df <- list() # empty list
    for(i in 1:length(spp_list)) {
      obj <- readRDS(here::here(preds_dir, paste(toupper(spp_list[i]), paste0(season_list[j], ".rds"), sep = "_")))
      df[[i]] <- prepare_components(obj, spp_list[i]) %>% 
        dplyr::select(-grid_100_category, -geometry)
    }
    
    df <- purrr::reduce(df, dplyr::left_join, by = c("cellID"))
    
    if(pred_label %in% c("coast_distance", "gebco")) {
      
      predictor[[j]] <- readRDS(here::here("Data", paste0(pred_label, ".rds"))) %>% # load predictor data set
        dplyr::select(-geometry) %>% 
        dplyr::left_join(df, ., by = "cellID")
      
    } else {
      
      predictor[[j]] <- readRDS(here::here(clim_dir, paste0(pred_label, paste("", "historical", season_list[j], "interpolated.rds", sep = "_")))) %>% # load predictor data set
        dplyr::select(-geometry) %>% 
        dplyr::left_join(df, ., by = "cellID")
      
    }
    

  }
  
  preds_allSeasons <- purrr::reduce(predictor, dplyr::bind_rows)
  
  return(preds_allSeasons)
  
}


