# DESCRIPTION: Assemble the predictors

assemblePredictor <- function(spp_list, # species list
                              pred_label # predictor label
                              ) {
  
  season_list <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")
  
  predictor <- list()
  for(j in 1:length(season_list)) {
    
    df <- list() # empty list
    for(i in 1:length(spp_list)) {
      obj <- readRDS(file.path(preds_dir, paste(toupper(spp_list[i]), paste0(season_list[j], ".rds"), sep = "_")))
      df[[i]] <- prepare_components(obj, spp_list[i]) %>% 
        dplyr::select(-grid_100_category, -geometry)
    }
    
    df <- purrr::reduce(df, dplyr::left_join, by = c("cellID"))
    
    if(pred_label %in% c("coast_distance", "gebco")) {
      
      predictor[[j]] <- readRDS(file.path("data_input", paste0(pred_label, ".rds"))) %>% # load predictor data set
        dplyr::select(-geometry) %>% 
        dplyr::left_join(df, ., by = "cellID")
      
    } else if(pred_label == "chlos") {
      
      predictor[[j]] <- readRDS(file.path(clim_dir, paste0(pred_label, paste("", "omip2", season_list[j], "interpolated.rds", sep = "_")))) %>% # load predictor data set
        dplyr::select(-geometry) %>% 
        dplyr::mutate(chlos_transformed = chlos_transformed/1000) %>% # transform to g/m3
        dplyr::left_join(df, ., by = "cellID")
      
    }  else if(pred_label == "o2os") {
      
      predictor[[j]] <- readRDS(file.path(clim_dir, paste0(pred_label, paste("", "omip2", season_list[j], "interpolated.rds", sep = "_")))) %>% # load predictor data set
        dplyr::select(-geometry) %>% 
        dplyr::mutate(o2os_transformed = o2os_transformed*32000/1000) %>% # transform to mg / L
        dplyr::left_join(df, ., by = "cellID")
      
    } else {
      
      predictor[[j]] <- readRDS(file.path(clim_dir, paste0(pred_label, paste("", "omip2", season_list[j], "interpolated.rds", sep = "_")))) %>% # load predictor data set
        dplyr::select(-geometry) %>% 
        dplyr::left_join(df, ., by = "cellID")
      
    }
    

  }
  
  preds_allSeasons <- purrr::reduce(predictor, dplyr::bind_rows)
  
  return(preds_allSeasons)
  
}


