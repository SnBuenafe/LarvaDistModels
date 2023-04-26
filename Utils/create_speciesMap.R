# Description: Preparing object for plotting
create_speciesMap <- function(train_tmp, # train object
                              test_tmp, # test object
                              season_name, 
                              predict_df, # grid cells that model will predict values
                              model, 
                              grid_season # full grid
                              ) {
  
  plot_df <- dplyr::bind_rows(train_tmp, test_tmp) %>% 
    dplyr::arrange(cellID) %>% 
    dplyr::filter(season %in% season_name)
  
  # Predict for the rest of the ocean cells
  preds <- gbm::predict.gbm(model, predict_df, n.trees = model$gbm.call$best.trees, type = "response")
  
  plot_predict <- predict_df %>% 
    dplyr::mutate(model = preds)
  
  plot_model <- dplyr::bind_rows(plot_df, plot_predict) %>% 
    dplyr::arrange(cellID) %>%  # Make sure everything is arranged by cellID
    dplyr::select(model) %>% 
    pull()
  
  gg <- grid_season %>% 
    dplyr::bind_cols(., model = plot_model) %>% 
    dplyr::select(cellID, ocean, model, geometry) %>% 
    sf::st_as_sf(sf_column_name = "geometry")
  
  # gg_abun <- grid_season %>%
  #   dplyr::mutate(abundance_presence = factor(case_when(abundance > 0 ~ 1,
  #                                                       abundance == 0 ~ 0), levels = c(1, 0))) %>% 
  #   sf::st_as_sf(sf_column_name = "geometry") %>% 
  #   sf::st_centroid() 
  
  return(gg)
}
