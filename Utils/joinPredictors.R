# Description: Joining predictors and response
joinPredictors <- function(grid, tos, o2os, phos, chlos, sos, mlotst, no3os, po4os, nh4os, tf, sf, eke, bathy, dist2coast, species, season = TRUE) {
  df <- dplyr::left_join(tos, o2os, by = "cellID") %>% 
    dplyr::left_join(., phos, by = "cellID") %>% 
    dplyr::left_join(., chlos, by = "cellID") %>% 
    dplyr::left_join(., sos, by = "cellID") %>% 
    dplyr::left_join(., mlotst, by = "cellID") %>% 
    dplyr::left_join(., no3os, by = "cellID") %>% 
    dplyr::left_join(., po4os, by = "cellID") %>% 
    dplyr::left_join(., nh4os, by = "cellID") %>% 
    dplyr::left_join(., tf, by = "cellID") %>% 
    dplyr::left_join(., sf, by = "cellID") %>% 
    dplyr::left_join(., eke, by = "cellID") %>% 
    dplyr::left_join(., bathy, by = "cellID") %>% 
    dplyr::left_join(., dist2coast, by = "cellID") %>% 
    dplyr::left_join(., species, by = "cellID") %>% 
    dplyr::left_join(grid, ., by = "cellID") # Join with species data
  
  if(isTRUE(season)) {
    df %<>% dplyr::select(cellID, species, abundance, season, longitude, latitude, ocean, tos_transformed, o2os_transformed, phos_transformed, chlos_transformed, sos_transformed, mlotst_transformed, no3os_transformed, po4os_transformed, nh4os_transformed, thermal_front_transformed, salinity_front_transformed, eke, meanDepth, coastDistance, Thunnus_albacares, Katsuwonus_pelamis, Thunnus_alalunga, Thunnus_obesus, Thunnus_atlanticus, Auxis_rochei, Auxis_thazard, Xiphias_gladius, Makaira_nigricans, Tetrapturus_angustirostris, Kajikia_audax, Kajikia_albida, Istiophorus_platypterus, Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus, Scombrolabrax_heterolepis, geometry) # arrange columns
  } else {
    df %<>% dplyr::select(cellID, species, abundance, longitude, latitude, ocean, tos_transformed, o2os_transformed, phos_transformed, chlos_transformed, sos_transformed, mlotst_transformed, no3os_transformed, po4os_transformed, nh4os_transformed, thermal_front_transformed, salinity_front_transformed, eke, meanDepth, coastDistance, Thunnus_albacares, Katsuwonus_pelamis, Thunnus_alalunga, Thunnus_obesus, Thunnus_atlanticus, Auxis_rochei, Auxis_thazard, Xiphias_gladius, Makaira_nigricans, Tetrapturus_angustirostris, Kajikia_audax, Kajikia_albida, Istiophorus_platypterus, Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus, Scombrolabrax_heterolepis, geometry) # arrange columns
  }
  
  
  return(df)
}
