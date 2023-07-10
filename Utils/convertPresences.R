# DESCRIPTION: Using the median of the probabilities for each taxon as a threshold, we convert the model outputs to 1s and 0s

convertPresences <- function(df, spp_list, season, variable) {
  
  dum_list <- list()
  
  for(i in 1:length(spp_list)) {
    dum_list[[i]] <- df %>% 
      dplyr::mutate(!!sym(spp_list[i]) := case_when(!!sym(spp_list[i]) >= median(df[[spp_list[i]]], na.rm = TRUE) ~ !!sym(variable), 
                                                    TRUE ~ NA)) %>% 
      dplyr::select(!!sym(spp_list[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::bind_cols) %>% 
    dplyr::rename_with(.fn = function(.x){paste0(.x, "_", season)})# bind columns of list
  
}
