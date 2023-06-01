fRunPCA <- function(df,
                    spp # list of species
) {
  
  pca_df <- df %>%  # convert sf to a tibble and select model columns
    dplyr::as_tibble() %>% 
    dplyr::select(all_of(tolower(spp)))
  
  pca <- stats::princomp(pca_df, cor = FALSE)
  
  scores <- pca$scores[,1:2] %>% 
    tibble::as_tibble() %>% 
    dplyr::rename(PCA_1 = Comp.1,
                  PCA_2 = Comp.2) %>% 
    dplyr::bind_cols(., df) %>% 
    dplyr::relocate(cellID) # arrange columns
  
  return(scores)
}