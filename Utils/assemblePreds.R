# DESCRIPTION: Assemble data frame to plot predictor plots

assemblePreds <- function(predictor) {
  
  if(predictor == "eke") {
    var <- predictor
  } else {
    var <- paste0(predictor, "_transformed") 
  }
  
  # January-March
  df <- read_csv(here::here(preds_dir, "FULL_predictions_jan-mar.csv")) %>% # load full prediction data set
    dplyr::select(-1)
  
  tmp1 <- readRDS(here::here(clim_dir, paste0(predictor, "_historical_jan-mar_interpolated.rds"))) %>% # load predictor data set
    dplyr::select(-geometry) %>% 
    dplyr::left_join(., df, by = "cellID") %>% 
    convertPresences(., spp_list, "January-March", var)
  
  # April-June
  df <- read_csv(here::here(preds_dir, "FULL_predictions_apr-jun.csv")) %>% # load full prediction data set
    dplyr::select(-1)
  
  tmp2 <- readRDS(here::here(clim_dir, paste0(predictor, "_historical_apr-jun_interpolated.rds"))) %>% # load predictor data set
    dplyr::select(-geometry) %>% 
    dplyr::left_join(., df, by = "cellID") %>% 
    convertPresences(., spp_list, "April-June", var)
  
  # July-September
  df <- read_csv(here::here(preds_dir, "FULL_predictions_jul-sept.csv")) %>% # load full prediction data set
    dplyr::select(-1)
  
  tmp3 <- readRDS(here::here(clim_dir, paste0(predictor, "_historical_jul-sept_interpolated.rds"))) %>% # load predictor data set
    dplyr::select(-geometry) %>% 
    dplyr::left_join(., df, by = "cellID") %>% 
    convertPresences(., spp_list, "July-September", var)
  
  # October-December
  df <- read_csv(here::here(preds_dir, "FULL_predictions_oct-dec.csv")) %>% # load full prediction data set
    dplyr::select(-1)
  
  tmp4 <- readRDS(here::here(clim_dir, paste0(predictor, "_historical_oct-dec_interpolated.rds"))) %>% # load predictor data set
    dplyr::select(-geometry) %>% 
    dplyr::left_join(., df, by = "cellID") %>% 
    convertPresences(., spp_list, "October-December", var)
  
  fin_tmp <- purrr::reduce(list(tmp1, tmp2, tmp3, tmp4), dplyr::bind_cols) # join all seasons
  
  return(fin_tmp)
}

assembleOthers <- function(predictor) {
  
  if(predictor == "meanDepth") {
    var <- "gebco.rds"
  } else {
    var <- "CoastDistance.rds"
  }
  
  # January-March
  df <- read_csv(here::here(preds_dir, "FULL_predictions_jan-mar.csv")) %>% # load full prediction data set
    dplyr::select(-1)
  
  tmp1 <- readRDS(here::here("Data", var)) %>% # load predictor data set
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, !!sym(predictor)) %>% 
    dplyr::left_join(., df, by = "cellID") %>% 
    convertPresences(., spp_list, "January-March", predictor)
  
  # April-June
  df <- read_csv(here::here(preds_dir, "FULL_predictions_apr-jun.csv")) %>% # load full prediction data set
    dplyr::select(-1)
  
  tmp2 <- readRDS(here::here("Data", var)) %>% # load predictor data set
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, !!sym(predictor)) %>% 
    dplyr::left_join(., df, by = "cellID") %>% 
    convertPresences(., spp_list, "April-June", predictor)
  
  # July-September
  df <- read_csv(here::here(preds_dir, "FULL_predictions_jul-sept.csv")) %>% # load full prediction data set
    dplyr::select(-1)
  
  tmp3 <- readRDS(here::here("Data", var)) %>% # load predictor data set
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, !!sym(predictor)) %>% 
    dplyr::left_join(., df, by = "cellID") %>% 
    convertPresences(., spp_list, "July-September", predictor)
  
  # October-December
  df <- read_csv(here::here(preds_dir, "FULL_predictions_oct-dec.csv")) %>% # load full prediction data set
    dplyr::select(-1)
  
  tmp4 <- readRDS(here::here("Data", var)) %>% # load predictor data set
    dplyr::as_tibble() %>% 
    dplyr::select(cellID, !!sym(predictor)) %>% 
    dplyr::left_join(., df, by = "cellID") %>% 
    convertPresences(., spp_list, "October-December", predictor)
  
  fin_tmp <- purrr::reduce(list(tmp1, tmp2, tmp3, tmp4), dplyr::bind_cols) # join all seasons
  
  return(fin_tmp)
}
