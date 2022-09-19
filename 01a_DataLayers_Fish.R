source("00_DataLayers_Utils.R")

inPath <- "Data/"

# assemble data
YFT1 <- vroom::vroom(file = paste0(inPath, "Fish/", "CSVFile_", "yellowfin-tuna", "_jan-mar.csv"),
                     show_col_types = FALSE) %>% 
  dplyr::select(-FAO_Major_Fishing_Areas, -FAO_CWP_Code)
YFT2 <- vroom::vroom(file = paste0(inPath, "Fish/", "CSVFile_", "yellowfin-tuna", "_apr-jun.csv"),
                     show_col_types = FALSE) %>% 
  dplyr::select(-FAO_Major_Fishing_Areas, -FAO_CWP_Code)
YFT3 <- vroom::vroom(file = paste0(inPath, "Fish/", "CSVFile_", "yellowfin-tuna", "_jul-sept.csv"),
                     show_col_types = FALSE) %>% 
  dplyr::select(-FAO_Major_Fishing_Areas, -FAO_CWP_Code)
YFT4 <- vroom::vroom(file = paste0(inPath, "Fish/", "CSVFile_", "yellowfin-tuna", "_oct-dec.csv"),
                     show_col_types = FALSE) %>% 
  dplyr::select(-FAO_Major_Fishing_Areas, -FAO_CWP_Code)

# join all data.frames
YFT <- dplyr::bind_rows(YFT1, YFT2, YFT3, YFT4) %>% 
  dplyr::mutate(cellID = factor(row_number())) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.)),
                abundance = ifelse(abundance > 0, yes = 1, no = 0)) %>% 
  dplyr::select(cellID, species, abundance, everything()) %>% 
  as.data.frame()

# dummy
YFT.model0 <- dismo::gbm.step(data = YFT, gbm.x = 4:6,
                              gbm.y = 3, family = "bernoulli", tree.complexity = 5,
                              learning.rate = 0.01, bag.fraction = 0.5)


angaus.tc5.lr01 <- gbm.step(data = Anguilla_train, gbm.x = 3:13, gbm.y = 2,
                            family = "bernoulli", tree.complexity = 5,
                            learning.rate = 0.01, bag.fraction = 0.5)
