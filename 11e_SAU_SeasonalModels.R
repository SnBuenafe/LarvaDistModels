# DESCRIPTION: Make seasonal models using adutl distributions as predictors

# Load SAU data
source("11a_SAU_Data.R")

#### Full model ####
CVGrid <- CVgridSearch(train, test, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:21, 23:24, 38:40), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

SAU_model3 <- dismo::gbm.step(data = train, gbm.x = c(7:21, 23:24, 38:40),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.009
)

summary(SAU_model3) # get the relative importance of each of the predictors

# AUCs
SAU_model3$self.statistics$discrimination # Training AUC Score
SAU_model3$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(SAU_model3, test, n.trees = SAU_model3$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test[, "abundance_presence"], preds, family = "bernoulli")
.roc(test$abundance_presence, preds) # Get testing AUC

train_tmp <- train %>% 
  dplyr::mutate(model = SAU_model3$fitted)
preds <- gbm::predict.gbm(SAU_model3, test, n.trees = SAU_model3$gbm.call$best.trees, type = "response") # predict to test

test_tmp <- test %>% 
  dplyr::mutate(model = preds)

#### January-March
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jan-mar", # season
                        SAU_predict_season1, # rest of the ocean cells
                        SAU_model3, # BRT model
                        `grid_SAU_jan-mar` # grid of species for specific season with restricted ranges
)

restricted_cellID <- SAU_ds1 %>% # Restricting the model to just the adult's range
  dplyr::filter((!is.na(Cololabis_saira) | !is.na(Cololabis_adocetus) | !is.na(Scomberesox_saurus)) & (Cololabis_saira >= 0.01 | Cololabis_adocetus >= 0.01 | Scomberesox_saurus >= 0.01)) %>% 
  dplyr::select(cellID) %>% 
  pull()

ggsquish1 <- plotSquishedModel(gg[[1]] %>% 
                                 dplyr::filter(cellID %in% restricted_cellID)) # Plot the squished model

#### April-June
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "apr-jun", # season
                        SAU_predict_season2, # rest of the ocean cells
                        SAU_model3, # BRT model
                        `grid_SAU_apr-jun` # grid of species for specific season
)

restricted_cellID <- SAU_ds2 %>% # Restricting the model to just the adult's range
  dplyr::filter((!is.na(Cololabis_saira) | !is.na(Cololabis_adocetus) | !is.na(Scomberesox_saurus)) & (Cololabis_saira >= 0.01 | Cololabis_adocetus >= 0.01 | Scomberesox_saurus >= 0.01)) %>% 
  dplyr::select(cellID) %>% 
  pull()

ggsquish2 <- plotSquishedModel(gg[[1]] %>% 
                                 dplyr::filter(cellID %in% restricted_cellID)) # Plot the squished model

#### July-September
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sept", # season
                        SAU_predict_season3, # rest of the ocean cells
                        SAU_model3, # BRT model
                        `grid_SAU_jul-sept` # grid of species for specific season
)

restricted_cellID <- SAU_ds3 %>% # Restricting the model to just the adult's range
  dplyr::filter((!is.na(Cololabis_saira) | !is.na(Cololabis_adocetus) | !is.na(Scomberesox_saurus)) & (Cololabis_saira >= 0.01 | Cololabis_adocetus >= 0.01 | Scomberesox_saurus >= 0.01)) %>% 
  dplyr::select(cellID) %>% 
  pull()

ggsquish3 <- plotSquishedModel(gg[[1]] %>% 
                                 dplyr::filter(cellID %in% restricted_cellID)) # Plot the squished model

#### October-December
gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "oct-dec", # season
                        SAU_predict_season4, # rest of the ocean cells
                        SAU_model3, # BRT model
                        `grid_SAU_oct-dec`# grid of species for specific season
)

restricted_cellID <- SAU_ds4 %>% # Restricting the model to just the adult's range
  dplyr::filter((!is.na(Cololabis_saira) | !is.na(Cololabis_adocetus) | !is.na(Scomberesox_saurus)) & (Cololabis_saira >= 0.01 | Cololabis_adocetus >= 0.01 | Scomberesox_saurus >= 0.01)) %>% 
  dplyr::select(cellID) %>% 
  pull()

ggsquish4 <- plotSquishedModel(gg[[1]] %>% 
                                 dplyr::filter(cellID %in% restricted_cellID)) # Plot the squished model

ggsquished <- (ggsquish1 + ggsquish2) / (ggsquish3 + ggsquish4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = ggsquished, filename = "Figures/SAU/SAU_model3_squished.png", width = 27, height = 15, dpi = 600)

#### Trying to combine all species into just one predictor ####

train_comb <- train %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(adult = mean(c(Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus), na.rm = TRUE)) %>% 
  ungroup() %>% 
  as.data.frame()

test_comb <- test %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(adult = mean(x = c(Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus), na.rm = TRUE)) %>% 
  ungroup() %>% 
  as.data.frame()

CVGrid <- CVgridSearch(train_comb, test_comb, tc = c(1, 2), bf = c(0.5, 0.75), lr = seq(0.005, 0.01, 0.001), pred_in = c(7:21, 23:24, 42), resp_in = 5)

print(CVGrid %>% dplyr::arrange(desc(test_AUC)), n = 1) # BEST TEST AUC

SAU_model4 <- dismo::gbm.step(data = train_comb, gbm.x = c(7:21, 23:24, 42),
                              gbm.y = 5, family = "bernoulli", n.folds = 5,
                              tree.complexity = 2, bag.fraction = 0.75, learning.rate = 0.007
)

summary(SAU_model4) # get the relative importance of each of the predictors

# AUCs
SAU_model4$self.statistics$discrimination # Training AUC Score
SAU_model4$cv.statistics$discrimination.mean # Validation AUC Score

preds <- gbm::predict.gbm(SAU_model4, test_comb, n.trees = SAU_model4$gbm.call$best.trees, type = "response") # predict to test

dismo::calc.deviance(test_comb[, "abundance_presence"], preds, family = "bernoulli")
.roc(test_comb$abundance_presence, preds) # Get testing AUC

  train_tmp <- train_comb %>% 
    dplyr::mutate(model = SAU_model4$fitted)
  preds <- gbm::predict.gbm(SAU_model4, test_comb, n.trees = SAU_model4$gbm.call$best.trees, type = "response") # predict to test
  
  test_tmp <- test_comb %>% 
    dplyr::mutate(model = preds)
  
  #### January-March
  gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                          test_tmp, # testing object with model column (predictions)
                          "jan-mar", # season
                          SAU_predict_season1 %>% 
                            dplyr::rowwise() %>% 
                            dplyr::mutate(adult = mean(x = c(Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus), na.rm = TRUE)) %>% 
                            ungroup(),
                          SAU_model4, # BRT model
                          `grid_SAU_jan-mar` # grid of species for specific season with restricted ranges
  )
  
  restricted_cellID <- SAU_ds1 %>% # Restricting the model to just the adult's range
    dplyr::filter((!is.na(Cololabis_saira) | !is.na(Cololabis_adocetus) | !is.na(Scomberesox_saurus)) & (Cololabis_saira >= 0.01 | Cololabis_adocetus >= 0.01 | Scomberesox_saurus >= 0.01)) %>% 
    dplyr::select(cellID) %>% 
    pull()
  
  ggsquish1 <- plotSquishedModel(gg[[1]] %>% 
                                   dplyr::filter(cellID %in% restricted_cellID)) # Plot the squished model
  
  #### April-June
  gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                          test_tmp, # testing object with model column (predictions)
                          "apr-jun", # season
                          SAU_predict_season2 %>% 
                            dplyr::rowwise() %>% 
                            dplyr::mutate(adult = mean(x = c(Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus), na.rm = TRUE)) %>% 
                            ungroup(), # rest of the ocean cells
                          SAU_model4, # BRT model
                          `grid_SAU_apr-jun` # grid of species for specific season
  )
  
  restricted_cellID <- SAU_ds2 %>% # Restricting the model to just the adult's range
    dplyr::filter((!is.na(Cololabis_saira) | !is.na(Cololabis_adocetus) | !is.na(Scomberesox_saurus)) & (Cololabis_saira >= 0.01 | Cololabis_adocetus >= 0.01 | Scomberesox_saurus >= 0.01)) %>% 
    dplyr::select(cellID) %>% 
    pull()
  
  ggsquish2 <- plotSquishedModel(gg[[1]] %>% 
                                   dplyr::filter(cellID %in% restricted_cellID)) # Plot the squished model
  
  #### July-September
  gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                          test_tmp, # testing object with model column (predictions)
                          "jul-sept", # season
                          SAU_predict_season3 %>% 
                            dplyr::rowwise() %>% 
                            dplyr::mutate(adult = mean(x = c(Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus), na.rm = TRUE)) %>% 
                            ungroup(), # rest of the ocean cells
                          SAU_model4, # BRT model
                          `grid_SAU_jul-sept` # grid of species for specific season
  )
  
  restricted_cellID <- SAU_ds3 %>% # Restricting the model to just the adult's range
    dplyr::filter((!is.na(Cololabis_saira) | !is.na(Cololabis_adocetus) | !is.na(Scomberesox_saurus)) & (Cololabis_saira >= 0.01 | Cololabis_adocetus >= 0.01 | Scomberesox_saurus >= 0.01)) %>% 
    dplyr::select(cellID) %>% 
    pull()
  
  ggsquish3 <- plotSquishedModel(gg[[1]] %>% 
                                   dplyr::filter(cellID %in% restricted_cellID)) # Plot the squished model
  
  #### October-December
  gg <- plotSeasonPredict(train_tmp, # training object with model column (fitted values)
                          test_tmp, # testing object with model column (predictions)
                          "oct-dec", # season
                          SAU_predict_season4 %>% 
                            dplyr::rowwise() %>% 
                            dplyr::mutate(adult = mean(x = c(Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus), na.rm = TRUE)) %>% 
                            ungroup(), # rest of the ocean cells
                          SAU_model4, # BRT model
                          `grid_SAU_oct-dec`# grid of species for specific season
  )
  
  restricted_cellID <- SAU_ds4 %>% # Restricting the model to just the adult's range
    dplyr::filter((!is.na(Cololabis_saira) | !is.na(Cololabis_adocetus) | !is.na(Scomberesox_saurus)) & (Cololabis_saira >= 0.01 | Cololabis_adocetus >= 0.01 | Scomberesox_saurus >= 0.01)) %>% 
    dplyr::select(cellID) %>% 
    pull()
  
  ggsquish4 <- plotSquishedModel(gg[[1]] %>% 
                                   dplyr::filter(cellID %in% restricted_cellID)) # Plot the squished model
  
  ggsquished <- (ggsquish1 + ggsquish2) / (ggsquish3 + ggsquish4) +
    plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
    theme(plot.tag = element_text(size = 25))
  
  ggsave(plot = ggsquished, filename = "Figures/SAU/SAU_model4_squished.png", width = 27, height = 15, dpi = 600)