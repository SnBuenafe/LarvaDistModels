# DESCRIPTION: Increasing confidence of seasonal species distribution maps

# Load preliminaries
source(file.path("analyses", "04_models", "03a_SKP_Data.R")) # Load SKP data
model <- readRDS(file.path(model_dir, paste(species, "model.rds", sep = "_"))) # load model

# Making sure train and test data sets have the fitted predictions
preds <- gbm::predict.gbm(model, test, n.trees = model$gbm.call$best.trees, type = "response")
train_tmp <- train %>%
  dplyr::mutate(model = model$fitted)
test_tmp <- test %>%
  dplyr::mutate(model = preds)
limits = c(0, 0.8)

#### January-March ####
# Associate seasonal grids with the 10x10 grid
full_grid <- associateGrids(`grid_SKP_jan-mar`, grid_100)

# Create seasonal map
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "jan-mar", # season
                            SKP_predict_season1, # rest of the ocean cells
                            model, # BRT model
                            `grid_SKP_jan-mar` # grid of species for specific season
)

# Filtering 10x10 grid cells that have at least 5% of its area as sampling points
gg_filt <- restrictThreshold(full_grid,
                             gg_obj,
                             5) %>% 
  restrictAQM("skp")

saveRDS(object = gg_filt, file = file.path(preds_dir, paste(species, "jan-mar.rds", sep = "_"))) # save predictions
# gg_filt <- readRDS(file.path(preds_dir, paste(species, "jan-mar.rds", sep = "_")))

# Plot model removing 10x10 areas with lower confidence
gg <- plotConfidence(gg_filt, full_grid, limits)
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "jan-mar", "highconf.png", sep = "_")), width = 14, height = 5, dpi = 600)
# write_rds(gg, file = file.path(str_remove(figure_dir, paste0("/",species)), "FigData", paste(species,  "1jan-mar", "highconf.rds", sep = "_")))

# Plot the longitude and latitude bar plots
bps <- plotLonLat(gg_filt, full_grid)
ggsave(plot = bps$longitude, filename = file.path(figure_dir, paste(species, "longitude", "jan-mar.png", sep = "_")), width = 7, height = 0.5, dpi = 600)
ggsave(plot = bps$latitude, filename = file.path(figure_dir, paste(species, "latitude", "jan-mar.png", sep = "_")), width = 5, height = 1, dpi = 600)

#### April-June ####
# Associate seasonal grids with the 10x10 grid
full_grid <- associateGrids(`grid_SKP_apr-jun`, grid_100)

# Create seasonal map
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "apr-jun", # season
                            SKP_predict_season2, # rest of the ocean cells
                            model, # BRT model
                            `grid_SKP_apr-jun` # grid of species for specific season
)

# Filtering 10x10 grid cells that have at least 5% of its area as sampling points
gg_filt <- restrictThreshold(full_grid,
                             gg_obj,
                             5) %>% 
  restrictAQM("skp")

saveRDS(object = gg_filt, file = file.path(preds_dir, paste(species, "apr-jun.rds", sep = "_"))) # save predictions
# gg_filt <- readRDS(file.path(preds_dir, paste(species, "jan-mar.rds", sep = "_")))

# Plot model removing 10x10 areas with lower confidence
gg <- plotConfidence(gg_filt, full_grid, limits)
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "apr-jun", "highconf.png", sep = "_")), width = 14, height = 5, dpi = 600) 
# write_rds(gg, file = file.path(str_remove(figure_dir, paste0("/",species)), "FigData", paste(species,  "2apr-jun", "highconf.rds", sep = "_")))

# Plot the longitude and latitude bar plots
bps <- plotLonLat(gg_filt, full_grid)
ggsave(plot = bps$longitude, filename = file.path(figure_dir, paste(species, "longitude", "apr-jun.png", sep = "_")), width = 7, height = 0.5, dpi = 600)
ggsave(plot = bps$latitude, filename = file.path(figure_dir, paste(species, "latitude", "apr-jun.png", sep = "_")), width = 5, height = 1, dpi = 600)

#### July-September ####
# Associate seasonal grids with the 10x10 grid
full_grid <- associateGrids(`grid_SKP_jul-sep`, grid_100)

# Create seasonal map
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                        test_tmp, # testing object with model column (predictions)
                        "jul-sep", # season
                        SKP_predict_season3, # rest of the ocean cells
                        model, # BRT model
                        `grid_SKP_jul-sep` # grid of species for specific season
)

# Filtering 10x10 grid cells that have at least 5% of its area as sampling points
gg_filt <- restrictThreshold(full_grid,
                             gg_obj,
                             5) %>% 
  restrictAQM("skp")

saveRDS(object = gg_filt, file = file.path(preds_dir, paste(species, "jul-sep.rds", sep = "_"))) # save predictions
# gg_filt <- readRDS(file.path(preds_dir, paste(species, "jul-sep.rds", sep = "_")))

# Plot model removing 10x10 areas with lower confidence
gg <- plotConfidence(gg_filt, full_grid, limits)
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "jul-sep", "highconf.png", sep = "_")), width = 14, height = 5, dpi = 600)
# write_rds(gg, file = file.path(str_remove(figure_dir, paste0("/",species)), "FigData", paste(species,  "3jul-sep", "highconf.rds", sep = "_")))

# Plot the longitude and latitude bar plots
bps <- plotLonLat(gg_filt, full_grid)
ggsave(plot = bps$longitude, filename = file.path(figure_dir, paste(species, "longitude", "jul-sep.png", sep = "_")), width = 7, height = 0.5, dpi = 600)
ggsave(plot = bps$latitude, filename = file.path(figure_dir, paste(species, "latitude", "jul-sep.png", sep = "_")), width = 5, height = 1, dpi = 600)

#### October-December ####
# Associate seasonal grids with the 10x10 grid
full_grid <- associateGrids(`grid_SKP_oct-dec`, grid_100)

# Create seasonal map
gg_obj <- create_speciesMap(train_tmp, # training object with model column (fitted values)
                            test_tmp, # testing object with model column (predictions)
                            "oct-dec", # season
                            SKP_predict_season4, # rest of the ocean cells
                            model, # BRT model
                            `grid_SKP_oct-dec` # grid of species for specific season
)

# Filtering 10x10 grid cells that have at least 10% of its area as sampling points
gg_filt <- restrictThreshold(full_grid,
                             gg_obj,
                             5) %>% 
  restrictAQM("skp")

saveRDS(object = gg_filt, file = file.path(preds_dir, paste(species, "oct-dec.rds", sep = "_"))) # save predictions
# gg_filt <- readRDS(file.path(preds_dir, paste(species, "oct-dec.rds", sep = "_")))

# Plot model removing 10x10 areas with lower confidence
gg <- plotConfidence(gg_filt, full_grid, limits)
ggsave(plot = gg, filename = file.path(figure_dir, paste(species, "oct-dec", "highconf.png", sep = "_")), width = 14, height = 5, dpi = 600)
# write_rds(gg, file = file.path(str_remove(figure_dir, paste0("/",species)), "FigData", paste(species,  "4oct-dec", "highconf.rds", sep = "_")))

# Plot the longitude and latitude bar plots
bps <- plotLonLat(gg_filt, full_grid)
ggsave(plot = bps$longitude, filename = file.path(figure_dir, paste(species, "longitude", "oct-dec.png", sep = "_")), width = 7, height = 0.5, dpi = 600)
ggsave(plot = bps$latitude, filename = file.path(figure_dir, paste(species, "latitude", "oct-dec.png", sep = "_")), width = 5, height = 1, dpi = 600)

