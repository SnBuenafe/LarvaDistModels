suppressPackageStartupMessages({
  library(tidyverse)
  library(ncdf4)
  library(terra)
  library(sf)
  library(rnaturalearth)
  library(cmocean)
  library(patchwork)
  library(magrittr)
  library(raster)
  library(dismo) # for boosted regression trees
  library(RColorBrewer)
  library(patchwork)
  library(Hmisc)
  library(corrplot)
  library(VoCC)
  library(stars)
  library(spatialplanr)
  library(ggpattern)
})

source("fSpatPlan_Convert2PacificCentered.R")

lonlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
moll_pacific <- "+proj=moll +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

# Load worldwide landmass
landmass <- rnaturalearth::ne_countries(scale = "medium") %>% 
  sf::st_as_sf(crs = lonlat) %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = moll_pacific)

# Load worldwide ocean
oceans <- sf::read_sf("Data/ne_50m_geography_marine_polys/ne_50m_geography_marine_polys.shp") %>%
  dplyr::select(label) %>% 
  sf::st_as_sf(crs = lonlat) %>% 
  fSpatPlan_Convert2PacificCentered(., cCRS = moll_pacific)

# Establish the grid
# Using the Mollweide projection (moll)
Bndry <- spatialplanr::SpatPlan_Get_Boundary(Limits = c(xmin = -40, xmax = 40, ymax = 40, ymin = -40),
                                             cCRS = moll_pacific) 
  
grid <- spatialplanr::SpatPlan_Get_PlanningUnits(Bndry,
                                                 oceans,
                                                 CellArea = 2500, # let's do half a degree? (~ 50 km x 50 km)
                                                 Shape = "square",
                                                 inverse = TRUE)

tmp <- sf::st_nearest_feature(grid, oceans)
grid %<>%
  dplyr::mutate(ocean = oceans$label[tmp[cellID]]) %>% 
  dplyr::filter(ocean %in% c('Andaman Sea', 'Arabian Sea', 'Arafura Sea', 'Banda Sea', 'Bay of Bengal', 
                              'Bay of Plenty', 'Bering Sea', 'Bismarck Sea', 'Bo Hai', 'Bristol Bay',
                             'Celebes Sea', 'Ceram Sea', 'Coral Sea', 'East China Sea', 
                             'INDIAN OCEAN', 'Java Sea', 'Korea Strait', 'Laccadive Sea', 'Golfo de California',
                             'Great Australian Bight', 'Gulf of Alaska', 'Gulf of Carpentaria', 'Gulf of Kutch',
                             'Gulf of Mannar', 'Golfo de Panamá', 'Gulf of Thailand', 'Gulf of Tonkin',
                             'Makassar Strait', 'Molucca Sea', 'Mozambique Channel', 'NORTH PACIFIC OCEAN',
                             'Philippine Sea', 'Sea of Japan', 'Sea of Okhotsk', 'Shelikhova Gulf',
                             'Solomon Sea', 'South China Sea', 'Strait of Singapore',
                             'SOUTH PACIFIC OCEAN', 'Strait of Malacca',
                             'Sulu Sea', 'Taiwan Strait', 'Tasman Sea', 'Timor Sea', 'Yellow Sea')) %>% 
  dplyr::mutate(cellID = row_number())

#### Helper functions ####

# Convert rs to sf objects
rs2sf <- function(rs) {
  sf <- rs %>% 
    terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to sf polygon
    sf::st_as_sf()
  sf::st_crs(sf) <- lonlat # set the CRS
  
  sf %<>% fSpatPlan_Convert2PacificCentered(., cCRS = moll_pacific) # then transform

 # Removing degree grids that wholly or partially intersect with the landmass object.
  logi_Reg <- sf::st_centroid(sf) %>%
    sf::st_intersects(landmass) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary

  int <- sf[!logi_Reg, ]

  fin <- int %>%
    dplyr::mutate(mean = rowMeans(across(1:(ncol(int)-1)), na.rm = FALSE)) %>%
    dplyr::select(mean)

  return(fin)
}

# Convert spatial gradients to sf objects
spat2sf <- function(rs) {
  sf <- rs %>% 
    terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to sf polygon
    sf::st_as_sf()
  sf::st_crs(sf) <- lonlat # set the CRS
  
  sf %<>% fSpatPlan_Convert2PacificCentered(., cCRS = moll_pacific) # then transform
  
  # Removing degree grids that wholly or partially intersect with the landmass object.
  logi_Reg <- sf::st_centroid(sf) %>%
    sf::st_intersects(landmass) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  
  int <- sf[!logi_Reg, ]
  
  return(int)
}

# combine seasonal data.frames of species into one sf object
combineFish <- function(species) {
  
  path <- "Data/Fish"
  list <- list.files(path)
  param <- c(species, ".rds")
  x <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(x == 1)
  
  df <- list()
  for(i in 1:length(file)) {
    df[[i]] <- readRDS(file.path(path, list[file[i]])) %>% 
      tibble::as_tibble()
  }
  combined <- do.call(bind_rows, df) %>% 
    dplyr::mutate(across(where(is.character), ~factor(.)) #, # change all character columns in  to factors
                  #abundance = ifelse(abundance > 0, yes = 1, no = 0)
                  ) %>% # convert data into binary
    dplyr::select(species, abundance, everything())
  
  sf <- combined %>% # convert into sf
    sf::st_as_sf(sf_column_name = "geometry")
  
  return(sf)
  
}

# assemble the fish data wrt grid
assembleGrid <- function(grid, sf) {
  grid_sf <- sf::st_join(grid, sf, join = st_contains_properly, left = TRUE)# join with the grid data if the centroid is contained within the grid
  
  centroid <- grid_sf %>% 
    sf::st_centroid() %>% # Convert to points
    sf::st_transform(crs = lonlat) %>%  # Transform back to lonlat
    dplyr::distinct(cellID, .keep_all = TRUE) # Select only distinct cellIDs, we're only interested in the geographic locations and not the seasons
  
  centroid_coordinates <- sf::st_coordinates(centroid) # Get coordinates
  
  grid_sf %<>% dplyr::mutate(longitude = ifelse(!is.na(longitude), 
                                                yes = longitude, 
                                                no = centroid_coordinates[cellID, "X"]),
                             latitude = ifelse(!is.na(latitude), 
                                               yes = latitude, 
                                               no = centroid_coordinates[cellID, "Y"])) %>% 
    dplyr::as_tibble()
  
  return(grid_sf)
}

# Convert bathymetry data (.TIFF) to sf (based on the grid provided)
gebcoConvert <- function(grid, area) {
  
  path <- "Data/GEBCO"
  list <- list.files(path)
  x <- apply(outer(list, ".tif", stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(x == 1)
  
  grid_tibble <- grid %>% 
    tibble::as_tibble()
  grid_filled <- list()
  
  for(i in 1:length(file)) {
    
    gebco <- terra::rast(file.path(path, list[file[i]])) %>% 
      terra::aggregate(., fact = 10)
    
    # convert to sf object
    gebco_reclassified_sf <- gebco %>% 
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = FALSE) %>% 
      sf::st_as_sf(crs = lonlat) %>% 
      fSpatPlan_Convert2PacificCentered(., cCRS = moll_pacific)
    
    colnames(gebco_reclassified_sf)[1] <- "depth" # change column name of the first column
    
    time <- system.time(grid_filled[[i]] <- gebco_reclassified_sf %>% 
                          st_interpolate_aw(grid, extensive = FALSE) %>% 
                          tibble::as_tibble() %>% 
                          left_join(grid_tibble, ., by = "geometry"))
    
    print(time)
    
    print("Interpolated")
  }
    
    # rename columns
    for(i in 1:8) {
      name = paste0("depth", i)
      grid_filled[[i]] %<>% 
        dplyr::rename(!!sym(name) := depth) %>% 
        dplyr::select(-ocean)
    }
    
    joined <- purrr::reduce(grid_filled, left_join, by = c("cellID", "geometry")) # join all 8 files
    
    joined %<>% 
      dplyr::mutate(meanDepth = rowMeans(joined[, 3:10], na.rm = TRUE))
    
    joined_sf <- joined %>% # converting into sf
      sf::st_as_sf(sf_column_name = "geometry") %>% 
      dplyr::select(cellID, meanDepth, geometry)
    
    saveRDS(joined_sf, paste0(path, "/gebco", area, ".rds"))
    
    return(joined_sf)
  
}

# Replacing NAs with the nearest neighborhood value
replaceNN <- function(climate, grid, colname) {
  filtered <- climate %>% dplyr::filter(!is.na(!!sym(colname)))
  vector <- purrr::as_vector(sf::st_nearest_feature(grid, filtered))
  
  mutatedDF <- climate %>% 
    dplyr::mutate(!!sym(paste0(colname, "_transformed")) := ifelse(is.na(!!sym(colname)),
                                                            yes = (filtered[[ colname ]])[vector[cellID]],
                                                            no = !!sym(colname)))
}

# Calculate the distance to the nearest coastline
calculateDist2Coast <- function(grid) {
  # Load coast
  coast <- rnaturalearth::ne_coastline(scale = 'large') %>% 
    sf::st_as_sf(crs = lonlat) %>% 
    fSpatPlan_Convert2PacificCentered(., cCRS = moll_pacific)
  
  # Convert grid to points (centroids)
  grid_centroid <- grid %>% 
    sf::st_centroid()
  
  # Find the nearest coast for all the grid cells
  nearest <- sf::st_nearest_feature(grid_centroid, coast)
  
  dists <- c()
  for(i in 1:nrow(grid_centroid)) {
    # get the distance and populate an empty vector with it
    dists[i] <- sf::st_distance(grid_centroid[i, ], coast[nearest[i], ])
    print(dists[i])
  }
  
  # then add that in the grid_centroid df
  grid_centroid$coastDistance <- dists
  
  saveRDS(grid_centroid, "Data/CoastDistance.rds") # save the coast distance df
}

##########
# Plotting
##########

# Plot the model
plotModel <- function(sf) {
  ggmodel <- ggplot() + 
    geom_sf(data = sf, aes(fill = model), color = NA, size = 0.1) +
    scale_fill_cmocean(name = "ice",
                       aesthetics = c("fill"),
                       direction = -1, 
                       limits = c(0, 1),
                       na.value = NA,
                       guide = guide_colourbar(
                         title.hjust = -1,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    labs(fill = "Probability ") +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
  
  return(ggmodel)
}

# Plot the abundance
plotAbundance <- function(sf) {
  ggabundance <- ggplot() + 
    geom_sf(data = sf, aes(fill = as.factor(abundance), color = as.factor(abundance)), size = 0.1) +
    scale_fill_brewer(name = "Abundance",
                      aesthetics = c("fill", "color"),
                      palette = "RdPu") +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw()
  
  return(ggabundance)
}

# Plot the presence/absence
plotPA <- function(sf, positive, saveFile) {
  ggpresence <- ggplot() +
    geom_sf(data = sf, aes(fill = as.factor(abundance_presence), color = as.factor(abundance_presence)), size = 0.1) +
    scale_color_manual(name = "Presence/Absence",
                       aesthetics = c("color", "fill"),
                       values = c("#feebe2", "#7a0177")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    annotate("text", x = -15000000, y = -6000000, label = paste0(positive, "% of points\n are presences")) + 
    theme_bw()
  
  ggsave(plot = ggpresence, filename = saveFile, width = 15, height = 8, dpi = 300)
  
  return(ggpresence)
}

# Plot predictions
plotPredictions <- function(sf, saveFile) {
  ggpreds <- ggplot() + 
    geom_sf(data = sf, aes(fill = predictions, color = predictions), size = 0.1) +
    scale_fill_cmocean("Predictions",
                       name = "tempo", 
                       aesthetics = c("color", "fill"),
                       limits = c(0, 1)) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    theme_bw()
  ggsave(plot = ggpreds, filename = saveFile, width = 15, height = 8, dpi = 300)
  
  return(ggpreds)
}

# Plotting seasonal plots
plotSeasonPredict <- function(train_tmp, test_tmp, season_name, predict_df, model, grid_season) {
  
  results <- list()
  
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
  
  gg_abun <- grid_season %>%
    dplyr::mutate(abundance_presence = factor(case_when(abundance > 0 ~ 1,
                                                 abundance == 0 ~ 0), levels = c(1, 0))) %>% 
    sf::st_as_sf(sf_column_name = "geometry") %>% 
    sf::st_centroid() 
  
  results[[1]] <- gg
  results[[2]] <- gg_abun
  
  return(results)
}

# Joining predictors and response
joinPredictors <- function(grid, tos, o2os, phos, chlos, sos, mlotst, no3os, po4os, nh4os, tf, sf, mesoscale, bathy, dist2coast, species, season = TRUE) {
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
    dplyr::left_join(., mesoscale, by = "cellID") %>% 
    dplyr::left_join(., bathy, by = "cellID") %>% 
    dplyr::left_join(., dist2coast, by = "cellID") %>% 
    dplyr::left_join(., species, by = "cellID") %>% 
    dplyr::left_join(grid, ., by = "cellID") # Join with species data
  
  if(isTRUE(season)) {
    df %<>% dplyr::select(cellID, species, abundance, season, longitude, latitude, ocean, tos_transformed, o2os_transformed, phos_transformed, chlos_transformed, sos_transformed, mlotst_transformed, no3os_transformed, po4os_transformed, nh4os_transformed, thermal_front_transformed, salinity_front_transformed, eke, vel, meanDepth, coastDistance, Thunnus_albacares, Katsuwonus_pelamis, Thunnus_alalunga, Thunnus_obesus, Thunnus_atlanticus, Auxis_rochei, Auxis_thazard, Xiphias_gladius, Makaira_nigricans, Tetrapturus_angustirostris, Kajikia_audax, Kajikia_albida, Istiophorus_platypterus, Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus, Scombrolabrax_heterolepis, geometry) # arrange columns
  } else {
    df %<>% dplyr::select(cellID, species, abundance, longitude, latitude, ocean, tos_transformed, o2os_transformed, phos_transformed, chlos_transformed, sos_transformed, mlotst_transformed, no3os_transformed, po4os_transformed, nh4os_transformed, thermal_front_transformed, salinity_front_transformed, eke, vel, meanDepth, coastDistance, Thunnus_albacares, Katsuwonus_pelamis, Thunnus_alalunga, Thunnus_obesus, Thunnus_atlanticus, Auxis_rochei, Auxis_thazard, Xiphias_gladius, Makaira_nigricans, Tetrapturus_angustirostris, Kajikia_audax, Kajikia_albida, Istiophorus_platypterus, Cololabis_saira, Cololabis_adocetus, Scomberesox_saurus, Scombrolabrax_heterolepis, geometry) # arrange columns
  }

  
  return(df)
}

# ROC CODE FROM DISMO PACKAGE ### CITE PROPERLY
.roc <-function (obsdat, preddat) {
  # code adapted from Ferrier, Pearce and Watson's code, by J.Elith
  #
  # see:
  # Hanley, J.A. & McNeil, B.J. (1982) The meaning and use of the area
  # under a Receiver Operating Characteristic (ROC) curve.
  # Radiology, 143, 29-36
  #
  # Pearce, J. & Ferrier, S. (2000) Evaluating the predictive performance
  # of habitat models developed using logistic regression.
  # Ecological Modelling, 133, 225-245.
  # this is the non-parametric calculation for area under the ROC curve, 
  # using the fact that a MannWhitney U statistic is closely related to
  # the area
  #
  
  # in dismo, this is used in the gbm routines, but not elsewhere (see evaluate).
  
  if (length(obsdat) != length(preddat)) { 
    stop("obs and preds must be equal lengths")
  }
  n.x <- length(obsdat[obsdat == 0])
  n.y <- length(obsdat[obsdat == 1])
  xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
  rnk <- rank(xy)
  wilc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x * n.y)
  return(round(wilc, 4))
}

# Do a k-fold grid search
CVgridSearch <- function(test, train, tc, bf, lr, pred_in, resp_in) {
  
  gridTib <- tibble(tree_complexity = numeric(),
                    bag_fraction = numeric(),
                    learning_rate = numeric(),
                    train_AUC = numeric(),
                    valid_AUC = numeric(),
                    test_AUC = numeric(),
                    train_test_diff = numeric(),
                    pred_dev = numeric())
  
  x = 1
  for(t in 1:length(tc)) {
    for(b in 1:length(bf)) {
      for(l in 1:length(lr)) {
        model <- dismo::gbm.step(data = train,
                                 gbm.x = pred_in,
                                 gbm.y = resp_in,
                                 tree.complexity = tc[t],
                                 learning.rate = lr[l],
                                 bag.fraction = bf[b],
                                 family = "bernoulli",
                                 n.folds = 5, # Use a 5-fold cross-validation
                                 n.trees = 10
                                 )
        
        # Populate the grid tibble
        gridTib[x, "tree_complexity"] = tc[t]
        gridTib[x, "bag_fraction"] = bf[b]
        gridTib[x, "learning_rate"] = lr[l]
        gridTib[x, "train_AUC"] = model$self.statistics$discrimination
        gridTib[x, "valid_AUC"] = model$cv.statistics$discrimination.mean
        
        # Predict
        pred <- gbm::predict.gbm(model, test, n.trees = model$gbm.call$best.trees, type = "response")
        roc <- .roc(test[,resp_in], pred)
        
        gridTib[x, "test_AUC"] = roc
        gridTib[x, "train_test_diff"] = model$self.statistics$discrimination - roc
        
        # Calculate predictive deviance
        gridTib[x, "pred_dev"] = dismo::calc.deviance(test[,resp_in], pred, family = "bernoulli")
        
        print(paste0("Run ", x)) # sanity check
        
        x = x + 1
        
      }
    }
  }
  
  return(gridTib)
}

# Plot test fitted values vs predictors
plotPredictors <- function(test_tmp) {
  longitude <- ggplot() +
    geom_point(data = test_tmp, aes(x = longitude, y = model)) +
    ylab("fitted values") +
    theme_bw()
  latitude <- ggplot() +
    geom_point(data = test_tmp, aes(x = latitude, y = model)) +
    ylab("fitted values") +
    theme_bw()
  season <- ggplot() +
    geom_boxplot(data = test_tmp, aes(x = season, y = model)) +
    ylab("fitted values") +
    theme_bw()
  tos <- ggplot() +
    geom_point(data = test_tmp, aes(x = tos_transformed, y = model)) +
    ylab("fitted values") +
    theme_bw()
  o2os <- ggplot() +
    geom_point(data = test_tmp, aes(x = o2os_transformed, y = model)) +
    ylab("fitted values") +
    theme_bw()
  phos <- ggplot() +
    geom_point(data = test_tmp, aes(x = phos_transformed, y = model)) +
    ylab("fitted values") +
    theme_bw()
  chlos <- ggplot() +
    geom_point(data = test_tmp, aes(x = chlos_transformed, y = model)) +
    ylab("fitted values") +
    theme_bw()
  sos <- ggplot() +
    geom_point(data = test_tmp, aes(x = sos_transformed, y = model)) +
    ylab("fitted values") +
    theme_bw()
  mlotst <- ggplot() +
    geom_point(data = test_tmp, aes(x = mlotst_transformed, y = model)) +
    ylab("fitted values") +
    theme_bw()
  no3os <- ggplot() +
    geom_point(data = test_tmp, aes(x = no3os_transformed, y = model)) +
    ylab("fitted values") +
    theme_bw()
  po4os <- ggplot() +
    geom_point(data = test_tmp, aes(x = po4os_transformed, y = model)) +
    ylab("fitted values") +
    theme_bw()
  nh4os <- ggplot() +
    geom_point(data = test_tmp, aes(x = nh4os_transformed, y = model)) +
    ylab("fitted values") +
    theme_bw()
  depth <- ggplot() +
    geom_point(data = test_tmp, aes(x = meanDepth, y = model)) +
    ylab("fitted values") +
    theme_bw()
  coast <- ggplot() +
    geom_point(data = test_tmp, aes(x = coastDistance, y = model)) +
    ylab("fitted values") +
    theme_bw()
  
  ggpredictors <- (longitude | latitude | season | tos) /
    (o2os | phos | chlos | sos) /
    (mlotst | no3os | po4os | nh4os) /
    (depth | coast | plot_spacer() | plot_spacer())
  
  return(ggpredictors)
}

# Plot squished model results
# Plot the model
plotSquishedModel <- function(sf, df 
                      # abundance
) {
  
  x <- sf %>%
    restrict_adult(df, .) %>%
    dplyr::filter(adult_cat == 0) %>%
    sf::st_union()
  
  # palette = brewer.pal(9, "YlGnBu")
  ggmodel <- ggplot() + 
    geom_sf(data = sf, aes(fill = model),
                               color = NA, size = 0.1) +
    scale_fill_cmocean("Probability ",
                       name = "ice",
                       direction = -1, 
                       limits = c(0, as.numeric(quantile(sf$model, 0.99))),
                       na.value = NA,
                       oob = scales::squish,
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.01, "npc"),
                         barwidth = grid::unit(0.25, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = NA, size = 0.01) +
    # geom_sf(data = abundance, aes(color = as.factor(abundance_presence)), size = 0.5) +
    # scale_fill_manual(name = "",
    #                   aesthetics = c("color"),
    #                   values = c("#67DBDB", "#143875"),
    #                   labels = c("Recorded presence", "Recorded absence", ""),
    #                   na.value = NA) +
    xlab("Longitude") +
    ylab("Latitude") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          panel.border = element_blank()) +
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim) # +
    geom_sf_pattern(data = x,
                    pattern = "stripe",
                    pattern_fill = "grey55",
                    pattern_color = "grey55",
                    fill = NA,
                    color = "grey55",
                    size = 0.5,
                    pattern_size = 0.5)

  
  return(ggmodel)
}

# Crop the predictor data to just 50N-50S
crop_predictor <- function(x) {
  # Get coordinates
  coord <- sf::st_centroid(x) %>% 
    sf::st_coordinates()
  
  x %<>%
    dplyr::mutate(longitude = coord[cellID, "X"],
                  latitude = coord[cellID, "Y"]) %>% 
    dplyr::filter((latitude < 5821011) & (latitude > -5821011))
}

# Adding text annotations to plot
# gg_add_text <- function(x, color = "black") {
 # list(#annotate("text", x=15000000, y=1600000, label= "Western\n Pacific Ocean", size = 5, color = color),
       #annotate("text", x=15000000, y=1234041, label= "Western\n Pacific Ocean", size = 5, color = color), (for all except habitat suitability maps)
       #annotate("text", x=-11483937, y=1234041, label= "Eastern\n Pacific Ocean", size = 5, color = color),
       #annotate("text", x=-3474813, y=1234041, label= "Atlantic Ocean", size = 4.5, color = color),
       #annotate("text", x=7942430, y=-1234041, label= "Indian Ocean", size = 5, color = color),
#        theme(legend.position = "bottom",
#              axis.title = element_blank(),
#              legend.text = element_text(size = 12),
#              legend.title = element_text(size = 18),
#              panel.border = element_blank()),
#        coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim))
# }

# Organizing data set for building model
organize_build <- function(x) {
  x %<>%
    dplyr::mutate(across(where(is.character), ~factor(.))) %>% # convert all characters to factors
    dplyr::mutate(abundance_presence = case_when(abundance > 0 ~ 1,
                                                 abundance == 0 ~ 0), 
                  row = row_number()) %>%  # mutate the abundance data into 1s and 0s
    dplyr::select(-geometry) %>% 
    dplyr::select(row, cellID, species, abundance, abundance_presence, ocean, longitude, latitude, season, everything()) %>% # arrange columns
    as.data.frame() #gbm.step doesn't work if it's a tibble...
}

# Organizing data set for predicting
organize_predict <- function(x) {
  x %<>%
    dplyr::filter(is.na(abundance)) %>% 
    organize_build()
}
