# Load preliminaries
#source("00_Utils.R")

###################
## Mixed Layer ##
###################

# A. Historical (to fit the model)
# i. Full (annual)
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_historical_1956_1984_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(1956, 1984, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_historical_interpolated.rds")

# ii. January-March (seasonal)
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_historical_1956_1984_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(1956, 1984, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_historical_jan-mar_interpolated.rds")

# iii. April-June (seasonal)
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_historical_1956_1984_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(1956, 1984, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_historical_apr-jun_interpolated.rds")

# iv. July-September (seasonal)
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_historical_1956_1984_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(1956, 1984, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_historical_jul-sept_interpolated.rds")

# v. October-December (seasonal)
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_historical_1956_1984_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(1956, 1984, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_historical_oct-dec_interpolated.rds")

# B. Present (2017-2026)
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_ssp585_2017_2026_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(2017, 2026, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_present_interpolated.rds")

# C. Mid-century (2046-2055)
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_ssp585_2046_2055_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(2046, 2055, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_midCentury_interpolated.rds")

# D. End of the century (2091-2100)
mlotst_rs <- stars::read_ncdf("Data/Climatology/ensemble/mlotst_ssp585_2091_2100_ensemble.nc") %>% 
  terra::rast()
names(mlotst_rs) <- paste0("X", seq(2091, 2100, by = 1))
mlotst <- rs2sf(mlotst_rs) %>% 
  dplyr::rename(mlotst = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "mlotst") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, mlotst_transformed, geometry)
saveRDS(mlotst, "Data/Climatology/sf/mlotst_endCentury_interpolated.rds")