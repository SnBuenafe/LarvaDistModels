# Load preliminaries
#source("00_Utils.R")

###################
## Salinity ##
###################

# A. Historical (to fit the model)
# i. Full (annual)
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_historical_1956_1984_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(1956, 1984, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_historical_interpolated.rds")

# ii. January-March (seasonal)
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_historical_1956_1984_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(1956, 1984, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_historical_jan-mar_interpolated.rds")

# iii. April-June (seasonal)
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_historical_1956_1984_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(1956, 1984, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_historical_apr-jun_interpolated.rds")

# iv. July-September (seasonal)
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_historical_1956_1984_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(1956, 1984, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_historical_jul-sept_interpolated.rds")

# v. October-December (seasonal)
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_historical_1956_1984_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(1956, 1984, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_historical_oct-dec_interpolated.rds")

# B. Present (2017-2026)
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_ssp585_2017_2026_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(2017, 2026, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_present_interpolated.rds")

# C. Mid-century (2046-2055)
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_ssp585_2046_2055_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(2046, 2055, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_midCentury_interpolated.rds")

# D. End of the century (2091-2100)
sos_rs <- stars::read_ncdf("Data/Climatology/ensemble/sos_ssp585_2091_2100_ensemble.nc") %>% 
  terra::rast()
names(sos_rs) <- paste0("X", seq(2091, 2100, by = 1))
sos <- rs2sf(sos_rs) %>% 
  dplyr::rename(sos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "sos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, sos_transformed, geometry)
saveRDS(sos, "Data/Climatology/sf/sos_endCentury_interpolated.rds")