# Load preliminaries
#source("00_Utils.R")

###################
## Chlorophyll-a ##
###################

# A. Historical (to fit the model)
# i. Full (annual)
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_historical_1956_1984_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(1956, 1984, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_historical_interpolated.rds")

# ii. January-March (seasonal)
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_historical_1956_1984_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(1956, 1984, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_historical_jan-mar_interpolated.rds")

# iii. April-June (seasonal)
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_historical_1956_1984_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(1956, 1984, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_historical_apr-jun_interpolated.rds")

# iv. July-September (seasonal)
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_historical_1956_1984_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(1956, 1984, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_historical_jul-sept_interpolated.rds")

# v. October-December (seasonal)
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_historical_1956_1984_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(1956, 1984, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_historical_oct-dec_interpolated.rds")

# B. Present (2017-2026)
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_ssp585_2017_2026_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(2017, 2026, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_present_interpolated.rds")

# C. Mid-century (2046-2055)
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_ssp585_2046_2055_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(2046, 2055, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_midCentury_interpolated.rds")

# D. End of the century (2091-2100)
chlos_rs <- stars::read_ncdf("Data/Climatology/ensemble/chlos_ssp585_2091_2100_ensemble.nc") %>% 
  terra::rast()
names(chlos_rs) <- paste0("X", seq(2091, 2100, by = 1))
chlos <- rs2sf(chlos_rs) %>% 
  dplyr::rename(chlos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "chlos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, chlos_transformed, geometry)
saveRDS(chlos, "Data/Climatology/sf/chlos_endCentury_interpolated.rds")