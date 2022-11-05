# Load preliminaries
#source("00_Utils.R")

#################
## TEMPERATURE ##
#################

##### Historical (to fit the model) ####
# i. Full (annual)
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_historical_1956_1984_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(1956, 1984, by = 1))
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)
saveRDS(tos, "Data/Climatology/sf/tos_historical_interpolated.rds")

# ii. January-March (seasonal)
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_historical_1956_1984_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(1956, 1984, by = 1))  
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)  
saveRDS(tos, "Data/Climatology/sf/tos_historical_jan-mar_interpolated.rds")

# iii. April-June (seasonal)
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_historical_1956_1984_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(1956, 1984, by = 1))  
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)  
saveRDS(tos, "Data/Climatology/sf/tos_historical_apr-jun_interpolated.rds")  

# iv. July-September (seasonal)
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_historical_1956_1984_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(1956, 1984, by = 1))  
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)  
saveRDS(tos, "Data/Climatology/sf/tos_historical_jul-sept_interpolated.rds")

# v. October-December (seasonal)
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_historical_1956_1984_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(1956, 1984, by = 1))  
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)  
saveRDS(tos, "Data/Climatology/sf/tos_historical_oct-dec_interpolated.rds")

#### Present (2017-2026) ####
# i. Present (annual)
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_ssp585_2017_2026_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(2017, 2026, by = 1))
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)
saveRDS(tos, "Data/Climatology/sf/tos_present_interpolated.rds")

# ii. January-March (seasonal)
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_ssp585_2017_2026_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(2017, 2026, by = 1))
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)
saveRDS(tos, "Data/Climatology/sf/tos_present_jan-mar_interpolated.rds")

# iii. April-June (seasonal)
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_ssp585_2017_2026_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(2017, 2026, by = 1))
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)
saveRDS(tos, "Data/Climatology/sf/tos_present_apr-jun_interpolated.rds")

#### Mid-century (2046-2055) ####
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_ssp585_2046_2055_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(2046, 2055, by = 1))
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)
saveRDS(tos, "Data/Climatology/sf/tos_midCentury_interpolated.rds")

#### End of the century (2091-2100) ####
tos_rs <- stars::read_ncdf("Data/Climatology/ensemble/tos_ssp585_2091_2100_ensemble.nc") %>% 
  terra::rast()
names(tos_rs) <- paste0("X", seq(2091, 2100, by = 1))
tos <- rs2sf(tos_rs) %>% 
  dplyr::rename(tos = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "tos") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, tos_transformed, geometry)
saveRDS(tos, "Data/Climatology/sf/tos_endCentury_interpolated.rds")