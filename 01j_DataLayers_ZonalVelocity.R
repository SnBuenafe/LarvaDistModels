# DESCRIPTION: Creating seasonal zonal velocity layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
uo_rs <- stars::read_ncdf("Data/Climatology/ensemble/uo_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(uo_rs) <- paste0("X", seq(1956, 1981, by = 1))
uo <- rs2sf(uo_rs) %>% 
  dplyr::rename(uo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "uo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, uo_transformed, geometry)
saveRDS(uo, "Data/Climatology/sf/uo_historical_jan-mar_interpolated.rds")

# ii. April-June
uo_rs <- stars::read_ncdf("Data/Climatology/ensemble/uo_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(uo_rs) <- paste0("X", seq(1956, 1981, by = 1))
uo <- rs2sf(uo_rs) %>% 
  dplyr::rename(uo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "uo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, uo_transformed, geometry)
saveRDS(uo, "Data/Climatology/sf/uo_historical_apr-jun_interpolated.rds")

# iii. July-September
uo_rs <- stars::read_ncdf("Data/Climatology/ensemble/uo_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(uo_rs) <- paste0("X", seq(1956, 1981, by = 1))
uo <- rs2sf(uo_rs) %>% 
  dplyr::rename(uo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "uo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, uo_transformed, geometry)
saveRDS(uo, "Data/Climatology/sf/uo_historical_jul-sept_interpolated.rds")

# iv. October-December
uo_rs <- stars::read_ncdf("Data/Climatology/ensemble/uo_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(uo_rs) <- paste0("X", seq(1956, 1981, by = 1))
uo <- rs2sf(uo_rs) %>% 
  dplyr::rename(uo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "uo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, uo_transformed, geometry)
saveRDS(uo, "Data/Climatology/sf/uo_historical_oct-dec_interpolated.rds")
