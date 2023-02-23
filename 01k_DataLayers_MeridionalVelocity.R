# DESCRIPTION: Creating seasonal meridional velocity layers

# Load preliminaries
source("00_Utils.R")

# i. January-March
vo_rs <- stars::read_ncdf("Data/Climatology/ensemble/vo_historical_1956_1981_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(vo_rs) <- paste0("X", seq(1956, 1981, by = 1))
vo <- rs2sf(vo_rs) %>% 
  dplyr::rename(vo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "vo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, vo_transformed, geometry)
saveRDS(vo, "Data/Climatology/sf/vo_historical_jan-mar_interpolated.rds")

# ii. April-June
vo_rs <- stars::read_ncdf("Data/Climatology/ensemble/vo_historical_1956_1981_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(vo_rs) <- paste0("X", seq(1956, 1981, by = 1))
vo <- rs2sf(vo_rs) %>% 
  dplyr::rename(vo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "vo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, vo_transformed, geometry)
saveRDS(vo, "Data/Climatology/sf/vo_historical_apr-jun_interpolated.rds")

# iii. July-September
vo_rs <- stars::read_ncdf("Data/Climatology/ensemble/vo_historical_1956_1981_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(vo_rs) <- paste0("X", seq(1956, 1981, by = 1))
vo <- rs2sf(vo_rs) %>% 
  dplyr::rename(vo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "vo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, vo_transformed, geometry)
saveRDS(vo, "Data/Climatology/sf/vo_historical_jul-sept_interpolated.rds")

# iv. October-December
vo_rs <- stars::read_ncdf("Data/Climatology/ensemble/vo_historical_1956_1981_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(vo_rs) <- paste0("X", seq(1956, 1981, by = 1))
vo <- rs2sf(vo_rs) %>% 
  dplyr::rename(vo = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "vo") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, vo_transformed, geometry)
saveRDS(vo, "Data/Climatology/sf/vo_historical_oct-dec_interpolated.rds")
