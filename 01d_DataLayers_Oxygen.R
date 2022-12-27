# Load preliminaries
#source("00_Utils.R")

#################
## Oxygen ##
#################

# A. Historical (to fit the model)
# i. Full (annual)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_historical_1956_1984_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(1956, 1984, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_historical_interpolated.rds")

# ii. January-March (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_historical_1956_1984_jan-mar_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(1956, 1984, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_historical_jan-mar_interpolated.rds")

# iii. April-June (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_historical_1956_1984_apr-jun_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(1956, 1984, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_historical_apr-jun_interpolated.rds")

# iv. July-September (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_historical_1956_1984_jul-sept_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(1956, 1984, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_historical_jul-sept_interpolated.rds")

# v. October-December (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_historical_1956_1984_oct-dec_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(1956, 1984, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_historical_oct-dec_interpolated.rds")

# B. Present (2017-2026)
# i. Full (annual)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2017_2026_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2017, 2026, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_present_interpolated.rds")  

# ii. January-March (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2017_2026_jan-mar_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2017, 2026, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_present_jan-mar_interpolated.rds")  

# iii. April-June (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2017_2026_apr-jun_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2017, 2026, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_present_apr-jun_interpolated.rds")  

# iv. July-September (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2017_2026_jul-sept_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2017, 2026, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_present_jul-sept_interpolated.rds")  

# v. October-December (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2017_2026_oct-dec_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2017, 2026, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_present_oct-dec_interpolated.rds")  

# C. Mid-century (2046-2055)
# i. Full (annual)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2046_2055_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2046, 2055, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_midCentury_interpolated.rds")
# ii. January-March (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2046_2055_jan-mar_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2046, 2055, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_midCentury_jan-mar_interpolated.rds")
# iii. April-June (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2046_2055_apr-jun_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2046, 2055, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_midCentury_apr-jun_interpolated.rds")
# iv. July-September (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2046_2055_jul-sept_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2046, 2055, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_midCentury_jul-sept_interpolated.rds")
# v. October-December (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2046_2055_oct-dec_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2046, 2055, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_midCentury_oct-dec_interpolated.rds")

# D. End of the century (2091-2100)
# i. Full (annual)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2091_2100_ensemble.nc") %>% 
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2091, 2100, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_endCentury_interpolated.rds")
# ii. January-March (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2091_2100_jan-mar_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2091, 2100, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_endCentury_jan-mar_interpolated.rds")
# iii. April-June (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2091_2100_apr-jun_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2091, 2100, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_endCentury_apr-jun_interpolated.rds")
# iv. July-September (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2091_2100_jul-sept_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2091, 2100, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_endCentury_jul-sept_interpolated.rds")
# v. October-December (seasonal)
o2os_rs <- stars::read_ncdf("Data/Climatology/ensemble/o2os_ssp585_2091_2100_oct-dec_ensemble.nc") %>%
  terra::rast()
names(o2os_rs) <- paste0("X", seq(2091, 2100, by = 1))
o2os <- rs2sf(o2os_rs) %>% 
  dplyr::rename(o2os = mean) %>% # using the mean of the models
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  replaceNN(., grid, "o2os") %>%
  dplyr::as_tibble() %>% 
  dplyr::select(cellID, o2os_transformed, geometry)
saveRDS(o2os, "Data/Climatology/sf/o2os_endCentury_oct-dec_interpolated.rds")