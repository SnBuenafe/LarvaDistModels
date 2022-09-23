# Get 

WDPA <- sf::st_read("Data/WDPA/WDPA_Sep2022_Public_shp-polygons.shp") %>% 
  dplyr::filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>% 
  dplyr::filter(MARINE > 0) %>% # remove terrestrial reserves
  dplyr::filter(STATUS %in% c("Designated", "Established", "Inscribed")) %>%  # from spatialplanr
  sf::st_transform(crs = lonlat) %>% 
  sf::st_transform(crs = moll)

Bndry <- spatialplanr::SpatPlan_Get_Boundary(Limits = "Global",
Type = NA)

grid <- spatialplanr::SpatPlan_Get_PlanningUnits(Bndry,
                                                 landmass,
                                                 CellArea = 1000,# Default was 1000
                                                 Shape = "square",
                                                 inverse = FALSE)

# get the depth per grid cell I Guess?

grid_tibble <- grid %>% 
  tibble::as_tibble()
grid_filled <- list()

interpolateDDepths <- function() {
  
  path <- "Data/GEBCO"
  list <- list.files(path)
  x <- apply(outer(list, ".tif", stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(x == 1)
  
  for(i in 4:length(file)) {
    
    gebco <- terra::rast(file.path(path, list[file[i]])) %>% 
      terra::aggregate(., fact = 10)
    
    # convert to sf object
    gebco_reclassified_sf <- gebco %>% 
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = FALSE) %>% 
      sf::st_as_sf(crs = lonlat) %>% 
      sf::st_transform(crs = moll)
    
    colnames(gebco_reclassified_sf)[1] <- "depth" # change column name of the first column
    
    time <- system.time(grid_filled[[i]] <- gebco_reclassified_sf %>% 
      st_interpolate_aw(grid, extensive = FALSE) %>% 
      tibble::as_tibble() %>% 
      left_join(grid_tibble, ., by = "geometry"))
    
    print(time)
    
    print("Interpolated")
  }
}

# rename columns
for(i in 1:8) {
  name = paste0("depth", i)
  grid_filled[[i]] %<>% dplyr::rename(!!sym(name) := depth)
}

# join all
joined <- purrr::reduce(grid_filled, left_join, by = c("cellID", "geometry")) 

joined %<>% 
  dplyr::mutate(meanDepth = rowMeans(joined[, 3:10], na.rm = TRUE))

joined_sf <- joined %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  dplyr::select(cellID, meanDepth, geometry)

saveRDS(joined_sf, "Data/GEBCO/gebco1000.rds")


# intersect WDPA with grid
WDPA_int <- sf::st_intersection(grid, WDPA)
# then I'll take the area of the protected area
WDPA_int %<>% dplyr::select(WDPAID, cellID)

saveRDS(WDPA_int, "Data/wdpa_int.rds")

WDPA_int$Area <- sf::st_area(WDPA_int) %>% 
  units::set_units(km^2)
