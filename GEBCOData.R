

# see: https://www.eea.europa.eu/themes/biodiversity/protected-areas/facts-and-figures/IUCN-management-categories
WDPA <- sf::st_read("Data/WDPA/WDPA_Sep2022_Public_shp-polygons.shp") %>% 
  dplyr::filter(IUCN_CAT %in% c("Ia", "Ib", "II", "III", "IV")) %>% 
  dplyr::filter(MARINE > 0) %>% # remove terrestrial reserves
  dplyr::filter(STATUS %in% c("Designated", "Established", "Inscribed")) %>%  # from spatialplanr
  sf::st_transform(crs = lonlat)

#######################
# sf way

# Create global grid
#sf_use_s2(FALSE)
Bndry <- spatialplanr::SpatPlan_Get_Boundary(Limits = "Global",
                                             Type = NA)

grid <- spatialplanr::SpatPlan_Get_PlanningUnits(Bndry,
                                                 landmass,
                                                 CellArea = 1000,# Default was 1000
                                                 Shape = "square",
                                                 inverse = FALSE)

# intersect WDPA with the grid data
x <- sf::st_intersects(grid, WDPA) %>% 
  lengths > 0

grid[x, "PROTECTED"] <- 1 # assign areas that are PAs as 1s

classifyDepth <- function(grid, path, level) {
  list <- list.files(path)
  x <- apply(outer(list, ".tif", stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(x == 1)
  
  for(i in 1:length(file)) {
    bathy <- terra::rast(file.path(path, list[file[i]])) %>% 
      terra::as.contour(., nlevels = level) %>% 
      sf::st_as_sf(crs = lonlat) %>% 
      sf::st_transform(crs = moll)
    
    bathy_deep <- bathy %>% 
      dplyr::filter(level <= -200)
    
    bathy_epi <- bathy %>% 
      dplyr::filter(level <= 0 & level >-200)
    
    d <- sf::st_intersects(grid, bathy_deep) %>% 
      lengths > 0
    grid[d, "DEEP"] <- 1
    print("Sanity check: how much was added for >= 200m?")
    print(sum(grid$DEEP, na.rm= TRUE)/nrow(grid))
    
    e <- sf::st_intersects(grid, bathy_epi) %>% 
      lengths > 0
    grid[e, "EPIPELAGIC"] <- 1
    print("Sanity check: how much was added for <200m?")
    print(sum(grid$EPIPELAGIC, na.rm = TRUE)/nrow(grid))
  }
  
  return(grid)
}

grid100_100 <- classifyDepth(grid,
                             path = "Data/GEBCO",
                             level = 100)
saveRDS(grid100_100, "Data/GEBCO/grid100_100.rds")

# filtering out only those that have data from GEBCO <= 0m elevation
grid100Filtered <- grid100_100 %>% 
  dplyr::filter(!(is.na(DEEP) & is.na(EPIPELAGIC))) # removing rows that have DEEP and EPIPELAGIC as both NA

# How many are the ocean cells?
oceanCells = nrow(grid100Filtered)

# How much of the ocean is protected?
sum(grid100Filtered$PROTECTED, na.rm = TRUE)*100/oceanCells # 1.601889%

# How much of the ocean is >= 200m?
sum(grid100Filtered$DEEP, na.rm = TRUE)*100/oceanCells # 97.75455%

# How much of the ocean is < 200m?
sum(grid100Filtered$EPIPELAGIC, na.rm = TRUE)*100/oceanCells 

# How much of all the ocean is both protected and deep?
deep_protected <- grid100Filtered %>% 
  dplyr::filter(PROTECTED == 1 & DEEP == 1)
nrow(deep_protected)

# How much of all the ocean is both protected and shallow?
epi_protected <- grid100Filtered %>% 
  dplyr::filter(PROTECTED == 1 & EPIPELAGIC == 1)
nrow(epi_protected)

######################################### 
# Doing raster way...

# We want the following areas:
# Protected areas
# Areas >= 200m
# Areas < 200m
# Areas >= 200m & Protected
# Areas < 200m & Protected

# First we rasterize the protected-area database

classifyDepthRaster <- function(WDPA, # shapefile of protected area
                                res = 0.04166667, # res of GEBCO aggregated by a factor of 10; should be the same res
                                path # where the files are
) {
  
  WDPA_rs <- WDPA %>% 
    terra::vect()
  
  # Creating a empty raster
  template <- terra::rast(ncol = 360*(1/res), nrow = 180*(1/res))  # same res as GEBCO
  template[] <- 1:ncell(template)
  
  # Rasterize the object
  df_rs <- terra::rasterize(WDPA_rs, template, field = "IUCN_CAT")
  PROTECTED = expanse(df_rs, unit = "km", transform = TRUE)
  
  list <- list.files(path)
  x <- apply(outer(list, ".tif", stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(x == 1)
  
  # Call for the constants
  EPI = 0
  DEEP = 0
  EPI_PROTECTED = 0
  DEEP_PROTECTED = 0
  
  for(i in 1:length(file)){
    
    # see https://gis.stackexchange.com/questions/421821/how-to-subset-a-spatraster-by-value-in-r
    rs_epi <- terra::rast(file.path(path, list[file[i]])) %>% 
      terra::clamp(., upper = 0, lower = -200, values = FALSE) # making areas with positive elevations and elevations <-200m as NAs
    
    rs_deep <- terra::rast(file.path(path, list[file[i]])) %>% 
      terra::clamp(., upper = -200.1, values = FALSE) # making areas with elevations >-200m as NAs
    
    # aggregated rasters by a factor of 10
    epi_aggregated <- terra::aggregate(rs_epi, fact = 10)
    EPI = EPI + expanse(epi_aggregated, unit = "km", transform = TRUE)
    
    deep_aggregated <- terra::aggregate(rs_deep, fact = 10)
    DEEP = DEEP + expanse(deep_aggregated, unit = "km", transform = TRUE)
    
    # see: https://gis.stackexchange.com/questions/135971/intersection-of-two-raster-objects
    # get areas that are protected AND epipelagic
    epi_protected <- terra::crop(df_rs, epi_aggregated) %>% 
      terra::mask(epi_aggregated)
    EPI_PROTECTED = EPI_PROTECTED + expanse(epi_protected, unit = "km", transform = TRUE)
    
    # get areas that are protected AND deep
    deep_protected <- terra::crop(df_rs, deep_aggregated) %>% 
      terra::mask(deep_aggregated)
    DEEP_PROTECTED = DEEP_PROTECTED + expanse(deep_protected, unit = "km", transform = TRUE)
  }
  
  df <- tibble::tribble(
    ~category, ~value,
    "protected", PROTECTED,
    "epipelagic", EPI,
    "deep", DEEP,
    "epipelagic and protected", EPI_PROTECTED,
    "deep and protected", DEEP_PROTECTED
  )
   
  return(df)
}

trial1 <- classifyDepthRaster(WDPA = WDPA, path = "Data/GEBCO")
trial1

#############################
WDPA_moll <- WDPA %>% 
  sf::st_transform(crs = moll) %>% 
  dplyr::select(WDPAID)

calculateData <- function() {
  
  path <- "Data/GEBCO"
  list <- list.files(path)
  x <- apply(outer(list, ".tif", stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(x == 1)
  
  # define the classification matrix
  matrix <- matrix(c(-Inf, -200, 1, # classify <= -200m: 1s (deep)
                     -200, 0, 0, # classify >200m but <= 0m: 0s (epipelagic)
                     0, Inf, NA), ncol = 3, byrow = TRUE)
  
  # Calculate the following values
  OCEAN = 537425422 %>% 
    units::set_units(km^2) # area of ocean cells
  EPI = 6625942.08562332 %>% 
    units::set_units(km^2)# area of ocean cells that are considered epipelagic
  DEEP = 201248476.150123%>% 
    units::set_units(km^2)# area of ocean cells that are considered deep
  EPI_PROTECTED = 163040.391131636 %>% 
    units::set_units(km^2)# area of ocean cells that are considered epipelagic + protected
  DEEP_PROTECTED = 957770.882620152 %>% 
    units::set_units(km^2)# area of ocean cells that are ocnsidered deep + protected

  for(i in 5:length(file)) {
    
    gebco <- terra::rast(file.path(path, list[file[i]])) %>% 
      terra::aggregate(., fact = 10) # aggregate by a factor of 10 so we can convert it to sf
    
    gebco_reclassified <- gebco %>% 
      terra::classify(., matrix, right = TRUE) # reclassify based on the matrix defined above
    
    # convert to sf object
    gebco_reclassified_sf <- gebco_reclassified %>% 
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm = FALSE) %>% 
      sf::st_as_sf(crs = lonlat) %>% 
      sf::st_transform(crs = moll)
    
    colnames(gebco_reclassified_sf)[1] <- "depth" # change column name of the first column
    
    # take out those cells that wholly or partially touch the landmass
    x <- sf::st_intersects(gebco_reclassified_sf, landmass) %>% 
      lengths > 0
    
    gebco_reclassified_sf[x, "depth"] <- NA # change these values to NA
    
    # filter for the deep cells
    deep <- gebco_reclassified_sf %>% 
      dplyr::filter(depth == 1)
    
    # calculate the area that is considered deep for this file...
    DEEP = DEEP + (sum(sf::st_area(deep)) %>% 
                     units::set_units(km^2)) # Converts it to km^2... see: https://github.com/r-spatial/sf/issues/291
    print(paste0("DEEP: ", DEEP))
    
    # get the intersection of deep and WDPA data
    (system.time(deep_int <- sf::st_intersection(deep, WDPA_moll)))
    
    # calculate the area for the intersection
    DEEP_PROTECTED = DEEP_PROTECTED + (sum(sf::st_area(deep_int)) %>% 
                                         units::set_units(km^2)) # Convert to km^2
    print(paste0("DEEP_PROTECTED: ", DEEP_PROTECTED))
    
    # filter for the epipelagic cells
    epipelagic <- gebco_reclassified_sf %>% 
      dplyr::filter(depth == 0)
    
    # calculate the area that is considered deep for this file...
    EPI = EPI + (sum(sf::st_area(epipelagic)) %>% 
                   units::set_units(km^2)) # Converts it to km^2... see: https://github.com/r-spatial/sf/issues/291
    print(paste0("EPI: ", EPI))
    
    # get the intersection of deep and WDPA data
    epi_int <- sf::st_intersection(epipelagic, WDPA_moll)
    
    # calculate the area for the intersection
    EPI_PROTECTED = EPI_PROTECTED + (sum(sf::st_area(epi_int)) %>% 
                                       units::set_units(km^2)) # Convert to km^2
    
    print(paste0("EPI_PROTECTED: ", EPI_PROTECTED))
    
    OCEAN = OCEAN + EPI + DEEP # update the ocean area
    print(OCEAN) # sanity check
    
  }
  
  df <- tibble::tribble(~category, ~area,
                        "epipelagic", EPI,
                        "deep", DEEP,
                        "epipelagic and protected", EPI_PROTECTED,
                        "deep and protected", DEEP_PROTECTED,
                        "ocean", OCEAN)
  
  return(df)
  
}

df <- calculateData()

# at i = 6
# DEEP: 246678948.688884
# DEEP_PROTECTED: 2375432.65809186
# EPI: 9456709.53644897
# EPI_PROTECTED: 206875.555718883
# 793561080 OCEAN