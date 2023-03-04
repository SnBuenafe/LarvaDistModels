# DESCRIPTION: Preparing aquamaps adult distribution layer

# From spatialplanr
SpatPlan_Crop_AQM <- function(df, spp, extent){
  
  cropped <- AquaMaps_sf %>%
    sf::st_crop(ex_sf, crop = TRUE) %>%  # TODO replace ex_sf with a polygon to deal with EEZ or coastal areas
    stars:::slice.stars(along = "band", index = aqm$SpeciesIDNum) %>% # indexes rows based on SpeciesIDNum
    stars::st_as_stars() %>% # loads it into memory
    stars::st_set_dimensions("band", values = aqm$longnames) %>%
    sf::st_as_sf(na.rm = FALSE, as_points = FALSE, merge = FALSE)
  
  rs <- cropped %>%
    sf::st_drop_geometry() %>%
    is.na() %>%
    rowSums()
  
  nc <- ncol(cropped) - 1 # Number of cols not including geometry
  
  cropped <- cropped %>%
    dplyr::filter({rs == nc} == FALSE) # Remove Rows with all NAs (except geometry)
  
  # Removed this code so I could get rid of . for RMD checks
  # cropped <- df %>%
  #   sf::st_crop(extent, crop = TRUE) %>%  # TODO replace ex_sf with a polygon to deal with EEZ or coastal areas
  #   stars:::slice.stars(along = "band", index = spp$SpeciesIDNum) %>% # indexes rows based on SpeciesIDNum
  #   stars::st_as_stars() %>% # loads it into memory
  #   stars::st_set_dimensions("band", values = spp$longnames) %>%
  #   sf::st_as_sf(na.rm = FALSE, as_points = FALSE, merge = FALSE) %>%
  #   dplyr::filter(sf::st_drop_geometry(.) %>%
  #                   is.na(.) %>%
  #                   {rowSums(.) == ncol(.)} == FALSE) # Remove Rows with all NAs (except geometry)
  
  return(cropped)
}

Direc <- "/Volumes/SeagateHub/01_Spatial Datasets/"

aqm <- readRDS(file.path(Direc, "AquaMaps", "AquaMaps_SpeciesInfoFB.rds")) %>%  # Load AquaMaps data
  dplyr::filter(longnames %in% c("Thunnus_albacares", # yellowfin tuna
                                 "Katsuwonus_pelamis", # skipjack tuna
                                 "Thunnus_alalunga", # albacore
                                 "Thunnus_obesus", # bigeye tuna (Pacific)
                                 "Thunnus_atlanticus", # bigeye tuna (Atlantic)
                                 "Auxis_rochei", # frigate tuna
                                 "Auxis_thazard", # frigate tuna
                                 "Xiphias_gladius", # swordfish
                                 "Makaira_nigricans", # Atlantic blue marlin
                                 "Tetrapturus_angustirostris", # Shortbill spearfish
                                 "Kajikia_audax", # Striped marlin
                                 "Kajikia_albida", # White marlin
                                 "Istiophorus_platypterus", # sailfish
                                 "Cololabis_saira", # sauries
                                 "Cololabis_adocetus", # sauries
                                 "Scomberesox_saurus", # Sauries
                                 "Scombrolabrax_heterolepis" # Longfin escolar
                                 ))


# stars code to subset by data by our species list and crop area to the region of PlanUnits
AquaMaps_sf <- stars::read_stars(file.path(Direc, "AquaMaps","AquaMaps.tif"), proxy = TRUE) # Load


PUextent <- grid %>% # Get the extent for AquaMaps from the Bndry extent
  sf::st_transform(crs = lonlat) %>% # Must be long/lat for AquaMaps
  sf::st_bbox() # get sf extent

ex_sf <- PUextent + c(-1, -1, 1, 1) # Pad the limits by 1 degree

AquaMaps_sf <- SpatPlan_Crop_AQM(AquaMaps_sf, aqm, ex_sf) %>%
  sf::st_transform(sf::st_crs(grid)) %>%  # Transform to PU units
  sf::st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  dplyr::as_tibble() %>% 
  dplyr::left_join(grid, ., by = "geometry") %>% # left_join with the grid
  sf::st_as_sf(crs = moll) %>% 
  dplyr::select(-ocean)

saveRDS(AquaMaps_sf, "Data/AquaMaps_sf.rds") # save file
