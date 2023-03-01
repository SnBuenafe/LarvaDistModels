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

AquaMaps_sf <- AquaMaps_sf %>%
  spatialplanr::SpatPlan_Crop_AQM(aqm, ex_sf) %>%
  sf::st_transform(sf::st_crs(grid)) %>%  # Transform to PU units
  sf::st_interpolate_aw(grid, extensive = FALSE) %>%
  dplyr::mutate(cellID = grid$cellID) %>% # We lose cellID in interpolate_aw
  dplyr::relocate(.data$cellID, tidyselect::everything()) # Move to the start
