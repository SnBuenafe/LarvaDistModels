# DESCRIPTION: Preparing Aquamaps adult distribution layer

# Note: This code can only be run if you have the raw AquaMaps files and your machine has enough computing power to run this code. The processed output of this script is what's used in downstream analyses and is available in the repository. We would generally suggest skipping this script.

# Load packages
# install.packages("pacman")
pacman::p_load(cmocean, here, stars)

# Define directories
input_dir <- file.path("/Volumes", "SeagateHub", "01_Spatial Datasets")
output_dir <- here("data_input")
figure_dir <- here("figures", "predictors")
preliminaries_dir <- here("analyses", "02_preliminaries")

# Load preliminaries
source(here(preliminaries_dir, "00_SetupGrid.R"))
source(here("functions", "change_gglayout.R"))

# Cropping aquamaps dataset (adopted from spatialplanr)
SpatPlan_Crop_AQM <- function(df, spp, extent){
  
  cropped <- AquaMaps_sf %>%
    st_crop(ex_sf, crop = TRUE) %>%  # TODO replace ex_sf with a polygon to deal with EEZ or coastal areas
    stars:::slice.stars(along = "band", index = aqm$SpeciesIDNum) %>% # indexes rows based on SpeciesIDNum
    st_as_stars() %>% # loads it into memory
    st_set_dimensions("band", values = aqm$longnames) %>%
    st_as_sf(na.rm = FALSE, as_points = FALSE, merge = FALSE)
  
  rs <- cropped %>%
    st_drop_geometry() %>%
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

# Function to prepare plot
create_plot <- function(df) {
  sf <- df %>% 
    dplyr::select(-cellID) %>% 
    mutate(across(tidyselect:::where(is.numeric) & !c(geometry), ~ case_when(. > 0 ~ 1, 
                                                                       . <= 0 ~ 0))) %>% 
    mutate(FeatureSum = rowSums(across(tidyselect:::where(is.numeric) & !c(geometry)), na.rm = TRUE))
  
  gg <- ggplot() +
    geom_sf(data = sf, aes(fill = FeatureSum), color = NA, size = 0.2) +
    scale_fill_cmocean(name = "deep",
                       #   alpha = 1,
                       aesthetics = c("fill"),
                       direction = 1,
                       limits = c(NA, NA),
                       oob = scales::squish,
                       na.value = "grey64",
                       guide = guide_colourbar(
                         title.vjust = 0.5,
                         barheight = grid::unit(0.035, "npc"),
                         barwidth = grid::unit(0.5, "npc"),
                         frame.colour = "black")) +
    geom_sf(data = landmass, fill = "black", color = "black") +
    labs(fill = expression('Adult richness ')) +
    change_gglayout()
}


# Preparing Aquamaps layer for adult distribution -------------------------

aqm <- readRDS(file.path(input_dir, "AquaMaps", "AquaMaps_SpeciesInfoFB.rds")) %>%  # Load AquaMaps data
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
                                 "Scombrolabrax_heterolepis", # Longfin escolar
                                 "Thunnus_thynnus", # Atlantic bluefin tuna
                                 "Thunnus_orientalis", # Pacific bluefin tuna
                                 "Euthynnus_affinis", # little tuna
                                 "Euthynnus_alletteratus", # little tuna
                                 "Euthynnus_lineatus", # little tuna
                                 "Thunnus_maccoyii", # Southern bluefin tuna
                                 "Allothunnus_fallai", # slender tuna
                                 "Sarda_australis", # S. Pacific ocean
                                 "Sarda_chiliensis", # E. Pacific ocean
                                 "Sarda_orientalis", # Indo-pacific
                                 "Sarda_sarda", # Atlantic ocean
                                 "Istiompax_indica" # Black marlin
                                 ))


# stars code to subset by data by our species list and crop area to the region of PlanUnits
AquaMaps_sf <- read_stars(file.path(input_dir, "AquaMaps","AquaMaps.tif"), proxy = TRUE) # Load

PUextent <- grid %>% # Get the extent for AquaMaps from the Bndry extent
  st_transform(crs = lonlat) %>% # Must be long/lat for AquaMaps
  st_bbox() # get sf extent

ex_sf <- PUextent + c(-1, -1, 1, 1) # Pad the limits by 1 degree

AquaMaps_sf <- SpatPlan_Crop_AQM(AquaMaps_sf, aqm, ex_sf) %>%
  fSpatPlan_Convert2PacificCentered(., cCRS = cCRS) %>%  # Transform to PU units
  st_interpolate_aw(grid, extensive = FALSE) %>% # interpolate with planning units
  as_tibble() %>% 
  left_join(grid, ., by = "geometry") %>% # left_join with the grid
  st_as_sf(crs = cCRS) %>% 
  dplyr::select(-ocean)
# saveRDS(AquaMaps_sf, here::here(output_dir, "AquaMaps_sf.rds")) # save file


# Plotting ----------------------------------------------------------------
AquaMaps_sf <- readRDS(here(output_dir, "AquaMaps_sf.rds"))

# Plot number of features
ggaq <- AquaMaps_sf %>%
   dplyr::select(-Istiompax_indica, -Thunnus_thynnus, 
                 -Euthynnus_affinis, -Euthynnus_alletteratus, -Euthynnus_lineatus,
                 -Sarda_australis, -Sarda_orientalis, -Sarda_chiliensis,
                 -Sarda_sarda) %>% # Remove species that will not be included
  create_plot()

ggsave(plot = ggaq, filename = here::here(figure_dir, "aquamaps.png"), width = 14, height = 5, dpi = 300)
