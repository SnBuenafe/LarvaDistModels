# DESCRIPTION: Reproject and prepare rasters of models

source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
source(file.path("functions","create_rast.R"))
          
#### Make grid ####
lonlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Establish the grid
Bndry <- SpatPlan_Get_Boundary(Limits = "Global",
                               cCRS = lonlat) 
grid <- sf::st_make_grid(Bndry,
                         square = TRUE,
                         cellsize = c(1,1),
                         what = "polygons") %>%
  sf::st_sf()

grid <- grid %>%
  dplyr::mutate(cellID = dplyr::row_number()) # Add a cell ID reference

#### Create raster for all species ####
spp_list <- c("yft", "skp", "alb", "swo", "blum", "fri", "bet", "bft", "sail", "sbft", "slt", # "sau",
              "shos", "strm", "lesc")

# January-March
seas1 <- rast(create_rast(spp_list, "jan-mar"))
writeRaster(seas1, filename = here::here(rast_dir, "ModelOutputs_jan-mar.tif"))

# April-June
seas2 <- rast(create_rast(spp_list, "apr-jun"))
writeRaster(seas2, filename = here::here(rast_dir, "ModelOutputs_apr-jun.tif"))

# July-September
seas3 <- rast(create_rast(spp_list, "jul-sept"))
writeRaster(seas3, filename = here::here(rast_dir, "ModelOutputs_jul-sept.tif"))

# October-December
seas4 <- rast(create_rast(spp_list, "oct-dec"))
writeRaster(seas4, filename = here::here(rast_dir, "ModelOutputs_oct-dec.tif"))
