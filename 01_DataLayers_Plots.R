source("00_DataLayers_Utils.R")

# ----- Temperature -----
dataTmp <- nc2sf("ACCESS-ESM1-5", "historical", "tos")

ggTemp <- ggplot() +
  geom_sf(data = dataTmp, aes(fill = mean), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "deep",
                  #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = -1,
                     na.value = "grey64") +
  geom_sf(data = landmass, fill = "grey70", color = "grey64") +
  labs(fill = expression('Mean temperature (1952-1981) ('^"o"*'C)')) +
  theme_bw()

# ----- Oxygen -----
dataO2 <- nc2sf("ACCESS-ESM1-5", "historical", "o2os")

ggO2 <- ggplot() +
  geom_sf(data = dataO2, aes(fill = mean), color = NA, size = 0.01) +
  scale_fill_cmocean(name = "tempo",
                     #   alpha = 1,
                     aesthetics = c("fill"),
                     direction = 1,
                     na.value = "grey64") +
  geom_sf(data = landmass, fill = "grey70", color = "grey64") +
  labs(fill = expression('Mean O'[2]*' levels (1952-1981) (mol m'^"-3"*')')) +
  theme_bw()

# ----- Distance to coast -----
# Data from NASA's OceanColor Web supported by Ocean Biology Processing Group (OBPG) at NASA's Goddard Space Flight Center (https://oceancolor.gsfc.nasa.gov/docs/distfromcoast/)
tmp <- read.table("Data/final/dist2coast.txt.bz2", header = FALSE, sep = "\t") 

coast <- tmp %>%  # Distance is in km
  dplyr::rename(longitude = V1, latitude = V2) %>% 
  st_as_sf(., coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

X <- st_coordinates(coast)[,1]
Y <- st_coordinates(coast)[,2]

coast %<>% bind_cols(., X, Y) %>% 
  rename(longitude = ...3, latitude = ...4) %>% 
  relocate(geometry, .after = latitude)

# Make grid around the limits of the area
df_poly <- coast %>% 
  st_make_grid(cellsize = c(1,1), offset = st_bbox(coast)[c("xmin", "ymin")] - 0.5) %>% 
  st_as_sf()

# "Intersect" grids and points (from csv); TRUE if points are contained within the grid cells.
idx <- st_contains(df_poly, df_sf, sparse = FALSE) %>%
  rowSums() %>% 
  as.logical()

int <- st_interpolate_aw(coast, dataTmp, extensive = FALSE)
