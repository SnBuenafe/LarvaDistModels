# Load coastline
# Convert the coast line into points?
# Get the nearest feature (i.e., coastline point) for each of the grid cells
# Calculate the distance

library(rnaturalearth)
library(sf)
library(tidyverse)

lonlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"

coast <- rnaturalearth::ne_coastline(scale = 'large') %>% 
  sf::st_as_sf(crs = lonlat) %>% 
  sf::st_transform(crs = moll)

ggplot() + geom_sf(data = coast, color = "red", size = 1)

# Just try calculating the distance of the centroid of each grid cell to the linestring, then choose the minimum distance! see: https://gis.stackexchange.com/questions/243994/how-to-calculate-distance-from-point-to-linestring-in-r-using-sf-library-and-g

# Load in the grid
grid_centroid <- grid %>% 
  sf::st_centroid()

# Find the nearest coast for all the grid cells
nearest <- sf::st_nearest_feature(grid_centroid, coast)

dists <- c()
for(i in 1:nrow(grid_centroid)) {
  # get the distance and populate an empty vector with it
  dists[i] <- sf::st_distance(grid_centroid[i, ], coast[nearest[i], ])
  print(dists[i])
}

# then add that in the grid_centroid df
grid_centroid$coastDistance <- dists

saveRDS(grid_centroid, "Data/CoastDistance.rds") # save the coast distance df



# Seems to do the right thing! Now I just need to automate it and code that :)
ggplot() +
  geom_sf(data = grid_centroid[500,], color = "red") +
  geom_sf(data = grid_centroid[1000,], color = "blue") +
  geom_sf(data = coast[nearest[500],], color = "red") + 
  geom_sf(data = coast[nearest[1000],], color = "blue")

