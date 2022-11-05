# Load preliminaries
#source("00_Utils.R")

# Bathymetry
bathy <- gebcoConvert(grid, 2500) # bathymetry data is extrapolated depending on the grid area provided

# Coastline
dist2coast <- calculateDist2Coast(grid) # distance to coast is calculated depending on the grid area provided