# Local spatial climatic gradients
#
# Function to calculate the magnitude and direction of the spatial gradient
# associated to a climatic variable after Burrows et al. (2011). This trend is
# to be used for the calculation of the gradient-based climate velocity using gVoCC.

# https://github.com/DavidSchoeman/terra_VoCC/blob/main/spatGrad.R
# Author: David Schoeman

spatGrad <- function(r, th = -Inf, projected = FALSE){
  if(terra::nlyr(r) > 1){r <- terra::mean(r, na.rm=TRUE)}
  # get resolution of the raster
  re <- terra::res(r)
  # Create a columns for focal and each of its 8 adjacent cells
  y <- data.table(terra::adjacent(r, 1:ncell(r), directions = 8, pairs = TRUE))
  y <- na.omit(y[, climFocal := values(r)[from]][order(from, to)])   # Get value for focal cell, order the table by raster sequence and omit NAs (land cells)
  y[, clim := values(r)[to]] # Insert values for adjacent cells
  y[, sy := rowFromCell(r, from)-rowFromCell(r, to)]  # Column to identify rows in the raster (N = 1, mid = 0, S = -1)
  y[, sx := colFromCell(r, to)-colFromCell(r, from)]  # Same for columns (E = 1, mid = 0, W = -1)
  y[sx > 1, sx := -1]   # Sort out the W-E wrap at the dateline, part I
  y[sx < -1, sx := 1]   # Sort out the W-E wrap at the dateline, part II
  y[, code := paste0(sx, sy)]    # Make a unique code for each of the eight neighbouring cells
  # Code cells with positions
  y[.(code = c("10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]
  y <- dcast(y[,c("from","code","clim")], from ~ code, value.var = "clim")
  y[, climFocal := values(r)[from]]   # Put climFocal back in
  y[, LAT := yFromCell(r, from)]         # Add focal cell latitude
  
  # Calculate individual spatial temperature gradients: grads (degC per km)
  # WE gradients difference in temperatures for each western and eastern pairs divided by the distance between the cells in each pair (corrected for  latitudinal distortion if unprojected)
  # Positive values indicate an increase in clim from W to E (i.e., in line with the Cartesian x axis)
  
  ifelse(projected == TRUE, d <- 1, d <- 111.325)
  ifelse(projected == TRUE, co <- 0, co <- 1)
  
  y[, gradWE1 := (climN-climNW)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
  y[, gradWE2 := (climFocal-climW)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
  y[, gradWE3 := (climS-climSW)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]
  y[, gradWE4 := (climNE-climN)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
  y[, gradWE5 := (climE-climFocal)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
  y[, gradWE6 := (climSE-climS)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]
  
  # NS gradients difference in temperatures for each northern and southern pairs divided by the distance between them (111.325 km per degC *re[2] degC)
  # Positive values indicate an increase in sst from S to N (i.e., in line with the Cartesian y axis)
  y[, gradNS1 := (climNW-climW)/(d*re[2])]
  y[, gradNS2 := (climN-climFocal)/(d*re[2])]
  y[, gradNS3 := (climNE-climE)/(d*re[2])]
  y[, gradNS4 := (climW-climSW)/(d*re[2])]
  y[, gradNS5 := (climFocal-climS)/(d*re[2])]
  y[, gradNS6 := (climE-climSE)/(d*re[2])]
  
  # Calulate NS and WE gradients. NOTE: for angles to work (at least using simple positive and negative values on Cartesian axes), S-N & W-E gradients need to be positive)
  y[, WEgrad := apply(.SD, 1, function(x) stats::weighted.mean(x, c(1,2,1,1,2,1), na.rm = T)), .SDcols = 12:17]
  y[, NSgrad := apply(.SD, 1, function(x) stats::weighted.mean(x, c(1,2,1,1,2,1), na.rm = T)), .SDcols = 18:23]
  y[is.na(WEgrad) & !is.na(NSgrad), WEgrad := 0L]     # Where NSgrad does not exist, but WEgrad does, make NSgrad 0
  y[!is.na(WEgrad) & is.na(NSgrad), NSgrad := 0L]     # same the other way around
  
  # Calculate angles of gradients (degrees) - adjusted for quadrant (0 deg is North)
  y[, angle := angulo(.SD$WEgrad, .SD$NSgrad), .SDcols = c("WEgrad", "NSgrad")]
  
  # Calculate the vector sum of gradients (C/km)
  y[, Grad := sqrt(apply(cbind((y$WEgrad^2), (y$NSgrad^2)), 1, sum, na.rm = TRUE))]
  
  # Merge the reduced file back into the main file to undo the initial na.omit
  from <- data.table(1:ncell(r)) # Make ordered from cells
  y <- y[from]   # merge both
  
  rAng <- rGrad <- terra::rast(r)
  rAng[y$from] <- y$angle
  rGrad[y$from] <- y$Grad
  rGrad[rGrad[] < th] <- th
  output <- c(rGrad,rAng)
  names(output) <- c("Grad", "Ang")
  return(output)
  
}