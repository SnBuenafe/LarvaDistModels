# Internal. Angle associated to the spatial gradient

# https://github.com/DavidSchoeman/terra_VoCC/blob/main/spatGrad.R
# Author: David Schoeman

angulo <- function(dx, dy){
  d <- cbind(dx, dy)
  angline <- function(rw){
    angle <- ifelse(rw[2] < 0, 180 + CircStats::deg(atan(rw[1]/rw[2])),
                    ifelse(rw[1] < 0, 360 + CircStats::deg(atan(rw[1]/rw[2])), CircStats::deg(atan(rw[1]/rw[2]))))
    return(angle)
  }
  return(apply(d, 1, angline))
}