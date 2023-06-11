fRestrictArea <- function(df, # combined data frame with cellID and geometry
                          bounds # can only be from 40N-40S (-40 to 40) and from -140W-110E (-140 to 110)
) {
  
  sf <- df %>% 
    sf::st_as_sf(crs = cCRS) # convert into an sf object
  
  boundary <- SpatPlan_Get_Boundary(Limits = c(xmin = bounds[["xmin"]],
                                               xmax = bounds[["xmax"]],
                                               ymin = bounds[["ymin"]],
                                               ymax = bounds[["ymax"]]),
                                    cCRS = cCRS)
  
  # First get all the PUs partially/wholly within the planning region
  logi_Reg <- sf::st_centroid(sf) %>%
    sf::st_intersects(boundary) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  
  sf <- sf[logi_Reg, ] # Get TRUE
  
  return(sf)
}