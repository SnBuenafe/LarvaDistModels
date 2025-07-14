# Associate 1x1 seasonal grid cells with 10x10 grid

associateGrids <- function(seas_grid, # seasonal grid
                           coarse_grid # coarser grid (e.g., 10x10 grid)
) {
  season_grid <- seas_grid %>% 
    sf::st_as_sf() # Convert to sf
  
  tmp <- sf::st_within(season_grid, grid_100) %>% 
    unlist() # Associate 1x1 season grids within 10x10 grid and get their ID
  
  full_grid <- seas_grid %>% 
    dplyr::mutate(grid_100_category = grid_100$cellID[tmp[cellID]]) # Assign an ID to each 1x1 grid representing the 10x10 grid cell they belong within
  
  return(full_grid)
}