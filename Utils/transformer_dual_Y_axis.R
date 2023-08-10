# Transform data for dual-y axes
# Written by Dave S (david.schoeman@gmail.com)
# Transformer function based on: https://www.r-bloggers.com/2022/12/how-to-make-a-plot-with-two-different-y-axis-in-r-with-ggplot2-a-secret-ggplot2-hack/
# There were many errors that had to be fixed!
transformer_dual_Y_axis <- function (data,
                                     primary_column, secondary_column,
                                     include_y_zero = FALSE) {
  # PARAMETER SETUP 
  params_tbl <- data %>%
    summarise(
      max_primary = max (!! enquo (primary_column)),
      min_primary = min (!! enquo (primary_column)),
      max_secondary = max(!! enquo (secondary_column)),
      min_secondary = min(!! enquo (secondary_column))
    )
  
  if (include_y_zero) {
    params_tbl$min_primary <- 0
    params_tbl$min_secondary <- 0
  }
  
  params_tbl <- params_tbl  %>% 
    mutate(
      scale = (max_primary - min_primary) / (max_secondary - min_secondary), #b
      shift = min_primary - scale * min_secondary #a
    )
  
  # MAKE SCALER FUNCTIONS
  scale_func <- function (x) {
    params_tbl$shift + (x * params_tbl$scale)
  }
  inv_func <- function(x) {
    (x - params_tbl$shift) / params_tbl$scale
  }
  
  # RETURN
  ret <- list(
    scale_func = scale_func,
    inv_func = inv_func,
    params_tbl = params_tbl
  )
  
  return (ret)
}  
