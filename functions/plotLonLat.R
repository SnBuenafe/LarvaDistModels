# DESCRIPTION: Plot mean probabilities across longitude (10) and latitude (5) bins

plotLonLat <- function(filt_df, # filtered sf object
                       full_df # full df
                       ) {
  
  # Create barplots across longitude and latitude
  df <- dplyr::left_join(full_df,
                         filt_df %>% 
                           dplyr::as_tibble(),
                         by = c("cellID", "grid_100_category", "geometry")) %>% 
    dplyr::select(cellID, grid_100_category, model, longitude, latitude) %>% 
    dplyr::mutate(longitude = plyr::round_any(longitude, 10),
                  latitude = plyr::round_any(latitude, 5)) # get categories in 5-degree for longitude and latitude
  
  df_lon <- df %>% 
    dplyr::select(cellID, model, longitude) %>% 
    dplyr::mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude)) %>% # change back to W-E longitude
    dplyr::summarise(x = sum(model, na.rm = TRUE),
                     y = n(),
                     z = x/y,
                     .by = "longitude") %>% 
    dplyr::select(longitude, z)
  
  # Reorder rows
  x <- df_lon %>% 
    dplyr::filter(longitude >= 0)
  y <- df_lon %>% 
    dplyr::filter(longitude < 0) %>% 
    dplyr::arrange(longitude)
  full <- dplyr::bind_rows(x, y) %>% 
    dplyr::mutate(longitude = factor(longitude, levels = longitude)) # keep this order
  
  gg_lon <- ggplot(data = full) +
    geom_col(aes(x = as.factor(longitude), y = z), width = 1, color = "#4275B5", fill = "#4275B5") +
    scale_y_continuous(expand = c(0,0)) +
    geom_vline(xintercept = c(as.factor(180)), color = "black", linewidth = 0.5) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"),
          panel.border = element_blank()
    )
  
  df_lat <- df %>% 
    dplyr::select(cellID, model, latitude) %>% 
    dplyr::summarise(x = sum(model, na.rm = TRUE),
                     y = n(),
                     z = x/y,
                     .by = "latitude") %>% 
    dplyr::select(latitude, z) %>% 
    dplyr::arrange(desc(latitude)) %>%  # make sure top is positive
    dplyr::mutate(latitude = factor(latitude, levels = latitude)) # keep this order
  
  gg_lat <- ggplot(data = df_lat) +
    geom_col(aes(x = as.factor(latitude), y = z), width = 1, color = "#4275B5", fill = "#4275B5") +
    geom_vline(xintercept = c(as.factor(0)), color = "black", linewidth = 0.5) +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(0,0,0,0), "cm"),
          panel.border = element_blank()
    )
  
  gg_res <- list(longitude = gg_lon, latitude = gg_lat)
  
  return(gg_res)
}