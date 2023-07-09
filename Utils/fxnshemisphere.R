# DESCRIPTION: Functions for creating seasonal bar plots

prepare_hemis_obj <- function(seas_list) {
  
  empt <- list() # empty vector
  seasons <- tolower(seas_list)
  
  for(i in 1:length(seasons)) {
    
    x <- terra::rast(here::here(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
      as.data.frame(xy = TRUE) %>% # get longitude and latitudes
      dplyr::as_tibble() %>% 
      dplyr::mutate(hemisphere = ifelse(y >= 0, "North", "South"))
    
    standardize <- function(x) {
      y <- sum(x) / n()
    }
    
    empt[[i]] <- x %>% 
      dplyr::group_by(hemisphere) %>% 
      dplyr::summarise(across(c(everything(), -x, -y), standardize)) %>% 
      dplyr::rename_with(~paste(., seasons[i], sep = "_"), !hemisphere)
    
  }
  
  full <- purrr::reduce(empt, dplyr::left_join)

  return(full)
  
}

plot_hemis_spp <- function(full_df, spp) {
  df <- full_df %>% 
    dplyr::select(hemisphere, starts_with(spp)) %>% 
    dplyr::mutate(across(starts_with(paste0(spp, "_")),
                         ~ ifelse(hemisphere == "South", -(.x), .x),
                         .names = "{col}"))
  
  fin <- df %>% 
    tidyr::pivot_longer(!hemisphere, names_to = "season", values_to = "sum") %>% 
    dplyr::mutate(season = case_when(str_detect(season, "jan-mar") ~ "Jan-Mar",
                                     str_detect(season, "apr-jun") ~ "Apr-Jun",
                                     str_detect(season, "jul-sept") ~ "Jul-Sept",
                                     str_detect(season, "oct-dec") ~ "Oct-Dec")) %>% 
    dplyr::mutate(season = fct_relevel(season, seas_list))
  
  fin2 <- df %>% 
    dplyr::summarise(across(!hemisphere, mean)) %>% 
    tidyr::pivot_longer(cols = everything(),
                        names_to = "season",
                        values_to = "mean") %>% 
    dplyr::mutate(season = case_when(str_detect(season, "jan-mar") ~ "Jan-Mar",
                                     str_detect(season, "apr-jun") ~ "Apr-Jun",
                                     str_detect(season, "jul-sept") ~ "Jul-Sept",
                                     str_detect(season, "oct-dec") ~ "Oct-Dec"))
  
  gg <- ggplot() + 
    geom_bar(data = fin, aes(x = season, y = sum, fill = hemisphere),
             width = 0.9,
             stat = "identity", position = "identity", show.legend = FALSE) +
    scale_fill_manual(aesthetics = "fill",
                      values = c("North" = "#0084C2", "South" = "#EAB47F")) +
    geom_line(data = fin2, aes(x = season, y = mean, group = 1), color = "black", linewidth = 3) +
    geom_point(data = fin2, aes(x = season, y = mean), size = 5) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
    scale_y_continuous(limits = c(-max(abs(fin$sum)), max(abs(fin$sum)))) +
    xlab("Seasons") +
    ylab("Sum of probabilities") +
    theme_bw() +
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text = element_text(size = 25, color = "black"),
          plot.margin = unit(c(1,0.5,1,0.5), "cm"),
          axis.title.x = element_blank()
          #plot.title = element_text(color = "black", size = 30))
          #axis.title.x = element_text(color = "black", size = 25))
    )
  
  return(gg)
}