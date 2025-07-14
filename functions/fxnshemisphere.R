# DESCRIPTION: Functions for creating seasonal bar plots

# Make latitude labels all positive
make_lat_lon_label <- function (x) {
  format(abs(x), scientific = FALSE)
}

prepare_hemis_obj <- function(seas_list) {
  
  empt <- list() # empty vector
  seasons <- tolower(seas_list)
  
  for(i in 1:length(seasons)) {
    
    x <- terra::rast(here::here(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
      as.data.frame(xy = TRUE) %>% # get longitude and latitudes
      dplyr::as_tibble() %>% 
      dplyr::mutate(hemisphere = ifelse(y >= 0, "North", "South"))
    
    standardize_mean <- function(x) {
      y <- sum(x, na.rm = TRUE) / n()
    }
    
    standardize_sd <- function(x) {
      y <- sd(x, na.rm = TRUE)
    }
    
    df_mean <- x %>% 
      dplyr::group_by(hemisphere) %>% 
      dplyr::reframe(across(c(everything(), -x, -y), standardize_mean)) %>% 
      dplyr::rename_with(~paste(., seasons[i], "mean", sep = "_"), !hemisphere)
    
    df_sd <- x %>% 
      dplyr::reframe(across(c(everything(), -x, -y, -hemisphere), standardize_sd)) %>% 
      dplyr::rename_with(~paste(., seasons[i], "sd", sep = "_"))
   
    empt[[i]] <- cross_join(df_mean, df_sd)
     
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
    dplyr::select(!ends_with("_sd")) %>% 
    tidyr::pivot_longer(ends_with("_mean"), names_to = "season", values_to = "sum") %>% 
    dplyr::mutate(season = case_when(str_detect(season, "jan-mar") ~ "Jan-Mar",
                                     str_detect(season, "apr-jun") ~ "Apr-Jun",
                                     str_detect(season, "jul-sep") ~ "Jul-Sep",
                                     str_detect(season, "oct-dec") ~ "Oct-Dec")) %>% 
    dplyr::mutate(season = fct_relevel(season, seas_list))
  
  fin2 <- df %>% 
    dplyr::summarise(across(ends_with("_mean"), mean)) %>% 
    tidyr::pivot_longer(cols = everything(),
                        names_to = "season",
                        values_to = "mean") %>% 
    dplyr::mutate(season = case_when(str_detect(season, "jan-mar") ~ "Jan-Mar",
                                     str_detect(season, "apr-jun") ~ "Apr-Jun",
                                     str_detect(season, "jul-sep") ~ "Jul-Sep",
                                     str_detect(season, "oct-dec") ~ "Oct-Dec"))
  
  fin3 <- df %>% 
    dplyr::select(hemisphere, ends_with("_sd")) %>% 
    tidyr::pivot_longer(cols = ends_with("_sd"),
                        names_to = "season",
                        values_to = "sd") %>% 
    dplyr::mutate(season = case_when(str_detect(season, "jan-mar") ~ "Jan-Mar",
                                     str_detect(season, "apr-jun") ~ "Apr-Jun",
                                     str_detect(season, "jul-sep") ~ "Jul-Sep",
                                     str_detect(season, "oct-dec") ~ "Oct-Dec"))
  
  fin3_north <- fin3 %>% 
    dplyr::filter(hemisphere == "North") %>% 
    dplyr::select(!hemisphere) %>% 
    dplyr::rename(sd_max = sd)
  
  fin3_south <- fin3 %>% 
    dplyr::filter(hemisphere == "South") %>% 
    dplyr::select(!hemisphere) %>% 
    dplyr::rename(sd_min = sd)
  
  fin3_comb <- full_join(fin3_north, fin3_south, by = "season") %>% 
    full_join(., fin2, by = "season") %>% 
    mutate(sd_min = ifelse(is.na(sd_min), yes = 0, no = sd_min),
           sd_max = ifelse(is.na(sd_max), yes = 0, no = sd_max)) %>% 
    mutate(sd_min = mean+sd_min,
           sd_max = mean+sd_max)
  
  limits <- full_join(fin, fin3, by = c("hemisphere", "season")) %>% 
    mutate(sum_limits = sum+sd) %>% 
    mutate(sum_limits = ifelse(is.na(sum_limits), yes = 0, no = sum_limits))
  
  gg <- ggplot() + 
    geom_bar(data = fin, aes(x = season, y = sum, fill = hemisphere),
             width = 0.9,
             stat = "identity", position = "identity", show.legend = FALSE) +
    scale_fill_manual(aesthetics = "fill",
                      values = c("North" = "#0084C2", "South" = "#EAB47F")) +
    geom_line(data = fin2, aes(x = season, y = mean, group = 1), color = "black", linewidth = 3) +
    geom_errorbar(data = fin3_comb, aes(x = season, y = mean, ymin = sd_min, ymax = sd_max), linewidth = 1, width = 0.2, color = "grey50") +
    geom_point(data = fin2, aes(x = season, y = mean), size = 5) +
    scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
    scale_y_continuous(limits = c(-max(abs(limits$sum_limits)), max(abs(limits$sum_limits))),
                       label = make_lat_lon_label) +
    xlab("Seasons") +
    ylab("Sum of probabilities") +
    theme_bw() +
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text.y = element_text(size = 30, color = "black"),
          axis.text.x = element_text(size = 30, angle = 45, hjust = 1, color = "black"),
          plot.margin = unit(c(1,0.5,1,0.5), "cm"),
          axis.title.x = element_blank()
          #plot.title = element_text(color = "black", size = 30))
          #axis.title.x = element_text(color = "black", size = 25))
    )
  
  return(gg)
}
