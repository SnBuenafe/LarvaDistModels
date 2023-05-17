# DESCRIPTION: Plots for Predictors

# Plot box plots
plotBP <- function(df, species, breaks, limits, expand, label) {
  
  x <- df %>% 
    dplyr::select(starts_with(paste0(species, "_"))) %>% 
    tidyr::pivot_longer(cols = everything(),
                        names_to = "season",
                        values_to = "predictor") %>% 
    dplyr::mutate(season = case_when(str_detect(season, pattern = "January-March") ~ "January-March",
                                     str_detect(season, pattern = "April-June") ~ "April-June",
                                     str_detect(season, pattern = "July-September") ~ "July-September",
                                     str_detect(season, pattern = "October-December") ~ "October-December"))
  
  ggplot(x, aes(x = season, y = predictor, fill = season)) +
    geom_boxplot(show.legend = FALSE) +
    scale_y_continuous(breaks = breaks,
                       limits = limits,
                       expand = expand) +
    scale_x_discrete(name = "Seasons",
                     labels = c("January-March", "April-June", "July-September", "October-December")) +
    scale_fill_brewer(palette="Blues") + 
    ggtitle(label) +
    theme_classic() +
    theme(axis.title.y = element_blank(),
          plot.title = element_text(color = "black", size = 25),
          axis.ticks = element_line(color = "black"),
          axis.text = element_text(color = "black", size = 18),
          axis.title.x = element_text(color = "black", size = 20)
    )
  
}

# Prepare the object for plotting the KD plot
prepareDF <- function(df, spp) {
  
  dum_list <- list()
  
  for(i in 1:length(spp)) {
    dum_list[[i]] <- df %>% 
      dplyr::select(., starts_with(spp[i])) %>% 
      tidyr::pivot_longer(cols = everything(),
                          names_to = c("species", "season"),
                          names_sep = "_",
                          values_to = "transformed")
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::bind_rows) # bind columns of list
  
  return(bind_df)
}

# Calculating and printing summary statistics
printSummary <- function(df) {
  stat_df <- df %>% 
    dplyr::group_by(species) %>% 
    dplyr::summarise(across("transformed", list(mean = ~mean(.x, na.rm = TRUE),
                                                quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                quant50 = ~quantile(.x, 0.5, na.rm = TRUE),
                                                quant75 = ~quantile(.x, 0.75, na.rm = TRUE)))) %>% 
    t()
  
  return(stat_df)
}

# Plot kernel density plot
plot_KD <- function(df,
                    spp,
                    xtitle,
                    limits, # title of x axis
                    col # values of colors
) {
  plot_df <- df %>%
    dplyr::mutate(species = fct_relevel(species, rev(spp))) # make sure they're in the desired order
  
  gg <- ggplot(plot_df, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 3, 
                        quantile_lines = TRUE,
                        quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_y_discrete(expand = expansion(add = c(0, 3.5))) +
    scale_x_continuous(#expand = c(0.01, 0.01),
      limits = limits) +
    scale_discrete_manual(aesthetics = "fill", values = col) +
    #geom_vline(xintercept = mean_val, color = col[1], linewidth = 2, linetype = "dashed") +
    xlab(xtitle) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text.x = element_text(color = "black", size = 18),
          axis.title.x = element_text(color = "black", size = 25),
          panel.border = element_rect(color = "black", linewidth = 3)
    )
}

#### SURFACE TEMPERATURE ####
plot_tos <- function(df, spp, col) {
  
  results <- list()
  # cutoff values for temperature are 20C - 30C
  # we change all values <20 to 20C and all values >30 to 30C
  
  plot_df %<>%
    dplyr::mutate(transformed = case_when(transformed < 20 ~ 20,
                                          transformed > 30 ~ 30,
                                          TRUE ~ transformed))
  
  # then we plot it

  
  # Define the output of this function
  results[[1]] <- stat_df
  results[[2]] <- gg
  
  return(results)
}

#### SURFACE OXYGEN ####
plot_o2os <- function(df, spp, col) {
  
  results <- list()
  dum_list <- list()
  
  # Get the sum per species
  for(i in 1:length(spp)) {
    dum_list[[i]] <- df %>% 
      dplyr::select(., starts_with(spp[i])) %>% 
      tidyr::pivot_longer(cols = everything(),
                          names_to = c("species", "season"),
                          names_sep = "_",
                          values_to = "transformed")
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::bind_rows) # bind columns of list
  
  stat_df <- bind_df %>% 
    dplyr::group_by(species) %>% 
    dplyr::summarise(across("transformed", list(mean = ~mean(.x, na.rm = TRUE),
                                                quant25 = ~quantile(.x, 0.25, na.rm = TRUE),
                                                quant50 = ~quantile(.x, 0.5, na.rm = TRUE),
                                                quant75 = ~quantile(.x, 0.75, na.rm = TRUE)))) %>% 
    t()
  
  plot_df <- bind_df %>%
    dplyr::mutate(species = fct_relevel(species, rev(spp))) # make sure they're in the desired order
  
  # mean_val <- mean(plot_df$transformed, na.rm = TRUE)
  
  # upper cutoff value for oxygen is 0.22
  
  plot_df %<>%
    dplyr::mutate(transformed = case_when(transformed > 0.22 ~ 0.22,
                                          TRUE ~ transformed))
  
  # then we plot it
  gg <- ggplot(plot_df, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 3, 
                        quantile_lines = TRUE,
                        quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_y_discrete(expand = expansion(add = c(0, 3.5))) +
    scale_x_continuous(expand = c(0.0005, 0.0005),
                       limits = c(NA, 0.22)) +
    scale_discrete_manual(aesthetics = "fill", values = col) +
    #geom_vline(xintercept = mean_val, color = col[1], linewidth = 2, linetype = "dashed") +
    xlab(expression('Oxygen concentration (mol m'^"-3"*')')) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text.x = element_text(color = "black", size = 18),
          axis.title.x = element_text(color = "black", size = 25),
          panel.border = element_rect(color = "black", linewidth = 3)
    )
  
  # Define the output of this function
  results[[1]] <- stat_df
  results[[2]] <- gg
  
  return(results)
}


# Plot broad-scale thermal gradients
plot_thermgrad <- function(df, spp, col) {
  
  results_list <- list()
  dum_list <- list()
  
  for(i in 1:length(spp)) {
    dum_list[[i]] <- df %>% 
      dplyr::mutate(!!sym(spp[i]) := case_when(!!sym(spp[i]) >= median(df[[spp[i]]]) ~ thermal_front_transformed, TRUE ~ NA)) %>% 
      dplyr::select(!!sym(spp[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::bind_cols) # bind columns of list
  
  stat_df <- bind_df %>% 
    dplyr::summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE),
                                               min = ~min(.x, na.rm = TRUE),
                                               max = ~max(.x, na.rm = TRUE)))) %>% 
    t()
  
  plot_df <- bind_df %>% 
    tidyr::pivot_longer(everything(), names_to = "species", values_to = "transformed") %>% # pivot the table
    dplyr::mutate(species = fct_relevel(species, rev(spp))) # make sure they're in the desired order
  
  # mean_val <- mean(plot_df$transformed, na.rm = TRUE)
  
  # cutoff values for broad-scale thermal gradients are 0 to 0.01
  
  plot_df %<>%
    dplyr::mutate(transformed = case_when(transformed > 0.01 ~ 0.01,
                                          TRUE ~ transformed))
  
  # then we plot it
  gg <- ggplot(plot_df, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 3, 
                        quantile_lines = TRUE,
                        quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_y_discrete(expand = expansion(add = c(0, 3.5))) +
    scale_x_continuous(expand = c(0.00001, 0.00001),
                       limits = c(-0.001, 0.011)) +
    scale_discrete_manual(aesthetics = "fill", values = col) +
    # geom_vline(xintercept = mean_val, color = col[1], linewidth = 2, linetype = "dashed") +
    xlab(expression('Broad-scale thermal gradients ('*Delta^"o"*'C km'^"-1"*')')) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text.x = element_text(color = "black", size = 18),
          axis.title.x = element_text(color = "black", size = 25),
          panel.border = element_rect(color = "black", linewidth = 3)
    )
  
  # Define the output of this function
  results_list[[1]] <- stat_df
  results_list[[2]] <- gg
  
  return(results_list)
  
}

# Plot EKE
plot_eke <- function(df, spp, col) {
  
  dum_list <- list()
  
  for(i in 1:length(spp)) {
    dum_list[[i]] <- df %>% 
      dplyr::mutate(!!sym(spp[i]) := case_when(!!sym(spp[i]) >= median(df[[spp[i]]]) ~ eke, TRUE ~ NA)) %>% 
      dplyr::select(!!sym(spp[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::bind_cols) %>% # bind columns of list
    tidyr::pivot_longer(everything(), names_to = "species", values_to = "transformed") %>% # pivot the table
    dplyr::mutate(species = fct_relevel(species, rev(spp))) # make sure they're in the desired order
  
  mean_val <- mean(bind_df$transformed, na.rm = TRUE)
  
  # cutoff values for eddy kinetic energy are 0 to 0.02
  
  bind_df %<>%
    dplyr::mutate(transformed = case_when(transformed > 0.02 ~ 0.02,
                                          TRUE ~ transformed))
  
  # then we plot it
  gg <- ggplot(bind_df, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 3, 
                        quantile_lines = TRUE,
                        quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_y_discrete(expand = expansion(add = c(0, 3.5))) +
    scale_x_continuous(expand = c(0.0005, 0.0005),
                       limits = c(-0.003, 0.023)) +
    scale_discrete_manual(aesthetics = "fill", values = col) +
    geom_vline(xintercept = mean_val, color = col[1], linewidth = 2, linetype = "dashed") +
    xlab(expression('Eddy kinetic energy (m'^"2"*' s'^"-2"*')')) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text.x = element_text(color = "black", size = 18),
          axis.title.x = element_text(color = "black", size = 25),
          panel.border = element_rect(color = "black", linewidth = 3)
    )
}

# Plot salinity
plot_sal <- function(df, spp, col) {
  
  dum_list <- list()
  
  for(i in 1:length(spp)) {
    dum_list[[i]] <- df %>% 
      dplyr::mutate(!!sym(spp[i]) := case_when(!!sym(spp[i]) >= median(df[[spp[i]]]) ~ sos_transformed, TRUE ~ NA)) %>% 
      dplyr::select(!!sym(spp[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::bind_cols) %>% # bind columns of list
    tidyr::pivot_longer(everything(), names_to = "species", values_to = "transformed") %>% # pivot the table
    dplyr::mutate(species = fct_relevel(species, rev(spp))) # make sure they're in the desired order
  
  mean_val <- mean(bind_df$transformed, na.rm = TRUE)
  
  # cutoff values for salinity are 32.5 to 36 ppt
  
  bind_df %<>%
    dplyr::mutate(transformed = case_when(transformed > 36 ~ 36,
                                          transformed < 32.5 ~ 32.5,
                                          TRUE ~ transformed))
  
  # then we plot it
  gg <- ggplot(bind_df, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 3, 
                        quantile_lines = TRUE,
                        quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_y_discrete(expand = expansion(add = c(0, 3.5))) +
    scale_x_continuous(expand = c(0.001, 0.001),
                       limits = c(32.2, 36.3)) +
    scale_discrete_manual(aesthetics = "fill", values = col) +
    geom_vline(xintercept = mean_val, color = col[1], linewidth = 2, linetype = "dashed") +
    xlab(expression('Salinity (ppt)')) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text.x = element_text(color = "black", size = 18),
          axis.title.x = element_text(color = "black", size = 25),
          panel.border = element_rect(color = "black", linewidth = 3)
    )
}

# Plot chlorophyll
plot_chl <- function(df, spp, col) {
  
  dum_list <- list()
  
  for(i in 1:length(spp)) {
    dum_list[[i]] <- df %>% 
      dplyr::mutate(!!sym(spp[i]) := case_when(!!sym(spp[i]) >= median(df[[spp[i]]]) ~ chlos_transformed, TRUE ~ NA)) %>% 
      dplyr::select(!!sym(spp[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::bind_cols) %>% # bind columns of list
    tidyr::pivot_longer(everything(), names_to = "species", values_to = "transformed") %>% # pivot the table
    dplyr::mutate(species = fct_relevel(species, rev(spp))) # make sure they're in the desired order
  
  mean_val <- mean(bind_df$transformed, na.rm = TRUE)
  
  # cutoff values for chlorophyll are 0 to 1.5e-7
  
  bind_df %<>%
    dplyr::mutate(transformed = case_when(transformed > 1.5e-7 ~ 1.5e-7,
                                          TRUE ~ transformed))
  
  # then we plot it
  gg <- ggplot(bind_df, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 3, 
                        quantile_lines = TRUE,
                        quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_y_discrete(expand = expansion(add = c(0, 3.5))) +
    scale_x_continuous(expand = c(5e-9, 5e-9),
                       limits = c(2e-8, 1.6e-07)) +
    scale_discrete_manual(aesthetics = "fill", values = col) +
    geom_vline(xintercept = mean_val, color = col[1], linewidth = 2, linetype = "dashed") +
    xlab(expression('Chlorophyll concentrations (mol m'^"-3"*')')) +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text.x = element_text(color = "black", size = 18),
          axis.title.x = element_text(color = "black", size = 25),
          panel.border = element_rect(color = "black", linewidth = 3)
    )
}