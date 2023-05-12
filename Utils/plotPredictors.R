# DESCRIPTION: Plots for Predictors

# Plot surface temperature
plot_tos <- function(df, spp, col) {
  
  dum_list <- list()
  
  for(i in 1:length(spp)) {
    dum_list[[i]] <- df %>% 
      dplyr::mutate(!!sym(spp[i]) := case_when(!!sym(spp[i]) >= median(df[[spp[i]]]) ~ tos_transformed, TRUE ~ NA)) %>% 
      dplyr::select(!!sym(spp[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::bind_cols) %>% # bind columns of list
    tidyr::pivot_longer(everything(), names_to = "species", values_to = "transformed") %>% # pivot the table
    dplyr::mutate(species = fct_relevel(species, rev(spp))) # make sure they're in the desired order
  
  mean_val <- mean(bind_df$transformed, na.rm = TRUE)
  
  # cutoff values for temperature are 20C - 30C
  # we change all values <20 to 20C and all values >30 to 30C
  
  bind_df %<>%
    dplyr::mutate(transformed = case_when(transformed < 20 ~ 20,
                                          transformed > 30 ~ 30,
                                          TRUE ~ transformed))
  
  # then we plot it
  gg <- ggplot(bind_df, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 3, 
                        quantile_lines = TRUE,
                        quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_y_discrete(expand = expansion(add = c(0, 3.5))) +
    scale_x_continuous(expand = c(0.01, 0.01),
                       limits = c(20, 30)) +
    scale_discrete_manual(aesthetics = "fill", values = col) +
    #geom_vline(xintercept = mean_val, color = col[1], linewidth = 2, linetype = "dashed") +
    xlab(expression('Temperature ('^"o"*'C)')) +
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

# Plot broad-scale thermal gradients
plot_thermgrad <- function(df, spp, col) {
  
  dum_list <- list()
  
  for(i in 1:length(spp)) {
    dum_list[[i]] <- df %>% 
      dplyr::mutate(!!sym(spp[i]) := case_when(!!sym(spp[i]) >= median(df[[spp[i]]]) ~ thermal_front_transformed, TRUE ~ NA)) %>% 
      dplyr::select(!!sym(spp[i]))
  }
  
  bind_df <- purrr::reduce(dum_list, dplyr::bind_cols) %>% # bind columns of list
    tidyr::pivot_longer(everything(), names_to = "species", values_to = "transformed") %>% # pivot the table
    dplyr::mutate(species = fct_relevel(species, rev(spp))) # make sure they're in the desired order
  
  mean_val <- mean(bind_df$transformed, na.rm = TRUE)
  
  # cutoff values for broad-scale thermal gradients are 0 to 0.01
  
  bind_df %<>%
    dplyr::mutate(transformed = case_when(transformed > 0.01 ~ 0.01,
                                          TRUE ~ transformed))
  
  # then we plot it
  gg <- ggplot(bind_df, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 3, 
                        quantile_lines = TRUE,
                        quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_y_discrete(expand = expansion(add = c(0, 3.5))) +
    scale_x_continuous(expand = c(0.00001, 0.00001),
                       limits = c(-0.001, 0.011)) +
    scale_discrete_manual(aesthetics = "fill", values = col) +
    geom_vline(xintercept = mean_val, color = col[1], linewidth = 2, linetype = "dashed") +
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