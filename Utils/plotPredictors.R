# DESCRIPTION: Plots for Predictors

# Plot surface temperature
plot_tos <- function(tmp, col, mean_val) {
  gg <- ggplot(tmp, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 7, 
                        #quantile_lines = TRUE,
                        #quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_y_discrete(expand = expansion(add = c(0, 7.5))) +
    scale_x_continuous(expand = c(0.01, 0.01),
                       limits = c(20, 31)) +
    scale_discrete_manual(aesthetics = "fill", values = col) +
    geom_vline(xintercept = mean_val, color = col[1], linewidth = 2, linetype = "dashed") +
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
plot_thermgrad <- function(tmp, col, mean_val) {
  gg <- ggplot(tmp, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 7, 
                        #quantile_lines = TRUE,
                        #quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_x_continuous(expand = expansion(add = c(0.00001, 0.0005)),
                       limits = c(-0.001, 0.015)) +
    scale_y_discrete(expand = expansion(add = c(0, 7.5))) +
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
          panel.border = element_rect(color = "black", linewidth = 3))
}

# Plot EKE
plot_eke <- function(tmp, col, mean_val) {
  gg <- ggplot(tmp, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 7, 
                        #quantile_lines = TRUE,
                        #quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_x_continuous(expand = expansion(add = c(0.0001, 0.0005)),
                       limits = c(-0.003, 0.025)
                       ) +
    scale_y_discrete(expand = expansion(add = c(0, 7.5))) +
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
          panel.border = element_rect(color = "black", linewidth = 3))
}

# Plot salinity
plot_sal <- function(tmp, col, mean_val) {
  gg <- ggplot(tmp, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 7, 
                        #quantile_lines = TRUE,
                        #quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_x_continuous(expand = expansion(add = c(0.1, 0.1)),
                       limits = c(32, 36.5)
    ) +
    scale_y_discrete(expand = expansion(add = c(0, 7.5))) +
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
          panel.border = element_rect(color = "black", linewidth = 3))
}

# Plot chlorophyll
plot_chl <- function(tmp, col, mean_val) {
  gg <- ggplot(tmp, aes(x = transformed, y = species, fill = species)) +
    geom_density_ridges(alpha = 0.8, 
                        scale = 7, 
                        #quantile_lines = TRUE,
                        #quantile_fun = function(x,...)mean(x),
                        show.legend = FALSE) +
    scale_x_continuous(expand = expansion(add = c(5e-9, 5e-9)),
                       limits = c(2e-8, 2e-07)
    ) +
    scale_y_discrete(expand = expansion(add = c(0, 7.5))) +
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
          panel.border = element_rect(color = "black", linewidth = 3))
}
