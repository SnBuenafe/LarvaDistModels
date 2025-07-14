# DESCRIPTION: Explore the relationship of SAI and SI across latitudes


# Source preliminaries ----------------------------------------------------
source(file.path("analyses", "05_spawning_analyses", "20a_SpawningIndices.R"))


# Explore SAI and SI across latitudes ----------------------------------------------

# What is the relationship between SAI vs Lat and SI vs Lat?
m1 <- lm(SAI ~ abs(MnLat), data = full)
summary(m1)
plot(abs(full$MnLat), full$SAI)

# Calculate confidence intervals
df_conf_intervals <- predict(m1, interval = "confidence", level = 0.95) %>% 
  as_tibble()
min(df_conf_intervals$lwr) # 0.7061857
max(df_conf_intervals$upr) # 4.264416

m2 <- lm(log10(SAI) ~ log10(abs(MnLat)), data = full)
summary(m2) # m1 is better - keep it simple

m3 <- lm(SI ~ abs(MnLat), data = full)
summary(m3)
plot(abs(full$MnLat), full$SI)

m4 <- lm(log10(SI) ~ log10(abs(MnLat)), data = full)
summary(m4)

# Calculate confidence intervals
df_conf_intervals <- predict(m3, interval = "confidence", level = 0.95) %>% 
  as_tibble()
min(df_conf_intervals$lwr) # 0.1108024
max(df_conf_intervals$upr) # 1.095436

m4 <- lm(log10(SI) ~ log10(abs(MnLat)), data = full)
summary(m4) # m3 is better - keep it simple


# SAI across latitudes ----------------------------------------------------

p7 <- ggplot(data = full, 
             aes(x = abs(MnLat), y = SAI, label = code)) + 
  geom_smooth(method = "lm", alpha = 0.15, color = "grey64") +
  xlab("Latitude (°N or °S)") +
  ylab("Spatial Aggregation Index") +
  geom_errorbar(aes(ymin = SAI-SAI_SEM, ymax = SAI+SAI_SEM), linewidth = 0.5, width = 0.2, color = "grey60") +
  geom_label_repel(aes(fill = Hemisphere), color = "white", segment.color = "black", min.segment.length = 0, box.padding = 0.5, force = 1.5, show.legend = FALSE) +
  geom_point(size = 4, aes(shape = Hemisphere, colour = Hemisphere), show.legend = FALSE) +
  scale_x_continuous(breaks = seq(12, 28, 4)) +
  scale_shape_manual(values = c(16, 18)) +
  scale_color_manual(aesthetics = c("color", "fill"),
                     values = c("North" = "#0084C2", "South" = "#EAB47F")) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black"),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_blank())

ggsave(filename = file.path(figure_dir, "SAI_across_latitudes.png"), plot = p7, dpi = 600, width = 10, height = 7, units = "in")

# SI across latitudes -----------------------------------------------------

p8 <- ggplot(data = full, 
             aes(x = abs(MnLat), y = SI, label = code)) +
  geom_smooth(method = "lm", alpha = 0.15, color = "grey64") +
  xlab("Latitude (°S or °N)") +
  ylab("Seasonality Index") +
  geom_errorbar(aes(ymin = SI-SI_SEM, ymax = SI+SI_SEM), linewidth = 0.5, width = 0.2, color = "grey60") +
  geom_label_repel(aes(fill = Hemisphere), color = "white", segment.color = "black", min.segment.length = 0, box.padding = 0.5, force = 1.5, show.legend = FALSE) +
  geom_point(size = 4, aes(shape = Hemisphere, colour = Hemisphere), show.legend = FALSE) +
  scale_x_continuous(breaks = seq(12, 28, 4)) +
  scale_shape_manual(values = c(16, 18)) +
  scale_color_manual(aesthetics = c("color", "fill"),
                     values = c("North" = "#0084C2", "South" = "#EAB47F")) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black"),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_blank())

ggsave(filename = file.path(figure_dir, "SI_across_latitudes.png"), plot = p8, dpi = 600, width = 10, height = 7, units = "in")

gg <- p7 + p8 + plot_layout(ncol = 1, guides = "collect", axes = "collect_x") + 
  plot_annotation(tag_levels = 'a') & 
  theme(legend.position = "top")

