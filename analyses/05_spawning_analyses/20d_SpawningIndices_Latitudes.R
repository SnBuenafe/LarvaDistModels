# DESCRIPTION: Explore the relationship of SAI and SI across latitudes


# Source preliminaries ----------------------------------------------------
source(file.path("analyses", "05_spawning_analyses", "20a_SpawningIndices.R"))


# Explore SAI and SI across latitudes ----------------------------------------------

# What is the relationship between SAI vs Lat and SI vs Lat?
m1 <- lm(SAI ~ abs(MnLat), data = full)
summary(m1)
plot(abs(full$MnLat), full$SAI)

m2 <- lm(log10(SAI) ~ log10(abs(MnLat)), data = full)
summary(m2) # m1 is better - keep it simple

m3 <- lm(SI ~ abs(MnLat), data = full)
summary(m3)
plot(abs(full$MnLat), full$SI)

m4 <- lm(log10(SI) ~ log10(abs(MnLat)), data = full)
summary(m4) # m3 is better - keep it simple


# SAI across latitudes ----------------------------------------------------

p7 <- ggplot(data = full, 
             aes(x = abs(MnLat), y = SAI, label = code)) + 
  geom_smooth(method = "lm", alpha = 0.15, color = "grey64") +
  geom_smooth(method = "lm") +
  xlab("Latitude (°N or °S)") +
  ylab("Spatial Aggregation Index") +
  geom_text_repel() +
  geom_point(size = 4, aes(shape = Hemisphere, colour = Hemisphere)) +
  scale_shape_manual(values = c(16, 18)) +
  scale_color_manual(aesthetics = "color",
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
  geom_text_repel() +
  geom_point(size = 4, aes(shape = Hemisphere, colour = Hemisphere)) +
  scale_shape_manual(values = c(16, 18)) +
  scale_color_manual(aesthetics = "color",
                     values = c("North" = "#0084C2", "South" = "#EAB47F")) +
  theme_bw() +
  theme(panel.border = element_rect(color = "black"),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_blank())

ggsave(filename = file.path(figure_dir, "SI_across_latitudes.png"), plot = p8, dpi = 600, width = 10, height = 7, units = "in")

gg <- p7 + p8 + plot_layout(ncol = 1, guides = "collect", axes = "collect_x") + 
  plot_annotation(tag_levels = 'a') & 
  theme(legend.position = "top")

