# DESCRIPTION: Exploring relationship of SAI and SI across hemispheres

# Source preliminaries ----------------------------------------------------
source(file.path("analyses", "05_spawning_analyses", "20a_SpawningIndices.R"))

# Exploring relationships between SAI and SI ------------------------------

# What is the relationship between SI vs SAI
m1 <- lm(SI ~ SAI, data = full)
summary(m1) # Simplest model is best

m2 <- lm(log10(SI) ~ log10(SAI), data = full)
summary(m2)

m3 <- lm(SI ~ log(SAI), data = full)
summary(m3)


# Plotting SAI vs SI across hemispheres -----------------------------------
ggplot(data = full, 
       aes(x = SAI, y = SI, label = code)) +
  geom_smooth(method = "lm", alpha = 0.15, color = "grey64") +
  geom_point(size = 5, aes(shape = Hemisphere#, 
                           #color = Hemisphere
                           )
             ) +
  scale_shape_manual(values = c(16, 18)) +
  # scale_color_manual(aesthetics = "color",
  #                    values = c("North" = "#0084C2", "South" = "#EAB47F")) +
  xlab("Spatial Aggregation Index") +
  ylab("Seasonality Index") +
  geom_label_repel(size = 5, 
                  # aes(fill = Hemisphere), 
                   color = "black", segment.color = "black", show.legend = FALSE,
                   box.padding = 0.5) +
  # scale_fill_manual(aesthetics = "fill",
  #                   values = c("North" = "#0084C2", "South" = "#EAB47F")) +
  labs(shape = "Hemisphere") + 
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        axis.title = element_text(size = 15, color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        panel.border = element_rect(color = "black", linewidth = 1))

ggsave(filename = file.path(figure_dir, "SAI_SI_across_hemispheres_tmp.png"), dpi = 600, width = 15, height = 8, units = "in")
