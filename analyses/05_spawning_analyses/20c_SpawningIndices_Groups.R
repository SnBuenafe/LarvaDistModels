# DESCRIPTION: Explore the relationship of SAI and SI across taxonomic groups and life history strategies


# Source preliminaries ----------------------------------------------------
source(file.path("analyses", "05_spawning_analyses", "20a_SpawningIndices.R"))


# Explore SAI vs SI across taxonomic groups -------------------------------
ggplot(data = full,
       aes(x = SAI, y = SI, label = code)) +
  geom_point(size = 1.5, aes(shape = groups)) +
  xlab("Spatial Aggregation Index") +
  ylab("Seasonality Index") +
  geom_text_repel() +
  geom_point(size = 4, aes(shape = groups, colour = groups)) +
  geom_smooth(method = "lm") +
  theme_bw() # plot still has each taxon separated into different hemispheres

# So now, we take the average across hemispheres
full2 <- full %>% 
  group_by(code) %>% 
  summarise(Grps = first(groups), 
            LH = first(lifehistory),
            MnSAI = mean(SAI, na.rm = TRUE), 
            MnSI = mean(SI, na.rm = TRUE), 
            MnLat = mean(abs(MnLat), na.rm = TRUE))

# SAI vs SI across taxonomic groups
ggplot(data = full2,
             aes(x = MnSAI, y = MnSI, label = code)) +
  geom_smooth(method = "lm") +
  xlab("Spatial Aggregation Index") +
  ylab("Seasonality Index") +
  geom_text_repel() +
  geom_point(size = 4, aes(shape = Grps, colour = Grps))


# Explore SAI vs SI across life history strategies ------------------------

ggplot(data = full2,
             aes(x = MnSAI, y = MnSI, label = code)) +
  geom_smooth(method = "lm") +
  xlab("Spatial Aggregation Index") +
  ylab("Seasonality Index") +
  geom_text_repel() +
  geom_point(size = 4, aes(shape = LH, colour = LH))



# Explore indices across taxonomic groups --------


# SAI across taxonomic groups ---------------------------------------------


full2 <- full2 %>% # Reorder Groups first
  mutate(Grps = fct_relevel(Grps, "Tuna", "Billfish", "Other"), 
         LH = fct_relevel(LH, "Slow", "Fast", "Unknown"))

p3 <- ggplot(data = full2, 
             aes(x = Grps, y = MnSAI)) +
  geom_boxplot() +
  xlab("Groups") +
  ylab("Spatial Aggregation Index") +
  theme_bw() +
  theme(panel.border = element_rect(color = "black"),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_blank())

ggsave(filename = file.path(figure_dir, "SAI_across_groups.png"), plot = p3, dpi = 600, width = 7, height = 5, units = "in")


# SI across taxonomic groups --------------------------------------

p4 <- ggplot(data = full2, 
             aes(x = Grps, y = MnSI)) +
  geom_boxplot() +
  xlab("Groups") +
  ylab("Seasonality Index") +
  theme_bw() +
  theme(panel.border = element_rect(color = "black"),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_blank())

ggsave(filename = file.path(figure_dir, "SI_across_groups.png"), plot = p4, dpi = 600, width = 7, height = 5, units = "in")


# Explore indices across life history strategies --------------------------


# SAI across life history strategies --------------------------------------

p5 <- ggplot(data = full2, 
             aes(x = LH, y = MnSAI)) +
  geom_boxplot() +
  xlab("Life history strategy") +
  ylab("Spatial Aggregation Index") +
  theme_bw() +
  theme(panel.border = element_rect(color = "black"),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_blank())

ggsave(filename = file.path(figure_dir, "SAI_across_life_history.png"), plot = p5, dpi = 600, width = 7, height = 5, units = "in")


# SI across life history strategies ---------------------------------------

p6 <- ggplot(data = full2, 
             aes(x = LH, y = MnSI)) +
  geom_boxplot() +
  xlab("Life history strategy") +
  ylab("Seasonality Index") +
  theme_bw() +
  theme(panel.border = element_rect(color = "black"),
        axis.text = element_text(color = "black", size = 20),
        axis.title = element_blank())

ggsave(filename = file.path(figure_dir, "SI_across_life_history.png"), plot = p6, dpi = 600, width = 7, height = 5, units = "in")

# Put life history figures together
gg <- p3 + p4 + p5 + p6 + plot_layout(ncol = 2, byrow = FALSE, axes = "collect") +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = '(',
                  tag_suffix = ')') &
  theme(plot.tag = element_text(size = 20, color = "black"))

# ggsave(filename = file.path(figure_dir, "SAI_SI_across_groups.png"), plot = gg, dpi = 600, width = 15, height = 10, units = "in")
