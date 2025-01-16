# DESCRIPTION: Calculating the index of dispersion (Coefficient of Variation - CV)
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))
# source(here::here("Utils", "fxnshemisphere.R"))

pacman::p_load(purrr, magrittr, ggrepel, patchwork)

seasons <- c("jan-mar", "apr-jun", "jul-sep", "oct-dec")

# Add the groups
spec_dict
groups <- c("Tuna", "Tuna", "Tuna", "Billfish", "Billfish", "Tuna", "Tuna", "Tuna", "Other", "Billfish", "Tuna", "Tuna", "Billfish", "Billfish", "Other")
LifeHistory <- c("Fast", "Fast", "Slow", "Slow", "Slow", "Fast", "Fast", "Slow", "Unknown", "Slow", "Slow", "Unknown", "Slow", "Slow", "Unknown")

spec_dict %<>%
  dplyr::filter(!code %in% c("BON", "LIT")) %>% # Remove species with too few data
  dplyr::bind_cols(., groups = groups) %>% 
  dplyr::bind_cols(., lifehistory = LifeHistory)

# Define the Spatial Aggregation Index (SAI, based on CV)
# Logic: for each season for each species, the larger the sd 
index <- function(x) {
  ind <- sd(x, na.rm = TRUE)/(mean(x, na.rm = TRUE))
}

# Prepare data
df_SAI <- list()
for(i in 1:length(seasons)) {
  # Load file
  tmp <- terra::rast(here::here(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
    as.data.frame(xy = TRUE) %>% # get longitude and latitudes
    dplyr::as_tibble() %>% 
    dplyr::mutate(hemisphere = ifelse(y >= 0, "North", "South")) # separate them per hemisphere
  
  # Repeat for all 4 seasons            
  df_SAI[[i]] <- tmp %>% 
    dplyr::group_by(hemisphere) %>% 
    dplyr::summarise(across(!c(x, y), index)) %>% # *** Calculate SAI
    tidyr::pivot_longer(cols = !c(hemisphere),
                        names_to = "species", 
                        values_to = "ind") %>% 
    dplyr::rename(!!sym(paste("ind", seasons[i], sep = "_")) := ind) # Rename columns
}

# Calculate across seasons
full_SAI <- purrr::reduce(df_SAI, dplyr::left_join, by = c("species", "hemisphere")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(SAI = mean(c_across(starts_with("ind")), na.rm = TRUE)) %>% # SAI Annual = Mean of SAI across Seasons
  dplyr::select(species, hemisphere, SAI) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hemisphere) %>% 
  dplyr::left_join(., spec_dict, by = "code") # %>% 
  # dplyr::mutate(common = fct_reorder(common, desc(SAI))) %>% # We want to plot it with the common names
  # dplyr::bind_cols(., )

# Calculate Seasonality Index (SI, aka Reviewer #2) for each Season
df_SI <- list()
for(i in 1:length(seasons)) {
  # Load file
  tmp <- terra::rast(here::here(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
    as.data.frame(xy = TRUE) %>% # get longitude and latitudes
    dplyr::as_tibble() %>% 
    dplyr::mutate(hemisphere = ifelse(y >= 0, "North", "South")) # separate them per hemisphere
  
  # Repeat for all 4 seasons            
    df_SI[[i]] <- tmp %>% 
    dplyr::group_by(hemisphere) %>% 
    dplyr::summarise(across(!c(x, y), mean, na.rm = TRUE)) %>% # Calculate the index
    tidyr::pivot_longer(cols = !c(hemisphere),
                        names_to = "species", 
                        values_to = "ind") %>% 
    dplyr::rename(!!sym(paste("ind", seasons[i], sep = "_")) := ind) # Rename columns
}

# Calculate across seasons
full_SI <- purrr::reduce(df_SI, dplyr::left_join, by = c("species", "hemisphere")) %>% 
  dplyr::rowwise() %>% 
  # dplyr::mutate(SI = sd(c_across(starts_with("ind")), na.rm = TRUE)) %>% # Take the SD across seasons for mean per species (Seasonality Index)
  dplyr::mutate(SI = sd(c_across(starts_with("ind")), na.rm = TRUE) / mean(c_across(starts_with("ind")), na.rm = TRUE)) %>% # Take the SD across seasons for mean per species and standardise for the mean (Seasonality Index)
  dplyr::select(species, hemisphere, SI) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hemisphere) %>% 
  dplyr::left_join(., spec_dict, by = "code")

# Calculate the weighted mean Lat for each species in each hemisphere
# Calculate MnLat for each Season (for each Species and each Hemisphere)
df_MnLat <- list()
for(i in 1:length(seasons)) {
  # Load file
  tmp <- terra::rast(here::here(rast_dir, paste("ModelOutputs", paste0(seasons[i], ".tif"), sep = "_"))) %>% 
    as.data.frame(xy = TRUE) %>% # get longitude and latitudes
    dplyr::as_tibble() %>% 
    dplyr::mutate(hemisphere = ifelse(y >= 0, "North", "South")) # separate them per hemisphere
  
  # Repeat for all 4 seasons            
  df_MnLat[[i]] <- tmp %>% 
    dplyr::group_by(hemisphere) %>% 
    dplyr::summarise(across(!c(x, y), ~ sum(.x * y, na.rm = TRUE)/sum(.x, na.rm = TRUE))) %>% # Calculate index2
    tidyr::pivot_longer(cols = !c(hemisphere),
                        names_to = "species", 
                        values_to = "ind") %>% 
    dplyr::rename(!!sym(paste("ind", seasons[i], sep = "_")) := ind) # Rename columns
}

# Calculate across seasons
full_MnLat <- purrr::reduce(df_MnLat, dplyr::left_join, by = c("species", "hemisphere")) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(MnLat = mean(c_across(starts_with("ind")), na.rm = TRUE)) %>%
  dplyr::select(species, hemisphere, MnLat) %>% 
  dplyr::mutate(code = toupper(species)) %>% 
  dplyr::select(-species) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(hemisphere) %>% 
  dplyr::left_join(., spec_dict, by = "code")

# Join SAI and SI in one df
full <- left_join(full_SAI, full_SI) #, by = c("common", "hemisphere"))
full <- left_join(full, full_MnLat) #, by = c("common", "hemisphere"))

# Remove single hemisphere species
# SBFT in NH
# BFT in SH
# SLT in NH
full <- full %>% mutate(SAI = replace(SAI, code == "SBFT" & hemisphere == "North", NA))
full <- full %>% mutate(SI = replace(SI, code == "SBFT" & hemisphere == "North", NA))
full <- full %>% mutate(SAI = replace(SAI, code == "BFT" & hemisphere == "South", NA))
full <- full %>% mutate(SI = replace(SI, code == "BFT" & hemisphere == "South", NA))
full <- full %>% mutate(SAI = replace(SAI, code == "SLT" & hemisphere == "North", NA))
full <- full %>% mutate(SI = replace(SI, code == "SLT" & hemisphere == "North", NA))


# What is the relationship between SI vs SAI
m1 <- lm(SI ~ SAI, data = full)
summary(m1) # Simplest model is best

m2 <- lm(log10(SI) ~ log10(SAI), data = full)
summary(m2)

m3 <- lm(SI ~ log(SAI), data = full)
summary(m3)

full <- full %>% 
  rename(Hemisphere = hemisphere)

# plot SAI and SI vs Lat for groups
theme_set(new = theme_bw(base_size = 25))

# Plot Spatial Aggregation Index (full_SAI) vs Seasonality Index (full_SI)
ggplot(data = full, 
       aes(x = SAI, y = SI, label = code)) +
  geom_point(size = 1.5, aes(shape = Hemisphere)) +
  xlab("Spatial Aggregation Index") +
  ylab("Seasonality Index") +
  geom_text_repel() +
  scale_shape_manual(values = c(1, 16)) +
  labs(shape = "Hemisphere") + 
  geom_smooth(method = "lm") +
  theme_bw()

ggsave(filename = here::here(figure_dir, "Dispersion.png"), dpi = 600, width = 15, height = 8, units = "in")

# SAI vs SI (Groups)
PtSize = 4
ggplot(data = full, 
       aes(x = SAI, y = SI, label = code)) +
  geom_point(size = 1.5, aes(shape = groups)) +
  xlab("Spatial Aggregation Index") +
  ylab("Seasonality Index") +
  geom_text_repel() +
  geom_point(size = PtSize, aes(shape = groups, colour = groups)) +
  geom_smooth(method = "lm") +
  theme_bw()


# full2
# average across hemispheres
full2 <- full %>% 
  group_by(code) %>% 
  summarise(Grps = first(groups), 
            LH = first(lifehistory),
            MnSAI = mean(SAI, na.rm = TRUE), 
            MnSI = mean(SI, na.rm = TRUE), 
            MnLat = mean(abs(MnLat), na.rm = TRUE))

# Groups
p1 <- ggplot(data = full2,
             aes(x = MnSAI, y = MnSI, label = code)) +
  geom_smooth(method = "lm") +
  xlab("Spatial Aggregation Index") +
  ylab("Seasonality Index") +
  geom_text_repel() +
  geom_point(size = PtSize, aes(shape = Grps, colour = Grps))

# Life history
p2 <- ggplot(data = full2,
       aes(x = MnSAI, y = MnSI, label = code)) +
  geom_smooth(method = "lm") +
  xlab("Spatial Aggregation Index") +
  ylab("Seasonality Index") +
  geom_text_repel() +
  geom_point(size = PtSize, aes(shape = LH, colour = LH))

p1 + p2 + plot_layout(ncol = 1, axes = "collect_x") & 
  theme(legend.position = "top")

ggsave(filename = here::here(figure_dir, "LifeHistoryPlots.png"), dpi = 600, width = 12, height = 15, units = "in")

# Investigate life history by functional group
# Reorder Groups first

full2 <- full2 %>%
  mutate(Grps = fct_relevel(Grps, "Tuna", "Billfish", "Other"), 
         LH = fct_relevel(LH, "Slow", "Fast", "Unknown"))

p3 <- ggplot(data = full2, 
             aes(x = Grps, y = MnSAI)) +
  geom_boxplot() +
  xlab("Groups") +
  ylab("Spatial Aggregation Index")

p4 <- ggplot(data = full2, 
            aes(x = Grps, y = MnSI)) +
  geom_boxplot() +
  xlab("Groups") +
  ylab("Seasonality Index")


# Investigate life history by speed of growth (LH)
p5 <- ggplot(data = full2, 
             aes(x = LH, y = MnSAI)) +
  geom_boxplot() +
  xlab("Speed of life history") +
  ylab("Spatial Aggregation Index")

p6 <- ggplot(data = full2, 
             aes(x = LH, y = MnSI)) +
  geom_boxplot() +
  xlab("Speed of life history") +
  ylab("Seasonality Index")


# Put life history figures together
Fig3 <- p3 + p4 + p5 + p6 + plot_layout(ncol = 2, byrow = FALSE, axes = "collect") +
  plot_annotation(tag_levels = 'a')

ggsave(filename = here::here(figure_dir, "Fig3_LH_Figure.png"), plot = Fig3, dpi = 600, width = 19, height = 15, units = "in")
ggsave(filename = here::here(figure_dir, "Fig3_LH_Figure.pdf"), plot = Fig3, dpi = 600, width = 19, height = 15, units = "in")


# Investigate SAI and SI by Latitude
p7 <- ggplot(data = full, 
       aes(x = abs(MnLat), y = SAI, label = code)) + 
  geom_smooth(method = "lm") +
  xlab("Latitude (°S or °N)") +
  ylab("Spatial Aggregation Index") +
  geom_text_repel() +
  geom_point(size = PtSize, aes(shape = Hemisphere, colour = Hemisphere))

p8 <- ggplot(data = full, 
             aes(x = abs(MnLat), y = SI, label = code)) +
  geom_smooth(method = "lm") +
  xlab("Latitude (°S or °N)") +
  ylab("Seasonality Index") +
  geom_text_repel() +
  geom_point(size = PtSize, aes(shape = Hemisphere, colour = Hemisphere))

Fig4 <- p7 + p8 + plot_layout(ncol = 1, guides = "collect", axes = "collect_x") + 
  plot_annotation(tag_levels = 'a') & 
  theme(legend.position = "top")

ggsave(filename = here::here(figure_dir, "Fig4_IndicesPlots.png"), plot = Fig4, dpi = 600, width = 12, height = 17, units = "in")
ggsave(filename = here::here(figure_dir, "Fig4_IndicesPlots.pdf"), plot = Fig4, dpi = 600, width = 12, height = 17, units = "in")

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

