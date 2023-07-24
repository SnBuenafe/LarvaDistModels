# DESCRIPTION: Summary plots for surface temperature

# Define preliminaries
source("00_Preliminaries.R")
pacman::p_load(ggridges, patchwork, purrr, sf)

spp_list <- spec_dict %>% 
  dplyr::filter(!code %in% c("BON", "LIT")) %>% # remove bonitos and little tuna
  dplyr::select(code) %>% 
  pull() %>% 
  tolower()

# Assemble data frame
df <- assemblePredictor(spp_list, "tos")
# TODO: Delete assemblePreds.R

#### Scatter plots with loess smoother ####
ggplot(data = df, aes(x = tos_transformed, y = yft)) + 
  geom_point(size = 0.1) +
  geom_smooth(method = "loess", color = "red", size = 1) +
  theme_bw()

fin_tmp <- assemblePreds("tos")



#### Kernel density plots ####

# Abundant tunas
spp <- c("skp", "yft", "alb", "bet", "fri")
col_values <- c("#48594B", "#708B75", "#91B397", "#A5CCAC", "#BAE6C1")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = case_when(transformed < 20 ~ 20,
                                        transformed > 30 ~ 30,
                                        TRUE ~ transformed))

(ab_tunas <- plot_KD(df,
                     spp,
                     expression('Temperature ('^"o"*'C)'),
                     limits = c(20, 30),
                     col_values
) +
    theme(axis.title.x = element_blank())) # KD plots

# Less abundant tunas
spp <- c("sbft", "bft", "lit", "slt", "bon")
col_values <- c("#48594B", "#708B75", "#91B397", "#A5CCAC", "#BAE6C1")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = case_when(transformed < 20 ~ 20,
                                        transformed > 30 ~ 30,
                                        TRUE ~ transformed))

(lab_tunas <- plot_KD(df,
                      spp,
                      expression('Temperature ('^"o"*'C)'),
                      limits = c(20, 30),
                      col_values
) +
    theme(axis.title.x = element_blank())) # KD plots

# Billfish
spp <- c("blum", "shos", "swo", "strm", "sail")
col_values <- c("#6A637A", "#9086A6", "#A297BA", "#B8ABD4", "#CABCE8")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = case_when(transformed < 20 ~ 20,
                                        transformed > 30 ~ 30,
                                        TRUE ~ transformed))

(bill <- plot_KD(df,
                 spp,
                 expression('Temperature ('^"o"*'C)'),
                 limits = c(20, 30),
                 col_values
) +
    theme(axis.title.x = element_blank())) # KD plots

# Other species
spp <- c("lesc", "sau")
col_values <- c("#DB9F1D", "#FFD67D")

df <- prepareDF(fin_tmp, spp) # assemble data frame to plot the KD plot
printSummary(df)

df %<>%
  dplyr::mutate(transformed = case_when(transformed < 20 ~ 20,
                                        transformed > 30 ~ 30,
                                        TRUE ~ transformed))

(oth <- plot_KD(df,
                spp,
                expression('Temperature ('^"o"*'C)'),
                limits = c(20, 30),
                col_values
)) # KD plots

all <- ((ab_tunas) / (lab_tunas) / (bill) / oth)

ggsave(plot = all, filename = here::here(figure_dir, "ImptPredictors_tos.png"), width = 15, height = 27, dpi = 300)