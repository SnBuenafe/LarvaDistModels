# DESCRIPTION: Plotting a summary of 20a-e

# Run all previous scripts first
source("00_Preliminaries.R")
scripts <- list.files(pattern = "^20", full.names = TRUE)
sapply(X = scripts[1:5], FUN = source) %>% invisible()

all <- ab_tunas1 + ab_tunas2 + ab_tunas3 + ab_tunas4 + ab_tunas5 +
  lab_tunas1 + lab_tunas2 + lab_tunas3 + lab_tunas4 + lab_tunas5 +
  bill1 + bill2 + bill3 + bill4 + bill5 +
  oth1 + oth2 + oth3 + oth4 + oth5 +
  plot_layout(ncol = 5, nrow = 4) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 30))

ggsave(plot = all, filename = here::here(figure_dir, "ImptPredictors_all.png"), width = 60, height = 30, dpi = 300, limitsize = FALSE)
