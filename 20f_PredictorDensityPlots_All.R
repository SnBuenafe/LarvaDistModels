# DESCRIPTION: Plotting a summary of 20a-e

# Run all previous scripts first
source("00_Preliminaries.R")
scripts <- list.files(pattern = "^20", full.names = TRUE)
sapply(X = scripts[1:5], FUN = source) %>% invisible()
fig_dir <- here::here("Figures")

all <- tunas1 + tunas2 + tunas3 + tunas4 + tunas5 +
  bill1 + bill2 + bill3 + bill4 + bill5 +
  oth1 + oth2 + oth3 + oth4 + oth5 +
  plot_layout(ncol = 5, nrow = 3) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 25))

ggsave(plot = all, filename = here::here(fig_dir, "ImptPredictors_all.png"), width = 60, height = 20, dpi = 300, limitsize = FALSE)
