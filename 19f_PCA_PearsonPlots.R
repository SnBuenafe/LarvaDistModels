# DESCRIPTION: Plotting the top row Pearson's Correlation plot

# Load preliminaries
source("00_Preliminaries.R")
pacman::p_load(corrplot)
seasons <- c("jan-mar", "apr-jun", "jul-sept", "oct-dec")
PC <- c("PC1", "PC2")

for(i in 1:length(seasons)) {
  for(j in 1:length(PC)) {
    res <- read_csv(here::here(pc_dir, paste("CorrMatrix", PC[j], seasons[i], "r.csv", sep = "_")))
    
    res <- res[1,2:ncol(res)]
    
    res <- res[,order(res[1,], decreasing = TRUE)] %>%  # arrange columns according to their r values
      as.matrix()  # to plot only the top row
    
    file_path_test = here::here(figure_dir, paste("CorrMatrix", PC[j], paste0(seasons[i], ".png"), sep = "_"))
    png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")
    
    corrplot(res,
             # type = "upper",
             order = "original",
             tl.col = "black",
             # addCoef.col = "black",
             tl.srt = 45,
             insig = "blank", # make r values that have p values < 0.05 blank
             col = COL2('BrBG', 200),
             diag = TRUE,
             col.lim = c(-1, 1), # dictate limits of colors
             # addgrid.col = NA # remove the grid
             )
    
    dev.off()
    
  }
}