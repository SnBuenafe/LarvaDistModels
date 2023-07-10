# DESCRIPTION: Plot correlation matrices

plotCorrMat <- function(x) {
  
  gg_cor <- ggcorrplot::ggcorrplot(x, 
                                   method = "circle",
                                   outline.color = "black"
  ) +
    expand_limits(y = c(0.5,1.5)) +
    scale_fill_gradient2(
      name = "Correlation coefficient",
      low = "#8c510a",
      mid = "#f5f5f5",
      high = "#01665e",
      limits = c(-1, 1)) +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    theme_bw() +
    theme(legend.title = element_text(),
          legend.text = element_text(color = "black", size = 10),
          panel.grid = element_blank(),
          panel.grid.major = element_line(color = "grey86"),
          panel.border = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(color = "black", size = 12))
  
  return(gg_cor)
  
}