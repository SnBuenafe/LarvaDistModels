# DESCRIPTION: Plot the predictor vs predictions (scatter plot) and add a smoother on top

plot_scatter_predictor <- function(df,
                                   col, # column name of predictor
                                   spp # species code
) {
  gg <- ggplot(data = df, aes(x = !!sym(col), y = !!sym(spp))) + 
    geom_point(size = 0.1) +
    geom_smooth(method = "loess", color = "salmon", linewidth = 1) +
    ylim(c(0,1)) +
    theme_bw() +
    theme(axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_line(color = "black"),
          axis.text = element_text(size = 12, color = "black"),
          panel.border = element_blank(),
          axis.title.x = element_blank(),
          axis.line = element_line(color = "black", linewidth = 1)
    )
  
  return(gg)
  
}