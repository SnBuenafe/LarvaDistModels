# List of similar aesthetics throughout the plotting process

change_gglayout <- function(x) {
  
  list(
    theme_bw(),
    theme(plot.title = element_text(size = 28, color = "black"),
          legend.position = "bottom",
          axis.title = element_blank(),
          legend.text = element_text(size = 30, color = "black"),
          legend.title = element_text(size = 30, color = "black"),
          axis.text = element_text(size = 25, color = "black"),
          panel.border = element_rect(linewidth = 2, color = "black"),
          plot.margin = unit(c(0,0.5,0,0.5), "cm")),
    coord_sf(xlim = st_bbox(grid)$xlim, ylim = st_bbox(grid)$ylim)
  )
  
}
