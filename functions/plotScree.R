# DESCRIPTION: Plot scree plot

plotScree <- function(var) {
  scree_obj <- var %>% 
    as.table() %>% 
    as.data.frame() %>% 
    dplyr::mutate(Var1 = as.numeric(substr(Var1, 6, 7)))
  
  scree_gg <- ggplot() + 
    geom_col(data = scree_obj,
             aes(x = as.factor(Var1), y = Freq),
             color = "slategrey",
             fill = "slategrey") +
    geom_point(data = scree_obj,
               aes(x = as.factor(Var1), y = Freq),
               color = "black",
               size = 2) +
    scale_y_continuous(expand = c(0,0)) +
    ylab("Proportion of variance explained") +
    xlab("Principal components") +
    theme_classic() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 15))
  
  return(scree_gg)
}