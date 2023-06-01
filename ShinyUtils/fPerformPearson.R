fPerformPearson <- function(df,
                            axis,
                            spp) {
  mat <- df %>% 
    dplyr::select(axis, all_of(tolower(spp))) %>% 
    base::as.matrix()
  
  res <- (Hmisc::rcorr(mat))$r
  
  res <- res[1, order(res[1,], decreasing = TRUE)] %>%   # arrange columns according to their r values
    as.matrix() %>% 
    t()
  
  colnames(res) <- c("PCA 1", 
                     spec_dict[match(spp, spec_dict$code), "common"] %>% 
                       pull())
  
  rownames(res) <- ""
  
  corrplot::corrplot(res,
                     # type = "upper",
                     order = "original",
                     tl.col = "black",
                     # addCoef.col = "black",
                     tl.srt = 45,
                     insig = "blank", # make r values that have p values < 0.05 blank
                     col = corrplot::COL2('BrBG', 200),
                     diag = TRUE,
                     col.lim = c(-1, 1), # dictate limits of colors
                     # addgrid.col = NA # remove the grid
  )
  
}