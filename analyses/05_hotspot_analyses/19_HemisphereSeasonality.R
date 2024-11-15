# DESCRIPTION: Barplots showing seasonality between N. and S. Hemispheres

# Load preliminaries
#source(file.path("analyses", "02_preliminaries", "00_SetupGrid.R"))
source(file.path("analyses", "02_preliminaries", "00_Preliminaries.R"))

# source("Utils/fxnshemisphere.R")

pacman::p_load(patchwork, purrr)
seas_list <- c("Jan-Mar", "Apr-Jun", "Jul-Sept", "Oct-Dec")

#### Calculate mean larval probabilities of hemispheres ####
full_df <- prepare_hemis_obj(seas_list)

spp_list <- spec_dict %>%  # remove species we're not interested in
  dplyr::filter(!code %in% c("LIT", "BON")) %>% 
  dplyr::filter(!code %in% "SAU") # Temporarily removed while we sort out the BRT

#### Plot hemispheric seasonality across species ####
for(i in 1:nrow(spp_list)) {
  
  if(spp_list$code[i] %in% c("SAIL", "LESC", "SAU")) {
    plot <- plot_hemis_spp(full_df, tolower(spp_list[[i, "code"]]))
    
    ggsave(plot = plot,
           filename = here::here(figure_dir, spp_list[[i, "code"]], paste(spp_list[[i, "code"]], "HemisSeas.png", sep = "_")),
           dpi = 600,
           width = 8,
           height = 12)
  } else {
    plot <- plot_hemis_spp(full_df, tolower(spp_list[[i, "code"]])) + # create bar plot
      theme(axis.text.x = element_blank())
    
    ggsave(plot = plot,
           filename = here::here(figure_dir, spp_list[[i, "code"]], paste(spp_list[[i, "code"]], "HemisSeas.png", sep = "_")),
           dpi = 600,
           width = 8,
           height = 10)
  }
  
  
}
dummy_df <- tibble::tribble(~hemisphere, ~`spp_jan-mar`, ~`spp_apr-jun`, ~`spp_jul-sept`, ~`spp_oct-dec`,
                            "North", 0.5, 0.5, 0.5, 0.5,
                            "South", 0.5, 0.5, 0.5, 0.5)
plot <- plot_hemis_spp(dummy_df, "spp") +
  scale_y_continuous(limits = c(-1, 1),
                     label = make_lat_lon_label)
ggsave(plot = plot,
       filename = here::here(figure_dir, "dummy_HemisSeas.png"),
       dpi = 600,
       width = 8,
       height = 12)
