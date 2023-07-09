# DESCRIPTION: Barplots showing seasonality between N. and S. Hemispheres

# Load preliminaries
#source("00_SetupGrid.R")
source("00_Preliminaries.R")
source("Utils/fxnshemisphere.R")
pacman::p_load(patchwork, purrr)
seas_list <- c("Jan-Mar", "Apr-Jun", "Jul-Sept", "Oct-Dec")

#### Calculate mean larval probabilities of hemispheres ####
full_df <- prepare_hemis_obj(seas_list)

spp_list <- spec_dict %>%  # remove species we're not interested in
  dplyr::filter(!code %in% c("LIT", "BON"))

#### Plot hemispheric seasonality across species ####
for(i in 1:nrow(spp_list)) {
  
  plot <- plot_hemis_spp(full_df, tolower(spp_list[[i, "code"]])) # create bar plot
  ggsave(plot = plot,
         filename = here::here(figure_dir, spp_list[[i, "code"]], paste(spp_list[[i, "code"]], "HemisSeas.png", sep = "_")),
         dpi = 600,
         width = 7.5,
         height = 10)
  
}
