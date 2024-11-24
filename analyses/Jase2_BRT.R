
# Hi Jase! Sorry a thought just occurred to me. If you do end up getting the 
# different models working for vo, you’d need to rerun the Meridional Velocity 
# R script as well as the EKE to update the predictor layers

source("analyses/03_assemble_predictors/01k_Assembling_MeridionalVelocity.R")

source("analyses/03_assemble_predictors/01j_Assembling_ZonalVelocity.R")

source("analyses/03_assemble_predictors/01n_Assembling_EddyKineticEnergy.R")

source("analyses/03_assemble_predictors/01q_DataLayers_Assembling.R")

source("analyses/03_assemble_predictors/01r_NumberSamplingPoints.R")



# Run BRTs ----------------------------------------------------------------

files1 <- c(
  "analyses/04_models/02b_YFT_ModelBuilding.R",
  "analyses/04_models/03b_SKP_ModelBuilding.R",
  "analyses/04_models/04b_ALB_ModelBuilding.R",
  "analyses/04_models/05b_SWO_ModelBuilding.R",
  "analyses/04_models/06b_BLUM_ModelBuilding.R",
  "analyses/04_models/07b_FRI_ModelBuilding.R",
  "analyses/04_models/08b_BET_ModelBuilding.R",
  "analyses/04_models/09b_BFT_ModelBuilding.R",
  "analyses/04_models/10b_SAU_ModelBuilding.R",
  "analyses/04_models/11b_SAIL_ModelBuilding.R",
  "analyses/04_models/12b_SBFT_ModelBuilding.R",
  "analyses/04_models/13b_SLT_ModelBuilding.R",
  "analyses/04_models/14b_SHOS_ModelBuilding.R",
  "analyses/04_models/15b_STRM_ModelBuilding.R",
  "analyses/04_models/16b_LESC_ModelBuilding.R"
)

files2 <- c(
  "analyses/04_models/02c_YFT_IncreasingConfidence.R",
  "analyses/04_models/03c_SKP_IncreasingConfidence.R",
  "analyses/04_models/04c_ALB_IncreasingConfidence.R",
  "analyses/04_models/05c_SWO_IncreasingConfidence.R",
  "analyses/04_models/06c_BLUM_IncreasingConfidence.R",
  "analyses/04_models/07c_FRI_IncreasingConfidence.R",
  "analyses/04_models/08c_BET_IncreasingConfidence.R",
  "analyses/04_models/09c_BFT_IncreasingConfidence.R",
  "analyses/04_models/10c_SAU_IncreasingConfidence.R",
  "analyses/04_models/11c_SAIL_IncreasingConfidence.R",
  "analyses/04_models/12c_SBFT_IncreasingConfidence.R",
  "analyses/04_models/13c_SLT_IncreasingConfidence.R",
  "analyses/04_models/14c_SHOS_IncreasingConfidence.R",
  "analyses/04_models/15c_STRM_IncreasingConfidence.R",
  "analyses/04_models/16c_LESC_IncreasingConfidence.R"
)

# Setup parallel processing
ncore <- parallelly::availableCores(omit = 1)
future::plan(future::multisession(), workers = ncore)

# Run models
furrr::future_walk(files1, source)

# Increase confidence
furrr::future_walk(files2, source)

## Explicitly close multisession workers by switching plan
future::plan(future::sequential)



# Hotspot Analysis --------------------------------------------------------


source("analyses/04_models/17_FinalizeModels.R")

source("analyses/05_hotspot_analyses/18a_PCA_Jan-Mar.R")

source("analyses/05_hotspot_analyses/18b_PCA_Apr-Jun.R")

source("analyses/05_hotspot_analyses/18c_PCA_Jul-Sept.R")

source("analyses/05_hotspot_analyses/18d_PCA_Oct-Dec.R")

source("analyses/05_hotspot_analyses/19_HemisphereSeasonality.R")

source("analyses/05_hotspot_analyses/20_SummaryHotspots.R")

# source("analyses/05_hotspot_analyses/21_SummaryPredictors.R") # Can't run because the data is not on GitHub

source("analyses/05_hotspot_analyses/22_DispersionIndex.R")
