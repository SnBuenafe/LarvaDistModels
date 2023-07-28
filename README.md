# Historical hotspots of pelagic fish larvae in the Indian and Pacific Oceans.
This repository contains all the pertinent code and analysis of the paper (see Buenafe et al., submitted for the preprint).

## DESCRIPTION
This project aims to:

1. Describe historical larval distributions in the Indian and Pacific Oceans of 15 taxa from the boosted regression tree models built using digitized presence/absence Nishikawa et al. (1985) data [(Buenafe et al., 2022)](https://doi.org/10.1038/s41597-022-01528-7) and historical environmental predictors (see __Data__ below).
2. Delineate potential drivers of larval distribution and hemispheric seasonality across these taxa.
3. Identify larval hotspots that could correspond to potential spawning grounds for these species.

The repository hosts the subset of data used to generate the boosted regression tree models (in `Data/`), but the raw and complete data can be extracted from their original sources described in __Data__ below.

## WORKFLOW
The scripts are named sequentially. To rerun all the analyses, the user would have to go through all the scripts starting from scripts prefixed with `01_`. Note that scripts prefixed with `00_` are preliminary scripts and are called within the subsequent scripts. Therefore, there is no need to run them independently.

### Summary of the code
- `01_`: assembles all the predictors and creates seasonal data sets with the larval data
- `02_` through `16_`: generate models for all 15 taxa. Scripts prefixed with `a_` refer to assembling the necessary data to run the models. `b_` scripts are where the full model is built. `c_` scripts are where the model outputs are restricted to areas where confidence is higher. Models are found in `Output/Models/` and model predictions for each taxa are found in `Output/Predictions/`
- `17_`: assembling model outputs across taxa and saving them as rasters, which can be accessed in `Output/FinalRaster`
- `18_`: Principal Component Analysis to determine hotspots
- `19_`: plotting hemispheric seasonality
- `20_`: generating seasonal taxa richness maps
- `21_`: plotting model predictions vs predictors

## DATA
The digitized larval data are found in [(Buenafe et al., 2022)](https://doi.org/10.1038/s41597-022-01528-7). The following species were included in this study:

1. Yellowfin tuna
2. Skipjack tuna
3. Albacore
4. Swordfish
5. Blue marlin
6. Frigate tuna
7. Bigeye tuna
8. Pacific bluefin tuna
9. Sauries
10. Sailfish
11. Southern bluefin tuna
12. Slender tuna
13. Shortbill spearfish
14. Striped marlin
15. Longfin escolar

The historical environmental predictors were prepared from Coupled Model Intercomparison Project 6 (CMIP6) Earth System Models (https://esgf-node.llnl.gov/search/cmip6/). The ensembles used for each of the variables are subsets of the set of models found below. 

We used (in parentheses are the CMIP6 codes for the climate variables): 

1. temperature (tos)
2. oxygen (o2os)
3. pH (phos)
4. chlorophyll-a (chlos)
5. salinity (sos)
6. mixed layer thickness (mlotst)
7. nitrate (no3os)
8. phosphate (po4os)
9. ammonium (nh4os)
10. zonal velocity (uo)
11. meridional velocity (vo)

Set of models used.
| Model    | Reference |
| -------- | ------- |
| ACCESS-ESM1-5 | [Ziehn et al. (2019)](http://doi.org/10.22033/ESGF/CMIP6.4272) |
| BCC-CSM2-MR | [Wu et al. (2018)](http://doi.org/10.22033/ESGF/CMIP6.2948) |
| CMCC-CM2-SR5 | [Lovato et al. (2020)](http://doi.org/10.22033/ESGF/CMIP6.3825) |
| CMCC-ESM2 | [Lovato et al. (2021)](http://doi.org/10.22033/ESGF/CMIP6.13195) |
| FGOALS-f3-L | [Yu (2019)](http://doi.org/10.22033/ESGF/CMIP6.3355) |
| FGOALS-g3 | [Li (2019)](http://doi.org/10.22033/ESGF/CMIP6.3356) |
| GFDL-CM4 | [Guo et al. (2018)](http://doi.org/10.22033/ESGF/CMIP6.8594) |
| GFDL-ESM4 | [Krasting et al. (2018)](http://doi.org/10.22033/ESGF/CMIP6.8597) |
| GISS-E2-1-G | [NASA Goddard Institute for Space Studies (2018)](http://doi.org/10.22033/ESGF/CMIP6.7127) |
| GISS-E2-1-H | [NASA Goddard Institute for Space Studies (2018)](http://doi.org/10.22033/ESGF/CMIP6.7128) |
| IPSL-CM5A2-INCA | [Boucher et al. (2020)](http://doi.org/10.22033/ESGF/CMIP6.13661) |
| MCM-UA-1-0 | [Stouffer (2019)](http://doi.org/10.22033/ESGF/CMIP6.8888) |
| MIROC-ES2L | [Hajima et al. (2019)](http://doi.org/10.22033/ESGF/CMIP6.5602) |
| MIROC6 | [Tatebe & Watanabe (2018)](http://doi.org/10.22033/ESGF/CMIP6.5603) |
| MPI-ESM1-2-HR | [Jungclaus et al. (2019)](http://doi.org/10.22033/ESGF/CMIP6.6594) |
| MRI-ESM2-0 | [Yukimoto et al. (2019)](http://doi.org/10.22033/ESGF/CMIP6.6842) |
| NorESM2-LM | [Seland et al. (2019)](http://doi.org/10.22033/ESGF/CMIP6.8036) |

The mean depth was calculated using [The General Bathymetric Chart of the Oceans](https://www.gebco.net/data_and_products/gridded_bathymetry_data/).