# Historical hotspots of pelagic fish larvae in the Indian and Pacific Oceans.
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8271292.svg)](https://doi.org/10.5281/zenodo.8271292)

This repository contains all the pertinent code and analysis of Buenafe et al. (submitted).

## DESCRIPTION
This project aims to:

1. Describe historical larval distributions in the Indian and Pacific Oceans of 15 taxa from the boosted regression tree models built using digitized presence/absence Nishikawa et al. (1985) data [(Buenafe et al., 2022)](https://doi.org/10.1038/s41597-022-01528-7) and historical environmental predictors (see __Data__ below).
2. Delineate potential drivers of larval distribution and hemispheric seasonality across these taxa.
3. Identify larval hotspots that could correspond to potential spawning grounds for these species.


## WORKFLOW
The scripts are named sequentially. To rerun __all__ the analyses, the user would have to go through all the scripts within the `/analyses/` folder sequentially. This requires downloading all necessary data from their original sources (see __Data__ section below) and placing them in their respective directories. However, the repository already hosts the subset of data that is sufficient to run the analyses without downloading additional data.


### STRUCTURE OF THE CODE
- `01_climate_data/`: processes climate model outputs from the Ocean Model Intercomparison Project Phase 2 (OMIP2) to prepare data for analysis and visualization

  To reproduce the climatology data, download Earth System Model outputs (see "Data" section below) and run the code in the markdown OMIP_runs.qmd.

- `02_preliminaries/`: contains the preliminary scripts (prefixed with `00_`), which are called within the subsequent scripts, therefore there is no need to run them independently.

- `03_assemble_predictors/` to `06_hotspot_analyses/`:

  - `01_`: assembles all the predictors and creates seasonal data sets with the larval data
  
  To reproduce the distribution of the 15 taxa in `01p_DataLayers_AquaMaps.R)`, the user would have to download their distribution from AquaMaps (see __Data__ below).

  - `02_` through `16_`: generate models for all 15 taxa (all found in `/analyses/04_models/`). Scripts prefixed with `a_` refer to assembling the necessary data to run the models. `b_` scripts are where the full model is built. `c_` scripts are where the model outputs are restricted to areas where confidence is higher. 
  
  Note that `b_` scripts take a significant amount of time to run, therefore the user can run `c_` scripts using existing files generated from `b_` scripts that are found in the repository. 

  To redo building the BRTs, make sure that the larval data (in `.rds` format) from [(Buenafe et al., 2022)](https://doi.org/10.1038/s41597-022-01528-7) is in `data_input/fish`. Please also make sure that the `crs` for these files are in `+proj=longlat +lon_0=180 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0` (see lines 14-15 of `00_SetupGrid.R`). The files in this repository are reprojected files of [(Buenafe et al., 2022)](https://doi.org/10.1038/s41597-022-01528-7).

  Models are found in `data_output/models/` and model predictions for each taxa are found in `data_output/predictions/`

  - `17_`: assembling model outputs across taxa and saving them as rasters, which can be accessed in `data_output/final_raster`

  - `18_`: plotting hemispheric seasonality

  - `19_`: plotting predictor preferences across taxa

  - `20_`: calculating spatial aggregation index and seasonality index

  - `21_`: extracting model parameters

  - `22_`: principal component analysis to determine hotspots

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

The historical environmental predictors were prepared from the OMIP2 Earth System Models [(Tsujino et al., 2020)](https://doi.org/10.5194/gmd-13-3643-2020). 

We used (in parentheses are the OMIP2 codes for the climate variables): 

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

The ensembles used for each of the variables are subsets of the set of models found below. 

__Table 1.__ Set of models used.
| Model    | Reference |
| -------- | ------- |
| ACCESS-OM2 | [Hayashida et al. (2021)](https://doi.org/10.22033/ESGF/CMIP6.14689) |
| ACCESS-OM2-025 | [Holmes et al. (2021)](https://doi.org/10.22033/ESGF/CMIP6.14690) |
| CESM2 | [Danabasoglu et al. (2019)](https://doi.org/10.22033/ESGF/CMIP6.7680) |
| CMCC-CM2-HR4 | [Fogli et al. (2020)](https://doi.org/10.22033/ESGF/CMIP6.13235) |
| CMCC-CM2-SR5 | [Fogli et al. (2020)](https://doi.org/10.22033/ESGF/CMIP6.13236) |
| CNRM-CM6-1 | [Voldoire (2020)](https://doi.org/10.22033/ESGF/CMIP6.10345) |
| CNRM-CM6-1-HR | [Voldoire (2021)](https://doi.org/10.22033/ESGF/CMIP6.10346) |
| EC-Earth3 | [Consortium (EC-Earth; 2020)](https://doi.org/10.22033/ESGF/CMIP6.14718) |
| FGOALS-f3-L| [Lin (2019)](https://doi.org/10.22033/ESGF/CMIP6.3419) |
| GFDL-CM4 | [Hurlin et al. (2018)](https://doi.org/10.22033/ESGF/CMIP6.8626) |
| MIROC6 | [Komuro (2019)](https://doi.org/10.22033/ESGF/CMIP6.5655) |
| MRI-ESM2.0 | [Yukimoto et al. (2019)](https://doi.org/10.22033/ESGF/CMIP6.6842) |
| NorESM2-LM | [Bentsen et al. (2019)](https://doi.org/10.22033/ESGF/CMIP6.8089) |
| TaiESM1-TIMCOM2 | [Tseng et al. (2021)](https://doi.org/10.22033/ESGF/CMIP6.16336) |

The mean depth was calculated using [The General Bathymetric Chart of the Oceans](https://www.gebco.net/data_and_products/gridded_bathymetry_data/).

The AquaMaps data can be accessed in [AquaMaps](https://www.aquamaps.org/).
