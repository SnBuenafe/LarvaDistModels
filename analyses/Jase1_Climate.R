library(hotrstuff)

base_dir <- here::here()
volumes_dir <- file.path("/Volumes", "T9", "OMIP_outputs")




# Variable vo -------------------------------------------------------------
variable <- "vo"

## Download ESMs -----------------------------------------------------------

htr_download_ESM(
  indir = file.path(volumes_dir, "wget"), # input directory
  outdir = file.path(volumes_dir, "download") # output directory
)


## Shift -------------------------------------------------------------------

htr_shift_years(indir = file.path(volumes_dir, "raw1"),
                outdir = file.path(volumes_dir, "shifted1"),
                adjust_value = 1652)



## Merge -------------------------------------------------------------------


# ACCESS Merge
htr_merge_files(indir = file.path(volumes_dir, "shifted1"), #input directory
                outdir = file.path(volumes_dir, "merged1"), #output directory
                year_start = 1956, # earliest year considered
                year_end = 1981 # latest year considered
)


# CMCC Merge
htr_merge_files(indir = file.path(volumes_dir, "raw3"), #input directory
                outdir = file.path(volumes_dir, "merged3"), #output directory
                year_start = 1956, # earliest year considered
                year_end = 1981 # latest year considered
)

# GFDL Merge
htr_merge_files(indir = file.path(volumes_dir, "raw4"), #input directory
                outdir = file.path(volumes_dir, "merged4"), #output directory
                year_start = 1956, # earliest year considered
                year_end = 1981 # latest year considered
)


## Slice -------------------------------------------------------------------


# ACCESS Slice
htr_slice_period(indir = file.path(volumes_dir, "merged1"), #input directory
                 outdir = file.path(volumes_dir, "sliced1"), #output directory
                 freq = "Omon", #ocean, monthly
                 scenario = "omip2",
                 year_start = 1963,
                 year_end = 1981,
                 overwrite = FALSE
)


htr_slice_period(indir = file.path(volumes_dir, "merged2"), #input directory
                 outdir = file.path(volumes_dir, "sliced2"), #output directory
                 freq = "Omon", #ocean, monthly
                 scenario = "omip2",
                 year_start = 1963,
                 year_end = 1981,
                 overwrite = FALSE
)

# CMCC Slice
htr_slice_period(indir = file.path(volumes_dir, "merged3"), #input directory
                 outdir = file.path(volumes_dir, "sliced3"), #output directory
                 freq = "Omon", #ocean, monthly
                 scenario = "omip2",
                 year_start = 1963,
                 year_end = 1981,
                 overwrite = FALSE
)




# GFDL Slice
htr_slice_period(indir = file.path(volumes_dir, "merged4"), #input directory
                 outdir = file.path(volumes_dir, "sliced4"), #output directory
                 freq = "Omon", #ocean, monthly
                 scenario = "omip2",
                 year_start = 1963,
                 year_end = 1981,
                 overwrite = FALSE
)


## Seasonal ----------------------------------------------------------------

# ACCESS Season
# January - March
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced1"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal1"),
                       months = c("01", "02", "03"), # define season (in numbered format)
                       months_name = "jan-mar" # define season name
)

# April-June
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced1"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal1"),
                       months = c("04", "05", "06"), # define season (in numbered format)
                       months_name = "apr-jun" # define season name
)

# July-September
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced1"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal1"),
                       months = c("07", "08", "09"), # define season (in numbered format)
                       months_name = "jul-sep" # define season name
)

# October-December
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced1"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal1"),
                       months = c("10", "11", "12"), # define season (in numbered format)
                       months_name = "oct-dec" # define season name
)

system(paste0("rm ", paste0(file.path(volumes_dir, "temporary"), "/*"))) # remove some of the intermediate files to save space



# January - March
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced2"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal2"),
                       months = c("01", "02", "03"), # define season (in numbered format)
                       months_name = "jan-mar" # define season name
)

# April-June
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced2"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal2"),
                       months = c("04", "05", "06"), # define season (in numbered format)
                       months_name = "apr-jun" # define season name
)

# July-September
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced2"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal2"),
                       months = c("07", "08", "09"), # define season (in numbered format)
                       months_name = "jul-sep" # define season name
)

# October-December
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced2"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal2"),
                       months = c("10", "11", "12"), # define season (in numbered format)
                       months_name = "oct-dec" # define season name
)

system(paste0("rm ", paste0(file.path(volumes_dir, "temporary"), "/*"))) # remove some of the intermediate files to save space


# January - March
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced3"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal3"),
                       months = c("01", "02", "03"), # define season (in numbered format)
                       months_name = "jan-mar" # define season name 
)

# April-June
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced3"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal3"),
                       months = c("04", "05", "06"), # define season (in numbered format)
                       months_name = "apr-jun" # define season name 
)

# July-September
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced3"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal3"),
                       months = c("07", "08", "09"), # define season (in numbered format)
                       months_name = "jul-sep" # define season name 
)

# October-December
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced3"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal3"),
                       months = c("10", "11", "12"), # define season (in numbered format)
                       months_name = "oct-dec" # define season name 
)



system(paste0("rm ", paste0(file.path(volumes_dir, "temporary"), "/*"))) # remove some of the intermediate files to save space


# January - March
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced4"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal4"),
                       months = c("01", "02", "03"), # define season (in numbered format)
                       months_name = "jan-mar" # define season name 
)

# April-June
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced4"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal4"),
                       months = c("04", "05", "06"), # define season (in numbered format)
                       months_name = "apr-jun" # define season name 
)

# July-September
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced4"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal4"),
                       months = c("07", "08", "09"), # define season (in numbered format)
                       months_name = "jul-sep" # define season name 
)

# October-December
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced4"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal4"),
                       months = c("10", "11", "12"), # define season (in numbered format)
                       months_name = "oct-dec" # define season name 
)

system(paste0("rm ", paste0(file.path(volumes_dir, "temporary"), "/*"))) # remove some of the intermediate files to save space





## Regrid ------------------------------------------------------------------


htr_regrid_esm(indir = file.path(volumes_dir, "seasonal1"),
               outdir = file.path(volumes_dir, "regridded1"),
               cell_res = 1,
               layer = "annual")

htr_regrid_esm(indir = file.path(volumes_dir, "seasonal2"),
               outdir = file.path(volumes_dir, "regridded2"),
               cell_res = 1,
               layer = "annual")

htr_regrid_esm(indir = file.path(volumes_dir, "seasonal3"),
               outdir = file.path(volumes_dir, "regridded3"),
               cell_res = 1,
               layer = "annual")

htr_regrid_esm(indir = file.path(volumes_dir, "seasonal4"),
               outdir = file.path(volumes_dir, "regridded4"),
               cell_res = 1,
               layer = "annual")




## Integrate levels --------------------------------------------------------


# We’re currently selecting levels up to 35. We had this conversation before 
# because I think the levels are medians of the actual max and min of the depth 
# range, but we have no way of knowing the range, I think. So not sure what 
# “max depth” is but the max selected depth should be 35m. I don’t actually 
# recall why we chose that to represent zonal and meridional surface currents…
# I think it was because that was what made sense across the ensemble members

htr_show_levels(indir = file.path(volumes_dir, "sliced1"))

htr_integrate_levels(indir = file.path(volumes_dir, "regridded1"),
                     tempdir = file.path(volumes_dir, "temporary"),
                     outdir = file.path(volumes_dir, "integrated1"),
                     min_level = 0,
                     max_level = 35,
                     domain_name = "" # Although we know it's the surface layer, for this particular analyses, we don't want to add any suffix.
)


htr_show_levels(indir = file.path(volumes_dir, "sliced3"))

htr_integrate_levels(indir = file.path(volumes_dir, "regridded3"),
                     tempdir = file.path(volumes_dir, "temporary"),
                     outdir = file.path(volumes_dir, "integrated3"),
                     min_level = 0,
                     max_level = 35,
                     domain_name = "" # Although we know it's the surface layer, for this particular analyses, we don't want to add any suffix.
)


htr_show_levels(indir = file.path(volumes_dir, "sliced4"))

htr_integrate_levels(indir = file.path(volumes_dir, "regridded4"),
                     tempdir = file.path(volumes_dir, "temporary"),
                     outdir = file.path(volumes_dir, "integrated4"),
                     min_level = 0,
                     max_level = 35,
                     domain_name = "" # Although we know it's the surface layer, for this particular analyses, we don't want to add any suffix.
)






## Create the ensembles ----------------------------------------------------



ensemble <- c("ACCESS-OM2", "ACCESS-OM2-O25", "CNRM-CM6-1", "EC-Earth3", "FGOALS-f3-L", 
              "GFDL-CM4", "MIROC6", "MRI-ESM2-0", "TaiESM1-TIMCOM2", "CMCC-CM2-HR4",
              "CMCC-CM2-SR5", "CNRM-CM6-1-HR")

variable = "vo"

# January-March
htr_create_ensemble(indir = file.path(base_dir, "data_input", "data", "proc", "integrated", "omip", variable), #input directory
                    outdir = file.path(base_dir, "climate_ensemble"), #output directory
                    model_list = ensemble, #list of models for ensemble
                    variable = variable, #variable name
                    freq = "Omon", #original frequency of data
                    scenario = "omip2", #emission scenario
                    season = "jan-mar",
                    mean = TRUE # if false, takes the median
)

# April-June
htr_create_ensemble(indir = file.path(base_dir, "data_input", "data", "proc", "integrated", "omip", variable), #input directory
                    outdir = file.path(base_dir, "climate_ensemble"), #output directory
                    model_list = ensemble, #list of models for ensemble
                    variable = variable, #variable name
                    freq = "Omon", #original frequency of data
                    scenario = "omip2", #emission scenario
                    season = "apr-jun",
                    mean = TRUE # if false, takes the median
)

# July-September
htr_create_ensemble(indir = file.path(base_dir, "data_input", "data", "proc", "integrated", "omip", variable), #input directory
                    outdir = file.path(base_dir, "climate_ensemble"), #output directory
                    model_list = ensemble, #list of models for ensemble
                    variable = variable, #variable name
                    freq = "Omon", #original frequency of data
                    scenario = "omip2", #emission scenario
                    season = "jul-sep",
                    mean = TRUE # if false, takes the median
)

# October-December
htr_create_ensemble(indir = file.path(base_dir, "data_input", "data", "proc", "integrated", "omip", variable), #input directory
                    outdir = file.path(base_dir, "climate_ensemble"), #output directory
                    model_list = ensemble, #list of models for ensemble
                    variable = variable, #variable name
                    freq = "Omon", #original frequency of data
                    scenario = "omip2", #emission scenario
                    season = "oct-dec",
                    mean = TRUE # if false, takes the median
)


















# Variable uo -------------------------------------------------------------

variable <- "uo"

## Download ESMs -----------------------------------------------------------

htr_download_ESM(
  indir = file.path(volumes_dir, "wget"), # input directory
  outdir = file.path(volumes_dir, "download") # output directory
)


## Merge -------------------------------------------------------------------


# CMCC-CM2-HR4 Merge
htr_merge_files(indir = file.path(volumes_dir, "raw5"), #input directory
                outdir = file.path(volumes_dir, "merged5"), #output directory
                year_start = 1956, # earliest year considered
                year_end = 1981 # latest year considered
)


# CMCC-CM2-SR5 Merge
htr_merge_files(indir = file.path(volumes_dir, "raw6"), #input directory
                outdir = file.path(volumes_dir, "merged6"), #output directory
                year_start = 1956, # earliest year considered
                year_end = 1981 # latest year considered
)

# CNRM Merge
htr_merge_files(indir = file.path(volumes_dir, "raw7"), #input directory
                outdir = file.path(volumes_dir, "merged7"), #output directory
                year_start = 1956, # earliest year considered
                year_end = 1981 # latest year considered
)


## Slice -------------------------------------------------------------------


# ACCESS Slice
htr_slice_period(indir = file.path(volumes_dir, "merged5"), #input directory
                 outdir = file.path(volumes_dir, "sliced5"), #output directory
                 freq = "Omon", #ocean, monthly
                 scenario = "omip2",
                 year_start = 1963,
                 year_end = 1981,
                 overwrite = FALSE
)


htr_slice_period(indir = file.path(volumes_dir, "merged6"), #input directory
                 outdir = file.path(volumes_dir, "sliced6"), #output directory
                 freq = "Omon", #ocean, monthly
                 scenario = "omip2",
                 year_start = 1963,
                 year_end = 1981,
                 overwrite = FALSE
)

# CNRM Slice
htr_slice_period(indir = file.path(volumes_dir, "merged7"), #input directory
                 outdir = file.path(volumes_dir, "sliced7"), #output directory
                 freq = "Omon", #ocean, monthly
                 scenario = "omip2",
                 year_start = 1963,
                 year_end = 1981,
                 overwrite = FALSE
)




## Seasonal ----------------------------------------------------------------

# January - March
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced5"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal5"),
                       months = c("01", "02", "03"), # define season (in numbered format)
                       months_name = "jan-mar" # define season name
)

# April-June
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced5"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal5"),
                       months = c("04", "05", "06"), # define season (in numbered format)
                       months_name = "apr-jun" # define season name
)

# July-September
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced5"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal5"),
                       months = c("07", "08", "09"), # define season (in numbered format)
                       months_name = "jul-sep" # define season name
)

# October-December
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced5"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal5"),
                       months = c("10", "11", "12"), # define season (in numbered format)
                       months_name = "oct-dec" # define season name
)

system(paste0("rm ", paste0(file.path(volumes_dir, "temporary"), "/*"))) # remove some of the intermediate files to save space



# January - March
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced6"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal6"),
                       months = c("01", "02", "03"), # define season (in numbered format)
                       months_name = "jan-mar" # define season name
)

# April-June
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced6"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal6"),
                       months = c("04", "05", "06"), # define season (in numbered format)
                       months_name = "apr-jun" # define season name
)

# July-September
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced6"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal6"),
                       months = c("07", "08", "09"), # define season (in numbered format)
                       months_name = "jul-sep" # define season name
)

# October-December
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced6"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal6"),
                       months = c("10", "11", "12"), # define season (in numbered format)
                       months_name = "oct-dec" # define season name
)

system(paste0("rm ", paste0(file.path(volumes_dir, "temporary"), "/*"))) # remove some of the intermediate files to save space


# January - March
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced7"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal7"),
                       months = c("01", "02", "03"), # define season (in numbered format)
                       months_name = "jan-mar" # define season name 
)

# April-June
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced7"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal7"),
                       months = c("04", "05", "06"), # define season (in numbered format)
                       months_name = "apr-jun" # define season name 
)

# July-September
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced7"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal7"),
                       months = c("07", "08", "09"), # define season (in numbered format)
                       months_name = "jul-sep" # define season name 
)

# October-December
htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced7"),
                       tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
                       outdir = file.path(volumes_dir, "seasonal7"),
                       months = c("10", "11", "12"), # define season (in numbered format)
                       months_name = "oct-dec" # define season name 
)



system(paste0("rm ", paste0(file.path(volumes_dir, "temporary"), "/*"))) # remove some of the intermediate files to save space






## Regrid ------------------------------------------------------------------


htr_regrid_esm(indir = file.path(volumes_dir, "seasonal5"),
               outdir = file.path(volumes_dir, "regridded5"),
               cell_res = 1,
               layer = "annual")

htr_regrid_esm(indir = file.path(volumes_dir, "seasonal6"),
               outdir = file.path(volumes_dir, "regridded6"),
               cell_res = 1,
               layer = "annual")

htr_regrid_esm(indir = file.path(volumes_dir, "seasonal7"),
               outdir = file.path(volumes_dir, "regridded7"),
               cell_res = 1,
               layer = "annual")





## Integrate levels --------------------------------------------------------


# We’re currently selecting levels up to 35. We had this conversation before 
# because I think the levels are medians of the actual max and min of the depth 
# range, but we have no way of knowing the range, I think. So not sure what 
# “max depth” is but the max selected depth should be 35m. I don’t actually 
# recall why we chose that to represent zonal and meridional surface currents…
# I think it was because that was what made sense across the ensemble members

htr_show_levels(indir = file.path(volumes_dir, "sliced5"))

htr_integrate_levels(indir = file.path(volumes_dir, "regridded5"),
                     tempdir = file.path(volumes_dir, "temporary"),
                     outdir = file.path(volumes_dir, "integrated5"),
                     min_level = 0,
                     max_level = 35,
                     domain_name = "" # Although we know it's the surface layer, for this particular analyses, we don't want to add any suffix.
)


htr_show_levels(indir = file.path(volumes_dir, "sliced6"))

htr_integrate_levels(indir = file.path(volumes_dir, "regridded6"),
                     tempdir = file.path(volumes_dir, "temporary"),
                     outdir = file.path(volumes_dir, "integrated6"),
                     min_level = 0,
                     max_level = 35,
                     domain_name = "" # Although we know it's the surface layer, for this particular analyses, we don't want to add any suffix.
)


htr_show_levels(indir = file.path(volumes_dir, "sliced7"))

htr_integrate_levels(indir = file.path(volumes_dir, "regridded7"),
                     tempdir = file.path(volumes_dir, "temporary"),
                     outdir = file.path(volumes_dir, "integrated7"),
                     min_level = 0,
                     max_level = 35,
                     domain_name = "" # Although we know it's the surface layer, for this particular analyses, we don't want to add any suffix.
)



## Create the ensembles ----------------------------------------------------


ensemble <- c("ACCESS-OM2", "ACCESS-OM2-O25", "CNRM-CM6-1", "EC-Earth3", "FGOALS-f3-L", 
              "GFDL-CM4", "MIROC6", "MRI-ESM2-0", "TaiESM1-TIMCOM2", "CMCC-CM2-HR4",
              "CMCC-CM2-SR5", "CNRM-CM6-1-HR")

# January-March
htr_create_ensemble(indir = file.path(base_dir, "data_input", "data", "proc", "integrated", "omip", variable), #input directory
                    outdir = file.path(base_dir, "climate_ensemble"), #output directory
                    model_list = ensemble, #list of models for ensemble
                    variable = variable, #variable name
                    freq = "Omon", #original frequency of data
                    scenario = "omip2", #emission scenario
                    season = "jan-mar",
                    mean = TRUE # if false, takes the median
)

# April-June
htr_create_ensemble(indir = file.path(base_dir, "data_input", "data", "proc", "integrated", "omip", variable), #input directory
                    outdir = file.path(base_dir, "climate_ensemble"), #output directory
                    model_list = ensemble, #list of models for ensemble
                    variable = variable, #variable name
                    freq = "Omon", #original frequency of data
                    scenario = "omip2", #emission scenario
                    season = "apr-jun",
                    mean = TRUE # if false, takes the median
)

# July-September
htr_create_ensemble(indir = file.path(base_dir, "data_input", "data", "proc", "integrated", "omip", variable), #input directory
                    outdir = file.path(base_dir, "climate_ensemble"), #output directory
                    model_list = ensemble, #list of models for ensemble
                    variable = variable, #variable name
                    freq = "Omon", #original frequency of data
                    scenario = "omip2", #emission scenario
                    season = "jul-sep",
                    mean = TRUE # if false, takes the median
)

# October-December
htr_create_ensemble(indir = file.path(base_dir, "data_input", "data", "proc", "integrated", "omip", variable), #input directory
                    outdir = file.path(base_dir, "climate_ensemble"), #output directory
                    model_list = ensemble, #list of models for ensemble
                    variable = variable, #variable name
                    freq = "Omon", #original frequency of data
                    scenario = "omip2", #emission scenario
                    season = "oct-dec",
                    mean = TRUE # if false, takes the median
)

