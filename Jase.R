library(hotrstuff)
library(terra)


base_dir <- path.expand(file.path("~", "Documents", "GitHub", "LarvaDistModels", "data_input"))
volumes_dir <- file.path("/Volumes", "T9", "OMIP_outputs")

variable = "vo"

# START WITH ACCESS -    need to redownload

# htr_slice_period(indir = file.path(volumes_dir, "merged1"), #input directory
#                  outdir = file.path(volumes_dir, "sliced1"), #output directory
#                  freq = "Omon", #ocean, monthly
#                  scenario = "omip2",
#                  year_start = 1963,
#                  year_end = 1981,
#                  overwrite = FALSE
# )

## WORKS
# htr_slice_period(indir = file.path(volumes_dir, "merged2"), #input directory
#                  outdir = file.path(volumes_dir, "sliced2"), #output directory
#                  freq = "Omon", #ocean, monthly
#                  scenario = "omip2",
#                  year_start = 1963,
#                  year_end = 1981,
#                  overwrite = FALSE
# )


# Doesn't seem to do anything?
# htr_fix_calendar(indir = file.path(base_dir, "sliced2")) #will be rewritten
# htr_fix_calendar(indir = file.path(base_dir, "sliced3")) #will be rewritten
# htr_fix_calendar(indir = file.path(base_dir, "sliced4")) #will be rewritten


# 
# # January - March
# htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced2"),
#                        tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
#                        outdir = file.path(volumes_dir, "seasonal2"),
#                        months = c("01", "02", "03"), # define season (in numbered format)
#                        months_name = "jan-mar" # define season name 
# )
# 
# # April-June
# htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced2"),
#                        tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
#                        outdir = file.path(volumes_dir, "seasonal2"),
#                        months = c("04", "05", "06"), # define season (in numbered format)
#                        months_name = "apr-jun" # define season name 
# )
# 
# # July-September
# htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced2"),
#                        tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
#                        outdir = file.path(volumes_dir, "seasonal2"),
#                        months = c("07", "08", "09"), # define season (in numbered format)
#                        months_name = "jul-sep" # define season name 
# )
# 
# # October-December
# htr_seasonal_frequency(indir = file.path(volumes_dir, "sliced2"),
#                        tempdir = file.path(volumes_dir, "temporary"), # you can also have this as a folder that isn't temporary, you just need to delete the line of code below that deletes all the files in this folder. `tempdir` holds the files that selects the particular months of interest for the specific season
#                        outdir = file.path(volumes_dir, "seasonal2"),
#                        months = c("10", "11", "12"), # define season (in numbered format)
#                        months_name = "oct-dec" # define season name 
# )
# 
# system(paste0("rm ", paste0(file.path(volumes_dir, "temporary"), "/*"))) # remove some of the intermediate files to save space


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


# base_rast <- hotrstuff:::htr_make_blankRaster(
#   file.path(volumes_dir, "temporary"),
#   1
# )
# 
# cdo_code <- paste0("cdo -s -L -remapbil,", base_rast, " ", file.path(volumes_dir, "seasonal1"), " ", file.path(volumes_dir, "regridded1"))
# system(cdo_code)


htr_regrid_esm(indir = file.path(volumes_dir, "seasonal2"),
               outdir = file.path(volumes_dir, "regridded2"),
               cell_res = 1,
               layer = "annual")



