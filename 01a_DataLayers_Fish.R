# Load preliminaries
source("00_Utils.R")

# Assemble yellowfin tuna data
YFT <- combineFish(species = "yellowfin-tuna")

####################
# Predictor data #
####################
# Climate

# Temperature
tos <- nc2sf(model = "ACCESS-ESM1-5",
             expt = "ssp585",
             var = "tos") %>% 
  dplyr::rename(tos = mean) %>% 
  tibble::as_tibble()

# Oxygen
o2os <- nc2sf(model = "ACCESS-ESM1-5",
              expt = "ssp585",
              var = "o2os") %>% 
  dplyr::rename(o2os = mean) %>% 
  tibble::as_tibble()

# pH
phos <- nc2sf(model = "CMCC-ESM2",
              expt = "ssp585",
              var = "phos") %>% 
  dplyr::rename(phos = mean) %>% 
  tibble::as_tibble()

# Bathymetry
bathy <- gebcoConvert()


inPath <- "Data/"

# assemble data
YFTsf1 <- readRDS(paste0(inPath, "Fish/", "VectorFile_", "yellowfin-tuna", "_jan-mar.rds")) %>% 
  tibble::as_tibble()
YFTsf2 <- readRDS(paste0(inPath, "Fish/", "VectorFile_", "yellowfin-tuna", "_apr-jun.rds")) %>% 
  tibble::as_tibble()
YFTsf3 <- readRDS(paste0(inPath, "Fish/", "VectorFile_", "yellowfin-tuna", "_jul-sept.rds")) %>% 
  tibble::as_tibble()
YFTsf4 <- readRDS(paste0(inPath, "Fish/", "VectorFile_", "yellowfin-tuna", "_oct-dec.rds")) %>% 
  tibble::as_tibble()


# join all data.frames
YFT <- dplyr::bind_rows(YFTsf1, YFTsf2, YFTsf3, YFTsf4) %>% 
  dplyr::mutate(across(where(is.character), ~factor(.)),
                abundance = ifelse(abundance > 0, yes = 1, no = 0)) %>% 
  dplyr::select(species, abundance, everything()) #%>% 
#  as.data.frame()
YFTsf <- YFT %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  sf::st_centroid()