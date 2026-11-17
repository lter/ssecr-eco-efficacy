rm(list = ls()); gc()
# load libraries
library(devtools)
library(neonUtilities)
library(geoNEON)
library(tidyverse)
library(dplyr)
library(lubridate)
# install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
# read token from a local file, see https://www.neonscience.org/resources/learning-hub/tutorials/neon-api-tokens-tutorial Option 1
source(paste0(file.path("neon_token_source.R")))
# read in sites from Alex's list
site_info <- read_csv(file.path("woody_encroachment_neon_data_explore.csv"))


assess_frequency <- function (
  df = NULL) {
  fn <- deparse(substitute(df)) # get file name 
  df <- as.data.frame(df) 
  # find the date column: "date" first, then "endDate"
  if ("date" %in% names(df)) {
    date_col <- "date"
  } else if ("endDate" %in% names(df)) {
    date_col <- "endDate"
  } else if ("collectDate" %in% names(df)) {
    date_col <- "collectDate"
  } else {
    stop("No 'date' or 'endDate' or 'collectDate' column found in ", fn, call. = FALSE)
  }
  df$date <- as.Date(df[[date_col]])   # standardize to "date"
  df$year <- year(df$date)
# one row per site per date
time_df <- df %>% distinct(siteID, date)
# summarize sampling frequency per year per site
sum_df <- time_df %>%
  mutate(years = year(date)) %>%
  count(siteID, years, name = "n_sampling_dates")
write.csv(sum_df, paste0("sum_", fn, ".csv"), row.names = FALSE)
}


# download vegetation structure data, this includes non-herbaceous perennial vegetation structure structure data DP1.10045.001
vst <- loadByProduct(
dpID='DP1.10098.001',
check.size=F, 
site = site_info$Site.ID,
package='basic',
token=NEON_TOKEN)
# turn all tables in the list to dataframe in the global environment
# the name of table = name of DF
list2env(vst, envir=.GlobalEnv)
# from NEON website: data in the vst_perplotperyear table describe the presence/absence and sampling area of woody and non-woody growth forms.
assess_frequency(df= vst_perplotperyear)
# from NEON website: data provided in the vst_shrubgroup table allow calculation of live and dead volume per taxonID within each shrub group
# and records may be linked with the vst_perplotperyear table via the plotID and eventID fields.
assess_frequency(df= vst_shrubgroup)
# from NEON website: the vst_non-woody table contains one record per individualID per eventID, 
# and contains growth form, structure and status data that may be linked to vst_perplotperyear data via the plotID and eventID fields.
assess_frequency(df= `vst_non-woody`)
# from NEON website: the vst_apparentindividual table contains at least one record per individualID per eventID, 
# and includes growth form, structure and plant status data that may be linked to vst_mappingandtagging records via the individualID; 
# records may also be linked to vst_perplotperyear via the plotID and eventID fields 
# in order to generate plot-level estimates of biomass and productivity. 
assess_frequency(df= vst_apparentindividual)


# download plant presence and percent cover data
# this data product contains the quality-controlled, native sampling resolution data from NEON's plant diversity sampling. 
div <- loadByProduct(
dpID='DP1.10058.001',
check.size=F, 
site = site_info$Site.ID,
package='basic',
token=NEON_TOKEN)
# turn all tables in the list to dataframe in the global environment
# the name of table = name of DF
list2env(div, envir=.GlobalEnv)
assess_frequency(df= div_1m2Data)
assess_frequency(df= div_10m2Data100m2Data)


# download root biomass and chemistry, Megapit data
# this data product contains the quality-controlled, native sampling resolution data from NEON's belowground plant biomass stock 
# and chemistry measurements from Megapits.  
mpr <- loadByProduct(
dpID='DP1.10066.001',
check.size=F, 
site = site_info$Site.ID,
package='basic',
token=NEON_TOKEN)
# turn all tables in the list to dataframe in the global environment
# the name of table = name of DF
list2env(mpr, envir=.GlobalEnv)
# from NEON website: the protocol dictates that up to 12 records per soil Megapit are created in the mpr_perPitProfile table, 
# corresponding to 3 vertical profiles, 2 root size classes, and 2 root statuses (live/dead). 
# Each profile in mpr_perPitProfile can have up to 15 child records in mpr_perDepthIncrement, 
# one for each of up to 15 depth increments per vertical profile. 
# Each record in mpr_perDepthIncrement creates up to 4 child records in mpr_perRootSample, 
# one record per root size class per root status. 
# Each record from mpr_perRootSample with sufficient sample mass is expected to appear up to two times 
# (if analytical replicates were conducted) in mpr_carbonNitrogen
assess_frequency(df= mpr_perpitprofile)


# download root biomass and chemistry, periodic data
# this data product contains the quality-controlled, native sampling resolution data from NEON's belowground biomass protocol.  
bbc <- loadByProduct(
dpID='DP1.10067.001',
check.size=F, 
site = site_info$Site.ID,
package='basic',
token=NEON_TOKEN)
# turn all tables in the list to dataframe in the global environment
# the name of table = name of DF
list2env(bbc, envir=.GlobalEnv)
assess_frequency(df= bbc_percore)