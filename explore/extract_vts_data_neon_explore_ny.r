rm(list = ls()); gc()
# load libraries
library(devtools)
library(neonUtilities)
library(geoNEON)
library(tidyverse)
library(dplyr)
library(lubridate)
# install_github('NEONScience/NEON-geolocation/geoNEON', dependencies=TRUE)
# read token
source(paste0(file.path("neon_token_source.R")))
# read in sites from Alex's list
site_info <- read_csv(file.path("woody_encroachment_neon_data_explore.csv"))
# download vegetation structure data
vst <- loadByProduct(
dpID='DP1.10098.001',
check.size=F, 
site = site_info$Site.ID,
package='basic',
token=NEON_TOKEN)
# turn all tables in the list to dataframe in the global environment
# the name of table = name of DF
list2env(vst, envir=.GlobalEnv)

# glimpse(vst_perplotperyear)
# from NEON website: data in the vst_perplotperyear table describe the presence/absence and sampling area of woody and non-woody growth forms.
vst_perplotperyear$date <- as.Date(vst_perplotperyear$date)
vst_perplotperyear$year <- year(vst_perplotperyear$date)
# one row per site per date
sum_vst_perplotperyear <- vst_perplotperyear %>% distinct(siteID, date)
# summarize sampling frequency per year per site
with(sum_vst_perplotperyear, table(siteID, year(date)))

# glimpse(vst_shrubgroup)
# from NEON website: data provided in the vst_shrubgroup table allow calculation of live and dead volume per taxonID within each shrub group
# and records may be linked with the vst_perplotperyear table via the plotID and eventID fields.
vst_shrubgroup$date <- as.Date(vst_shrubgroup$date)
vst_shrubgroup$year <- year(vst_shrubgroup$date)
# one row per site per date
sum_vst_shrubgroup <- vst_shrubgroup %>% distinct(siteID, date)
# summarize sampling frequency per year per site
with(sum_vst_shrubgroup, table(siteID, year(date)))

# glimpse(`vst_non-woody`)
# from NEON website: the vst_non-woody table contains one record per individualID per eventID, 
# and contains growth form, structure and status data that may be linked to vst_perplotperyear data via the plotID and eventID fields.