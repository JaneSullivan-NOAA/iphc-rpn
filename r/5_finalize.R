# summarize files for AKFIN
# final set data and results hosted in the AFSC_HOST schema of the AKFIN database
# Contacts: jane.sullivan@noaa.gov or cindy.tribuzio@noaa.gov
# Last update: Jul 2022

# read excel
libs <- c("dplyr", "fs", "readr", "readxl", "purrr") 
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# Range of years (important as we develop methods to incorporate years <= 1997)
FIRST_YEAR <- 1998
YEAR <- 2021

mainfiles <- list.files(path = paste0('output/', YEAR))
mainfiles <- mainfiles[!grepl('.png|.csv|ADFG', mainfiles)]
mainfiles

# jul 2021 = currently i am not including the seak adfg files on akfin since we can't also host the set
# level data (e.g. seak_iphc_clean_1998_2021.csv )
adfgfiles <- list.files(path = paste0('output/', YEAR, '/ADFG'))
adfgfiles <- adfgfiles[!grepl('.png|.csv', adfgfiles)]
adfgfiles

fiss_cpue <- as.list(paste0('output/', YEAR, "/", mainfiles, "/CPUE_", mainfiles, "_", YEAR, "_EXTRAP.csv"))%>% 
  map(read_csv) %>% 
  bind_rows() 
names(fiss_cpue)
fiss_cpue <- fiss_cpue %>% 
  rename_all(toupper) %>% 
  rename(SURVEY_YEAR = YEAR)
write_csv(fiss_cpue, paste0('output/', YEAR, '/fiss_cpue.csv'))

fiss_rpn <- as.list(paste0('output/', YEAR, "/", mainfiles, "/RPN_", mainfiles, "_", YEAR, "_EXTRAP.csv"))%>% 
  map(read_csv) %>% 
  bind_rows() 
names(fiss_rpn)
fiss_rpn <- fiss_rpn %>% 
  rename_all(toupper)%>% 
  rename(SURVEY_YEAR = YEAR)
write_csv(fiss_rpn, paste0('output/', YEAR, '/fiss_rpn.csv'))

# rename cleaned data to fiss_cleaned.csv - FLAG for some reason the target
# table names do not match what I provided to AKFIN in the metadata tables. This
# is annoying.
fiss_cleaned <- read_csv(paste0('output/', YEAR, '/final_iphc_survey_', FIRST_YEAR, '_', YEAR, '.csv'))
names(fiss_cleaned)
fiss_cleaned <- fiss_cleaned %>% 
  select(FISHING_EVENT_ID = fishing_event_id,
         NMFS_AREA = NMFS_AREA,
         OBS_INEFF_HOOKS = obs_ineffhks,
         SURVEY_YEAR = year,
         VESSEL = vessel,
         STATION = station,
         SET_NUMBER = setno,
         START_LAT = startlat,
         START_LON = startlon,
         HAUL_DATE = hauldate,
         IPHC_STAT = iphcstat,
         IPHC_REG = iphcreg,
         AVG_DEPTH = avgdep,
         LGL_HAL_NO = lglhalno,
         SUBLGL_HAL_NO = sublglhalno,
         LGL_HAL_WT = lglhalwt,
         SUBLGL_HAL_WT = sublglhalwt,
         SKATES_SET = sktsset,
         SKATES_HAUL = sktshaul,
         AVG_HK_PER_SKT = avghkperskt,
         EFF_SKATES = effskts,
         HOOKS_RETRIEVED = hksretriev,
         HOOKS_OBS = hksobs,
         PURPOSE = purpose,
         EFFECTIVE = effective,
         SPECIES_CODE_IPHC = spp_iphc,
         SPECIES_COMMON_NAME = spp_common,
         SPECIES_SCIENTIFIC_NAME = spp_sci,
         OBS_CATCH = obs_catch,
         SUBSAMPLE = subsample,
         SPECIES = species,
         SPECIES_CODE_RACE = spp_race,
         ASSUMED_USE = assumed_use,
         HAUL_DATE_2 = hauldate2,
         OBS_EFF_HOOKS = obs_effhks,
         EX_INEFF_HOOKS = ex_ineffhks,
         EX_EFF_HOOKS = ex_effhks,
         AVG_DEPTH_M = avgdep_m,
         DEPTH_BIN_M = depbin_m,
         FMP_SUB_AREA = FMP_sub_area,
         NMFS_MGMT_AREA = NMFS_mgmt_area,
         FMP_AREA = FMP,
         FMP_DEPTH = fmp_depth,
         RPN_STRATA = RPN_strata,
         AREA_KMSQ = area_kmsq)
ncol(fiss_cleaned)
names(fiss_cleaned)
write_csv(fiss_cleaned, paste0('output/', YEAR, '/fiss_cleaned.csv'))

# rename removed data to fiss_stations_removed.csv for AKFIN upload
fiss_stations_removed <- read_csv(paste0('output/', YEAR, '/removed_data_', FIRST_YEAR, '_', YEAR, '.csv'))
names(fiss_stations_removed)
fiss_stations_removed <- fiss_stations_removed %>% 
  rename_all(toupper)
str(fiss_stations_removed)
unique(fiss_stations_removed$REASON)
# formatting problem...(fixed in 2_clean_iphc_data.R, can be removed)
fiss_stations_removed <- fiss_stations_removed %>% 
  mutate(REASON = ifelse(REASON == "duplicate effective sets, delete second set from 2019-07-22",
                         "duplicate effective sets delete second set from 2019-07-22", REASON)) %>% 
  mutate(REASON = ifelse(REASON == "duplicate effective sets, delete second set from 2019-08-22",
                         "duplicate effective sets delete second set from 2019-08-22", REASON))

write_csv(fiss_stations_removed, paste0('output/', YEAR, '/fiss_stations_removed.csv'))
