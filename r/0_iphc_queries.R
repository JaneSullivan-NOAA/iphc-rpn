# Queries to pull IPHC data from AKFIN
# Updated october 2023
# Contact: jane.sullivan@noaa.gov

# In 2021, MESA staff worked with AKFIN to host raw IPHC data and finalized RPN
# assessment products in AKFIN's database.

# Set up ----

libs <- c("tidyverse", "RODBC", "lubridate")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

db <- read_csv('database.csv')
database_akfin=db$database
username_akfin=db$username
password_akfin=db$password

channel_akfin <- odbcConnect("akfin", uid = username_akfin, pwd = password_akfin, believeNRows=FALSE)

YEAR <- 2022

datpath <- paste0("data/", YEAR)
dir.create(datpath)
rawpath <- paste0(datpath, "/raw") 
dir.create(rawpath)
outpath <- paste0("output/", YEAR)
dir.create(outpath)

# non-halbut catch ----

# previously 'AllSpecies -> bycatch sheet', though there are additional columns
# in iphc.fiss_non_p_halb and things are formatted a little differently
nonhal <- sqlQuery(channel_akfin, query = ("
                select    *
                from      iphc.fiss_non_p_halb
                ")) %>% 
  dplyr::rename_all(tolower) 
glimpse(nonhal)
write_csv(nonhal, paste0(rawpath, "/raw_iphc_nonhal_", YEAR, ".csv")) # write raw data
nonhal

# set info ----

# previously 'AllSpecies -> Set Info sheet', though there are additional columns
# in iphc.fiss_set_p_halb and things are formatted a little differently)
set <- sqlQuery(channel_akfin, query = ("
                select    *
                from      iphc.fiss_set_p_halb
                ")) %>% 
  dplyr::rename_all(tolower) 
glimpse(set)
write_csv(set, paste0(rawpath, "/raw_iphc_set_", YEAR, ".csv")) # write raw data

# metadata ----

# pull this every year in case it changes
metadata <- sqlQuery(channel_akfin, query = ("
                select    *
                from      iphc.fiss_metadata
                ")) %>% 
  dplyr::rename_all(tolower) 

write_csv(metadata, paste0(rawpath, "/iphc_metadata_", YEAR, ".csv")) # write raw data

# station codes ----

# pull this every year in case it changes
station_codes <- sqlQuery(channel_akfin, query = ("
                select    *
                from      iphc.fiss_prp_codes
                ")) %>% 
  dplyr::rename_all(tolower) 
write_csv(station_codes, paste0(rawpath, "/iphc_station_codes_", YEAR, ".csv")) # write raw data

# read, combine, clean data -----

set <- read_csv(paste0(rawpath, "/raw_iphc_set_", YEAR, ".csv")) # write raw data
nonhal <- read_csv(paste0(rawpath, "/raw_iphc_nonhal_", YEAR, ".csv"))

cleanset <- set %>% 
  dplyr::select(-c(record_number, akfin_load_date)) %>% 
  mutate(fishing_event_id = paste(survey_year, station, sep = "_")) %>% 
  left_join(nonhal %>% 
              dplyr::select(-c(record_number, akfin_load_date)) %>% 
              mutate(fishing_event_id = paste(survey_year, station, sep = "_")),
            by = c("survey_year", "station", "set_number", "set_key", "fishing_event_id"))

# same column names as used in original 1_compile_raw_iphc_data.R
cleanset <- cleanset %>% 
  dplyr::select(year = survey_year,
                vessel = vessel_code,
                station = station,
                setno = set_number,
                startlat = begin_lat,
                startlon = begin_lon,
                hauldate = set_haul_date,
                iphcstat = iphc_stat_area,
                iphcreg = iphc_reg_area,
                avgdep = avg_depth_fm,
                lglhalno = o32_pacific_halibut_count, 
                sublglhalno = u32_pacific_halibut_count,
                lglhalwt = o32_pacific_halibut_weight,
                sublglhalwt = u32_pacific_halibut_weight,
                sktsset = num_skates_set,
                sktshaul = num_skates_hauled,
                avghkperskt = avg_num_hook_skate,
                effskts = effective_skates_hauled,
                hksretriev = hooks_retrieved,
                hksobs = hooks_observed,
                purpose = purpose_code,
                effective = eff,
                ineffcode = ineff_code,
                fishing_event_id,
                spp_iphc = iphc_species_code,
                spp_common = species_name,
                spp_sci = scientific_name,
                obs_catch = number_observed,
                subsample = sample_type)

# change fields to make them compatible with existing codes
cleanset <- cleanset %>% 
  # subsample	codes					
  # 1	| 20 hook count	  | near beginning of skate, U.S. reg areas except 1 year in SEAK for yelloweye only
  # 0	| 100% hook count	|	Canadian reg areas have 100% hook counts
  mutate(subsample = dplyr::case_when(subsample == '20Hook' ~ 1,
                                      subsample == 'WholeHaul' ~ 0),
         # remove time stamp
         hauldate = as.Date(hauldate),
         # use old codes, perhaps update if this ever becomes relevant using iphc.fiss_prp_codes
         purpose = dplyr::case_when(purpose %in% c('Standard grid', 'Standard Grid') ~ 'SG',
                                    purpose %in% c('Shallow expansion', 'Shallow Expansion') ~ 'SE',
                                    purpose %in% c('Extra station', 'Extra Station') ~ 'ES',
                                    purpose %in% c('Densified grid (2a)') ~ 'DG',
                                    purpose %in% c('Deep expansion', 'Deep Expansion') ~ 'DE',
                                    purpose %in% c('RI', 'Rockfish Index') ~ 'RI',
                                    purpose %in% c('Bering grid') ~ 'BG'))
summary(cleanset)
write_csv(cleanset, paste0('data/iphc_clean/clean_iphc_survey_', min(cleanset$year), '_', max(cleanset$year), '.csv'))
