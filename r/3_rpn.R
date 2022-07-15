# IPHC Relative Population Numbers by Species/Complex
# Contacts: jane.sullivan@noaa.gov or cindy.tribuzio@noaa.gov
# Last update: Jul 2022

libs<-c("tidyverse","boot")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )])>0){install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE )])}
lapply(libs,library,character.only=T)

# Set up ----

# Range of years (important as we develop methods to incorporate years <= 1997)
FIRST_YEAR <- 1998
YEAR <- 2021

# Number of bootstrap replicates
ITER <- 1500

# Source function to run CPUE and RPN indices
source("r/functions.R")

# This is a temporary flag to toggle between using observed or extrapolated CPUE
# to calculate area-wide CPUEs or RPNs. The methods aren't totally consistent in
# the existing functions, but I don't think this will ultimately matter for
# index calculations.
OBS_OR_EXTRAP <- "EXTRAP"

# Change log ----

# (1) Figures show bootstrap means instead of raw means

# (2) The original code sums the tot_ineff_hks and then subtracts them from the
# extrapolated effective hooks in the myCPUE function (line ~ 227). We've
# already done that step (eff_hks = hkretriev - ex_ineff) so I think this is
# doubling the ineffective hook correction. 

# Future development ----

# (1) Bootstrap for RPN - should it be strata = RPN_strata and summed at the
# FMP_sub_area level? Currently bootstrapped at RPN_strata level and summed up
# to FMP_sub_area.

# (2) Could potentially increase code speed using data.table. Will have to
# balance with code readability.

# (3) What are the priorities for development? Including benchmarks or time
# stamps, doing multiple species at once, etc.

# (4) Split INSIDE between PWS and SEAK. Collaborate with ADFG biologists to get
# depth strata area_kmsq, produce RPNs for YEAK and Pcod inside waters (lingcod?).

# Read data ----

# FLAG 2021 - ask Jane for help if you want to recreate results for the old vs
# new time series... the switch from spreadsheets to using data from IPHC's
# website was a little messy. Resulting comparisons for these data are stored in
# output/compare_web_data
full_set <- read_csv(paste0("output/", YEAR, "/final_iphc_survey_", FIRST_YEAR, "_", YEAR, ".csv")) %>% 
  select(fishing_event_id, NMFS_mgmt_area, FMP, FMP_sub_area, RPN_strata, area_kmsq, 
         species, year, obs_catch, obs_effhks, obs_ineffhks, ex_effhks, ex_ineffhks,
         spp_iphc, spp_race)

# # (2021 - use 'historical' data for 1998 - 2019, then new web data for 2020 and
# # 2021. full new data time series in output/2021a)
# full_set <- read_csv(paste0("output/", YEAR, "a/final_iphc_survey_", FIRST_YEAR, "_", YEAR, ".csv")) %>% 
#   select(fishing_event_id, NMFS_mgmt_area, FMP, FMP_sub_area, RPN_strata, area_kmsq, 
#          species, year, obs_catch, obs_effhks, obs_ineffhks, ex_effhks, ex_ineffhks,
#          spp_iphc, spp_race)
# full_set <- full_set %>% filter(year %in% c(2021, 2020))
# 
# full_set2 <- read_csv(paste0("output/2019/final_iphc_survey_", FIRST_YEAR, "_2019.csv")) %>% 
#   select(fishing_event_id, NMFS_mgmt_area, FMP, FMP_sub_area, RPN_strata, area_kmsq, 
#          species, year, obs_catch, obs_effhks, obs_ineffhks, ex_effhks, ex_ineffhks,
#          spp_iphc, spp_race)
# full_set2 <- full_set2 %>% filter(year <= 2019)
# 
# full_set <- full_set %>% 
#   bind_rows(full_set2)

# look up table ensure all sets are accounted for, even when the species
# wasn't observed
full_fishing_events <- full_set %>% 
  distinct(fishing_event_id, NMFS_mgmt_area, FMP, FMP_sub_area, RPN_strata, area_kmsq, year, 
           obs_effhks, obs_ineffhks, ex_effhks, ex_ineffhks)

# For species/assessment that exist across the full range of the IPHC survey,
# use the full data set. Otherwise, define subsets before running the functions.

# Sablefish ----

full_set %>% filter(species == "Sablefish") %>% distinct(species, spp_iphc, spp_race) # check codes
set <- full_set; fishing_events <- full_fishing_events
calc_iphc_indices(COMMON_NAME = "Sablefish")

# Pacific cod ----

full_set %>% filter(grepl(c("Cod|cod|Pcod|pcod"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(species == "Cod_Pacific", "Pacific cod", species)) #%>% 
set %>% filter(species == "Pacific cod") %>% count(species, FMP_sub_area) 
set <- set %>% filter(FMP_sub_area != "WC") # very low sample size
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Pacific cod")

# Sharks ---- 

# Following existing methods to only include main shark species
# (spiny dogfish, sleepers, salmon sharks)
full_set %>% filter(spp_race %in% c(310, 320, 232)) %>% count(species, spp_iphc, spp_race)
set <- full_set %>% mutate(species = ifelse(spp_race %in% c(310, 320, 232), "Common sharks", species))
set %>% filter(species == "Common sharks") %>% count(species, FMP_sub_area) # filter areas?
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Common sharks")

# Spiny dogfish ----

# spp_race = 310
set <- full_set %>% mutate(species = ifelse(species == "Shark_Spiny_Dogfish", "Spiny dogfish", species)) 
set %>% filter(species == "Spiny dogfish") %>% count(species, FMP_sub_area) 
# consider omitted AI and BS for low sample sizes
# set <- set %>% filter(!FMP_sub_area %in% c("AI", "BS")) 
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Spiny dogfish")

# Sleeper shark ----

# spp_race = 320
set <- full_set %>% mutate(species = ifelse(species == "Shark_Sleeper", "Sleeper shark", species)) 
set %>% filter(species == "Sleeper shark") %>% count(species, FMP_sub_area) 
# consider omitted AI, WC, and potentially EY/SE for low sample sizes
# set <- set %>% filter(!FMP_sub_area %in% c("AI", "WC", "EY/SE")) 
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Sleeper shark")

# Shortraker rockfish ----

# note the potential to use SR/RE complex data
full_set %>% filter(spp_race %in% c(30576) | grepl(c("Shortraker|shortraker"), species)) %>% 
  count(species, spp_iphc, spp_race)
set <- full_set %>% mutate(species = ifelse(spp_race %in% c(30576), "Shortraker rockfish", species))
set %>% filter(species == "Shortraker rockfish") %>% count(species, FMP_sub_area) # filter areas?
set <- set %>% filter(!(FMP_sub_area %in% c("WC", "WGOA"))) # low sample sizes
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Shortraker rockfish")

# Shortspine thornyhead rockfish ----

# note the potential to use unidentified thornyhead data
full_set %>% filter(spp_race %in% c(30020) | grepl(c("shortspine|Shortspine|thorny|Thorny"), species)) %>% 
  count(species, spp_iphc, spp_race)
set <- full_set %>% mutate(species = ifelse(spp_race %in% c(30020), "Shortspine thornyhead", species))
set %>% filter(species == "Shortspine thornyhead") %>% count(species, FMP_sub_area) # filter areas?
# set <- set %>% filter(!(FMP_sub_area %in% c("WC", "WGOA"))) # low sample sizes
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Shortspine thornyhead")

# Lingcod ----

full_set %>% filter(spp_race %in% c(21910) | grepl(c("lingcod|Lingcod"), species)) %>% 
  count(species, spp_iphc, spp_race)
set <- full_set %>% mutate(species = ifelse(spp_race %in% c(21910), "Lingcod", species))
set %>% filter(species == "Lingcod") %>% count(species, FMP_sub_area) # filter areas?
set <- set %>% filter(!(FMP_sub_area %in% c("BS", "AI", "WGOA"))) # low sample sizes
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Lingcod")

# Yelloweye rockfish ----

full_set %>% filter(spp_race %in% c(30470) | grepl(c("yelloweye|Yelloweye"), species)) %>% 
  count(species, spp_iphc, spp_race) %>% print(n = Inf)
set <- full_set %>% mutate(species = ifelse(spp_race %in% c(30470), "Yelloweye rockfish", species))
set %>% filter(species == "Yelloweye rockfish") %>% count(species, FMP_sub_area) # filter areas?
set <- set %>% filter(!(FMP_sub_area %in% c("BS", "AI"))) # low sample sizes
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Yelloweye rockfish")

# Rougheye/Blackspotted rockfish ----

# FLAG - I don't see 30051 in the data. Is this an old RACE code?
full_set %>% filter(spp_race %in% c(30050, 30051, 30052) | grepl(c("rougheye|Rougheye|Blackspotted"), species)) %>% 
  count(species, spp_iphc, spp_race) %>% print(n = Inf)
set <- full_set %>% mutate(species = ifelse(spp_race %in% c(30050, 30052), "REBS", species))
set %>% filter(species == "REBS") %>% count(species, FMP_sub_area) # filter areas?
# set <- set %>% filter(!(FMP_sub_area %in% c("BS", "WC", "WY", "WGOA"))) # low sample sizes
# set <- set %>% filter((FMP_sub_area %in% c("WGOA", "CGOA", "WY", "EY/SE", "AI"))) # low sample sizes
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "REBS")

# Arrowtooth flounder ----

# some Kamchatka/ATF overlap that may be useful?
full_set %>% filter(spp_race %in% c(10110) | grepl(c("arrowtooth|Arrowtooth"), species)) %>% 
  count(species, spp_iphc, spp_race) %>% print(n = Inf)
set <- full_set %>% mutate(species = ifelse(spp_race %in% c(10110), "Arrowtooth flounder", species))
set %>% filter(species == "Arrowtooth flounder") %>% count(species, FMP_sub_area) # filter areas?
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Arrowtooth flounder")

# kamchatka

# some Kamchatka/ATF overlap that may be useful?
full_set %>% filter(FMP %in% c("BSAI", "GOA")) %>% filter(spp_race %in% c(10110) | grepl(c("arrowtooth|Arrowtooth"), species)) %>% 
  count(species,FMP, spp_race) %>% print(n = Inf)


# Greenland turbot ----

# Low sample size overall, probably only applicable to BS, maybe AI
full_set %>% filter(spp_race %in% c(10115) | grepl(c("Greenland|turbot|Turbot"), species)) %>% 
  count(species, FMP_sub_area, spp_iphc, spp_race) %>% print(n = Inf)
set <- full_set %>% 
  filter(FMP_sub_area %in% c("AI", "BS")) %>% 
  mutate(species = ifelse(spp_race %in% c(10115), "Greenland turbot", species))
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Greenland turbot")

# Redbanded rockfish ----

full_set %>% filter(spp_race %in% c(30475) | grepl(c("Redbanded|redbanded"), species)) %>% 
  count(species, spp_iphc, spp_race) %>% print(n = Inf)
set <- full_set %>% mutate(species = ifelse(spp_race %in% c(30475), "Redbanded rockfish", species))
set %>% filter(species == "Redbanded rockfish") %>% count(species, FMP_sub_area) # filter areas?
set <- set %>% filter(FMP_sub_area %in% c("CAN", "EY/SE", "INSIDE")) # remove areas w/ low sample sizes
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Redbanded rockfish")

# GOA Other Rockfish ----

# Needs development in order to adhere to assessment definitions
OROX_code <- c(30340, 30400, 30410, 30370, 30120, 30170, 30200, 30320, 
               30475, 30430, 30270, 30560, 30100, 30190, 30380, 30350,
               30220, 30470, 30600, 30240)

set %>% filter(spp_race %in% OROX_code) %>% 
  count(species, spp_iphc, spp_race) %>% arrange(-n) %>% print(n = Inf)

# BSAI Other Rockfish ----

# Needs development in order to adhere to assessment definitions

# Other -----

# all spp
full_set %>%  count(species, spp_iphc, spp_race) %>% 
  filter(n > 200) %>% 
  arrange(-n) %>% print(n = Inf)

# rockfish
full_set %>% filter(grepl(c("rockfish|Rockfish"), species)) %>% 
  count(species, spp_iphc, spp_race) %>% 
  arrange(-n) %>% print(n = Inf)

# Skates ----

# species/group	RACE codes
# big skate	420
# longnose skate	440

# ABA complex (Bering, Alaska, Aleutian)	435, 471, 472
full_set %>% filter(spp_race %in% c(435, 471, 472)) %>% count(species, spp_iphc, spp_race)
set <- full_set %>% 
  mutate(species = ifelse(spp_race %in% c(435, 471, 472), "ABA skate complex", species)) %>% 
  filter(!FMP_sub_area %in% c('BS', 'AI', 'CAN', 'INSIDE', 'WC'))
set %>% filter(species == "ABA skate complex") %>% count(species, FMP_sub_area) # filter areas?
fishing_events <- full_fishing_events %>% 
  filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "ABA skate complex")

# big skate
full_set %>% filter(spp_race %in% c(420)) %>% count(species, spp_iphc, spp_race)
set <- full_set %>% 
  mutate(species = ifelse(spp_race %in% c(420), "Big skate", species)) %>% 
  filter(!FMP_sub_area %in% c('BS', 'AI', 'CAN', 'INSIDE', 'WC'))
set %>% filter(species == "Big skate") %>% count(species, FMP_sub_area) # filter areas?
fishing_events <- full_fishing_events %>% 
  filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Big skate")

# longnose skate
full_set %>% filter(spp_race %in% c(440)) %>% count(species, spp_iphc, spp_race)
set <- full_set %>% 
  mutate(species = ifelse(spp_race %in% c(440), "Longnose skate", species)) %>% 
  filter(!FMP_sub_area %in% c('BS', 'AI', 'CAN', 'INSIDE', 'WC'))
set %>% filter(species == "Longnose skate") %>% count(species, FMP_sub_area) # filter areas?
fishing_events <- full_fishing_events %>% 
  filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Longnose skate")