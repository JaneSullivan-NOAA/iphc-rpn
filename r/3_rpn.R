# IPHC Relative Population Numbers by Species/Complex
# Contacts: jane.sullivan@noaa.gov or cindy.tribuzio@noaa.gov
# Last update: Sep 2020

libs<-c("tidyverse","boot")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )])>0){install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE )])}
lapply(libs,library,character.only=T)

# Set up ----

# Range of years (important as we develop methods to incorporate years <= 1997)
FIRST_YEAR <- 1998
YEAR <- 2018

# Number of bootstrap replicates
ITER <- 1500

# This is a temporary flag to toggle between using observed or extrapolated CPUE
# to calculate area-wide CPUEs or RPNs. The methods aren't totally consistent in
# the existing functions, but I don't think this will ultimately matter for
# index calculations.
OBS_OR_EXTRAP <- "EXTRAP"

# Source function to run CPUE and RPN indices
source("r/functions.R")

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

set <- read_csv(paste0("output/", YEAR, "/final_iphc_survey_", FIRST_YEAR, "_", YEAR, ".csv")) %>% 
  select(fishing_event_id, NMFS_mgmt_area, FMP, FMP_sub_area, RPN_strata, area_kmsq, 
         species, year, obs_catch, obs_effhks, obs_ineffhks, ex_effhks, ex_ineffhks,
         spp_iphc, spp_race)

# look up table ensure all sets are accounted for, even when the species
# wasn't observed
fishing_events <- set %>% 
  distinct(fishing_event_id, NMFS_mgmt_area, FMP, FMP_sub_area, RPN_strata, area_kmsq, year, 
           obs_effhks, obs_ineffhks, ex_effhks, ex_ineffhks)

# Sablefish ----
set %>% filter(species == "Sablefish") %>% distinct(species, spp_iphc, spp_race) # check codes
calc_iphc_indices(COMMON_NAME = "Sablefish")

# Pacific cod ----
set %>% filter(grepl(c("Cod|cod|Pcod|pcod"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- set %>% mutate(species = ifelse(species == "Cod_Pacific", "Pacific cod", species))
calc_iphc_indices(COMMON_NAME = "Pacific cod")

# Sharks ---- 

# Following existing methods to only include main shark species
# (spiny dogfish, sleepers, salmon sharks)
set %>% filter(spp_race %in% c(310, 320, 232)) %>% count(species, spp_iphc, spp_race)
set <- set %>% mutate(species = ifelse(spp_race %in% c(310, 320, 232), "Common sharks", species))
calc_iphc_indices(COMMON_NAME = "Common sharks")

# Shortraker rockfish ----

# note the potential to use SR/RE complex data
set %>% filter(spp_race %in% c(30576) | grepl(c("Shortraker|shortraker"), species)) %>% 
  count(species, spp_iphc, spp_race)
set <- set %>% mutate(species = ifelse(spp_race %in% c(30576), "Shortraker rockfish", species))
calc_iphc_indices(COMMON_NAME = "Shortraker rockfish")

# Shortspine thornyheads ----

# note the potential to use unidentified thornyhead data
set %>% filter(spp_race %in% c(30020) | grepl(c("shortspine|Shortspine|thorny|Thorny"), species)) %>% 
  count(species, spp_iphc, spp_race)
set <- set %>% mutate(species = ifelse(spp_race %in% c(30020), "Shortspine thornyhead", species))
calc_iphc_indices(COMMON_NAME = "Shortspine thornyhead")

# Yelloweye rockfish ----

set %>% filter(spp_race %in% c(30470) | grepl(c("yelloweye|Yelloweye"), species)) %>% 
  count(species, spp_iphc, spp_race) %>% print(n = Inf)
set <- set %>% mutate(species = ifelse(spp_race %in% c(30570), "Yelloweye rockfish", species))
calc_iphc_indices(COMMON_NAME = "Yelloweye rockfish")

# Rougheye/Blackspotted rockfish ----

# FLAG - I don't see 30051 in the data. Is this an old RACE code?
set %>% filter(spp_race %in% c(30050, 30051, 30052) | grepl(c("rougheye|Rougheye|Blackspotted"), species)) %>% 
  count(species, spp_iphc, spp_race) %>% print(n = Inf)
set <- set %>% mutate(species = ifelse(spp_race %in% c(30050, 30052), "Rougheye Blackspotted", species))
calc_iphc_indices(COMMON_NAME = "Rougheye Blackspotted")

# Arrowtooth flounder ----

# some Kamchatka/ATF overlap that may be useful?
set %>% filter(spp_race %in% c(10110) | grepl(c("arrowtooth|Arrowtooth"), species)) %>% 
  count(species, spp_iphc, spp_race) %>% print(n = Inf)
set <- set %>% mutate(species = ifelse(spp_race %in% c(10110), "Arrowtooth flounder", species))
calc_iphc_indices(COMMON_NAME = "Arrowtooth flounder")

# Greenland turbot ----

# Low sample size overall, probably only applicable to BS, maybe AI
set %>% filter(spp_race %in% c(10115) | grepl(c("Greenland|turbot|Turbot"), species)) %>% 
  count(species, FMP_sub_area, spp_iphc, spp_race) %>% print(n = Inf)
set <- set %>% mutate(species = ifelse(spp_race %in% c(10115), "Greenland turbot", species))
calc_iphc_indices(COMMON_NAME = "Greenland turbot")

# Other -----

# all spp
set %>%  count(species, spp_iphc, spp_race) %>% 
  arrange(-n) %>% print(n = Inf)

# rockfish
set %>% filter(grepl(c("rockfish|Rockfish"), species)) %>% 
  count(species, spp_iphc, spp_race) %>% 
  arrange(-n) %>% print(n = Inf)
