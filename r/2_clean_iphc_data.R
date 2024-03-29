# Clean IPHC data
# Contacts: jane.sullivan@noaa.gov or cindy.tribuzio@noaa.gov
# Last update: Oct 2023

# Step two in creating a fully reproducible abundance index using the IPHC
# setline survey data is add in new columns, including spatial/area-specific
# information, and QA/QC existing rows. The final product from this script
# should be a csv file that is ready to input to CPUE and RPN analysis.

# Helpful resources ----

# Stat area definitions: https://iphc.int/uploads/pdf/tr/IPHC-2004-TR049.pdf
# FISS Stations: https://iphc.int/uploads/pdf/2017_FISS_station_maps.pdf

# Record of changes from original code ----

# (1) Depth fathoms to meters: was 1.82, changed to 1.8288 to increase
# accuracy/precision
# (2) Assign station 6604 (2017) as NMFS area 541. Currently listed in
# stations_lookup.csv as part of Canada. 
# (3) I removed rows with no subsample code 
# (4) Removed averaging across stations. NMFS areas are defined by where a set
# is fished in a given yr.
# (5) The logic to remove duplicate subsample codes for YE in 2010/11 is
# slightly different, more targeted to instances of duplcicates instead of broad
# area definitions

# Future development: (1) Redefine and potentially use species groupings like
# "Rougheye/Shortraker", "Greenland/Kamchatka/Arrowtooth",
# "Kamchatka/Arrowtooth" (these are is.na(spp_sci)) (2) Currently yelloweye
# rockfish with subsample == 0 are removed because hksretriev != hkobs. An easy
# development here would be to remove subsample == 1 instead and then
# extrapolate obs_ineffhks to hksretriev and replace hksobs with hksretriev. The
# issue here is that for YE with subsample = 0 have hksobs at the subsample = 1
# rate

# Questions for Cindy ----

# Questions under YE sampling section:
# (1) Sporadic subsample = 0 for multiple species in federal waters >= 2010.
# Also related, there are duplicate fishing_event_ids for YE in 2010.

# Questions under remove list section:
# (1) rmlist object from IPHC_RPN_CPUE_functions_v3 converted to
# fishing_event_id format. All of these have >= 490 hooks retried and are
# effective == Y. Can you help me figure out why these were removed so we can
# automate this?

# Question under FMP section:
# (1) What's the difference between FMP sub area and NMFS mgmt area?
# (2) How did Pugest Sound get nmfs = 679?

# Set up ----

# '%nin%' <- Negate('%in%')
libs <- c("tidyverse", "rgdal", "lubridate")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# User defined variable
FIRST_YEAR <- 1998 # first year of survey data used (currently exclude 1997)
YEAR <- 2022 # most recent year of data

# Read data ----

# Cleaned IPHC survey data (all species), output from 1_compile_raw_iphc_data.R
# - old years you have to change '1998' back to '1997'...
set <- read_csv(paste0("data/iphc_clean/clean_iphc_survey_1993_", YEAR, ".csv"),
                guess_max = 1e5) %>% 
  filter(year >= FIRST_YEAR)

# FMP/NMFS area lookup table - original file from C Tribuzio
fmp <- read_csv("data/FMP_areas_lookup.csv") #%>% 
  # select(nmfs = NMFS_AREA, fmp = FMP, fmp_subarea = FMP_sub_area, nmfs_mgmt_area = NMFS_mgmt_area)

# FMP and strata area sizes - original file from C Tribuzio
areasize <- read_csv("data/areasize_2015.csv")

# IPHC/RACE species codes conversions with assumed usage - original file from C
# Tribuzio
spp_conversion <- read_csv("data/species_code_conv_table.csv") %>% 
  select(species = Species, spp_iphc = IPHC_code, spp_race = RACE_CODE, assumed_use = Ass_usage)

# Reformat spatial data prior to 2013 ----

# NOV 2021 - no longer needed now that data are in akfin!

# # Starting location used instead of mid or end location because it's how the
# # location is selected and is therefore the most standardized spatial data.
# 
# # This chunk of code converts them from degrees decimal minutes (DDM) to decimal
# # degrees (DD)
# oldset <- set %>% 
#   filter(year < 2013) %>%
#   # REMOVE
#   # filter(fishing_event_id %in% unique(check_3A_CAN$fishing_event_id)) %>% 
#   # sep = positions of numeric string where split should occur
#   separate(col = startlat, into = c("latd", "latm", "lats"), sep = c(2,4,6), remove = FALSE, convert = TRUE) %>% 
#   separate(col = startlon, into = c("lond", "lonm", "lons"), sep = c(3,5,7), remove = FALSE, convert = TRUE) %>% 
#   # convert DDM to DD
#   mutate(startlat = latd + as.numeric(paste(latm, lats, sep = ".")) / 60,
#          startlon2 = lond + as.numeric(paste(lonm, lons, sep = ".")) / 60,
#          # Deal with records in the eastern hemisphere - FLAG I really don't
#          # understand subtracting 100 but it works
#          startlon = ifelse(startlon2 > 180, startlon2 - 100, -startlon2)) %>%
#   select(-c(latd, lond, latm, lonm, lats, lons, startlon2)) 
# 
# # Recombine set data  
# set <- oldset %>% 
#   bind_rows(set %>% filter(year >= 2013)) %>% 
#   arrange(year)

# Ineffective sets ----

# Get rid of any sets deemed by IPHC as ineffective
ineff_sets <- set %>% 
  filter(effective == "N") %>% 
  pull(fishing_event_id)

# Only keep "effective" sets
set <- set %>% filter(!(fishing_event_id %in% ineff_sets))

# Create running list of removed data
removed_data <- data.frame(fishing_event_id = ineff_sets,
                           entire_set_removed = TRUE,
                           reason = "effective code = N")


# Get rid of any remaining ineffcodes -UPDATE NOV 2021: These no longer are an issue in the updated akfin dataset!
# GI = Gear issues; PS = Predation shark; OT = Other
unique(set$ineffcode)
# ineff_sets2 <- set %>% filter(!is.na(ineffcode)) %>% distinct(ineffcode, fishing_event_id) %>% pull(fishing_event_id)
# # set %>% filter(fishing_event_id %in% ineff_sets2) %>% View()
# set <- set %>% filter(!(fishing_event_id %in% ineff_sets2)) %>% select(-ineffcode)
# 
# removed_data <- removed_data %>% 
#   bind_rows(data.frame(fishing_event_id = ineff_sets2,
#                        entire_set_removed = TRUE,
#                        reason = "effective code = Y, but ineffective code = GI | GI:PS | OT"))

# Subsamples ----

# subsample	codes					
# 1	| 20 hook count	  | near beginning of skate, U.S. reg areas except 1 year in SEAK for yelloweye only
# 0	| 100% hook count	|	Canadian reg areas have 100% hook counts

set %>% count(subsample)
no_subsample <- set %>% filter(is.na(subsample)) %>% pull(fishing_event_id)
# set %>% filter(fishing_event_id %in% no_subsample) %>% View()
# All of the cases where subsample is NA there are 0 hooks observed. Remove
# these.
set <- set %>% filter(!(fishing_event_id %in% no_subsample))

removed_data <- removed_data %>% 
  bind_rows(data.frame(fishing_event_id = no_subsample,
                       entire_set_removed = TRUE,
                       reason = "no subsample code"))

# Birds/marine mammals ----

set <- set %>% left_join(spp_conversion, by = "spp_iphc") # conversion codes from IPHC

# Remove rows with bird and marine mammal bycatch
not_gf <- set %>% 
  filter(assumed_use != "Groundfish") %>% 
  select(fishing_event_id, assumed_use, spp_common)
not_gf %>% count(assumed_use)  
not_gf <- not_gf %>% distinct(fishing_event_id) %>% pull()

# Remove birds and marine mammals
set <- set %>% filter(assumed_use == "Groundfish") 

removed_data <- removed_data %>% 
  bind_rows(data.frame(fishing_event_id = not_gf,
                       entire_set_removed = FALSE,
                       reason = "specific rows with bird or marine mammal bycatch"))

# Duplicate sets ----

# When a set at a station is deemed ineffective, the IPHC can do the set again.
# There should be no duplicates once ineffs are removed. 
set %>% distinct(fishing_event_id, setno) %>% 
  count(fishing_event_id) %>% 
  filter(n > 1) -> dup_tst 
dup_tst # should be zero! but see below.

# In 2019 3 duplicate sets weren't flagged as ineffective in the data set
# provided by IPHC. Executive decision 10/2/20: CT and JS decided to retain the
# first of the duplicates assuming the second are accidental repeats. In 2023, 1
# duplicate set from 2022 was also flagged. JS did the same thing as was done
# for the 2019 dups.
set %>% 
  filter(fishing_event_id %in% dup_tst$fishing_event_id) %>%
  # write_csv("output/2022/four_duplicates_2022.csv") %>% 
  # View() 
  distinct(fishing_event_id, hauldate, setno, vessel) %>% 
  group_by(fishing_event_id) %>% 
  summarize(second_set = max(hauldate)) %>% 
  mutate(combo = paste0(fishing_event_id, "_", second_set)) -> dup_tst2

# Remove duplicate sets in 2019
removed_data <- removed_data %>% 
  bind_rows(data.frame(fishing_event_id = dup_tst2$fishing_event_id,
                       entire_set_removed = TRUE,
                       reason = paste0("duplicate effective sets delete second set from ", dup_tst2$second_set)))
# update set df
set <- set %>% 
  mutate(hauldate2 = lubridate::date(hauldate),
         combo = paste0(fishing_event_id, "_", hauldate2)) %>% # max doesn't do anything but drop the time stamp of the date
  filter(!c(combo %in% dup_tst2$combo)) %>%
  select(-combo)

# Low hook observations ----

# If there are remaining stations with <= 15 hooks, remove them. They should
# already have been filtered out as ineffective.
set %>% filter(hksobs <= 15) %>% count() # should be 0 - if not must add them to removed_data and filter them from set
# set <- set %>% filter(hksobs > 15) 

# Remaining columns with NAs should only be spp_sci (species scientific name)
# and ineffcodes, which were already dealt with
set %>% 
  summarise_all(list(~ sum(is.na(.)))) %>% 
  pivot_longer(cols = names(set)) %>% 
  filter(value != 0)
set %>% filter(is.na(spp_sci)) %>% pull(spp_common) %>% unique() 
# these are all gear related, spp grps, unidentified spp.. make # TODO: added
# to list to discuss with Cindy. Do we want to treat these species groups in a
# specific way? At a minimum, we should tell data users that these are either
# used or removed.
set %>% 
  filter(spp_common %in% c("Rougheye/Shortraker", 
                           "Greenland/Kamchatka/Arrowtooth",
                           "Kamchatka/Arrowtooth",
                           "Bering/Aleutian Skate",
                           "Bering/Alaska Skate"))  %>% #View()
  # count(year, iphcreg, spp_common) #%>% View()
  group_by(year, iphcreg, spp_common) %>% 
  dplyr::summarise(catch = sum(obs_catch)) -> sppgroupcatch

sppgroupcatch %>% write_csv(paste0('output/', YEAR, '/sppgroupcatches.csv'))

# Double subsampling ----

# UPDATE NOV 2021: this has been resolved in new akfin data

# Area for future development: subsample = 0 could be used if we made hksretriev
# = hksobs for these sets and extrapolated ineffective hooks up to the
# hksretriev level

# Sets where there were 2 subsample rates within a set. Mostly yelloweye but
# also one spiny dogfish row in 2015 in 3B ("2015_5166"). NOTE THAT IN WEB DATA
# THESE HAVE GONE AWAY.
dbl_sub <- set %>% 
  count(fishing_event_id, subsample) %>% 
  pivot_wider(names_from = subsample, values_from = n, values_fill = 0) %>% 
  filter(`1` >= 1 & `0` >= 1) %>% 
  pull(fishing_event_id)

# The only way this would be ok is if the hksretriev == hksobs (like they are in
# area 2B where subsample = 0 is routine)
set %>% 
  filter(fishing_event_id %in% dbl_sub & hksobs == hksretriev) %>% 
  distinct(fishing_event_id) %>% 
  pull(fishing_event_id)
# this is 0 length, so these all need to be removed. NOTE THAT IN WEB DATA
# THESE HAVE GONE AWAY.

# set %>% 
#   filter(fishing_event_id %in% dbl_sub) %>% 
#   filter(subsample == 0) %>% 
#   count(year, iphcreg, spp_common) %>% 
#   ggplot(aes(x = year, y = n, fill = factor(spp_common))) +
#   geom_bar(stat = 'identity', position = position_dodge(preserve = "single")) +
#   facet_wrap(~iphcreg)

# Sets that have both subsample = 0 and 1 for Yelloweye Rockfish occurred in
# 2010 and 2011 65 times (all but 2 are in 2010). If future development occurs
# for subsample 0, then subsample = 1 for YE in this set should be removed. For
# now we're keeping only subsample = 1 as has been done in the past. NOTE THAT IN WEB DATA
# THESE HAVE GONE AWAY.
set %>% 
  filter(grepl("Yelloweye", spp_common) & 
           iphcreg != "2B") %>% 
  count(fishing_event_id, subsample) %>% 
  pivot_wider(names_from = subsample, values_from = n, values_fill = 0) %>% 
  filter(`1` >= 1 & `0` >= 1) %>% 
  pull(fishing_event_id) -> dbl_YE

setdiff(dbl_YE, dbl_sub) # all the dbl_YE are contained within dbl_sub

# Logic applied in IPHC_RPN_CPUE_functions_v3, which removes 172 fishing events
# instead of just the 65 identified as having both subsample 0 and 1. NOTE THAT IN WEB DATA
# THESE HAVE GONE AWAY.
set %>% 
  filter(year %in% c(2010, 2011) & station >= 3000 & station <= 4050 & subsample == 0) %>% 
  distinct(fishing_event_id) %>% 
  pull() -> old_dbl_YE

# It looks like the old logic missed 6 sets with double subsamples, 5 YE in 2011
# in 3A and 1 Spiny Dogfish in 3B. NOTE THAT IN WEB DATA
# THESE HAVE GONE AWAY.
setdiff(dbl_sub, old_dbl_YE) 
setdiff(old_dbl_YE, dbl_sub) # = 0, all of the old dbl YE are in dbl_sub
set %>% filter(fishing_event_id %in% setdiff(dbl_sub, old_dbl_YE) & subsample == 0) %>% 
  count(year, spp_common, iphcreg) #%>% View()

# Decision made 8/21/20 to remove these subsample = 0 until we want to develop
# this further for Yelloweye
tst <- nrow(set)
set <- set %>% 
  filter(!(fishing_event_id %in% dbl_sub & subsample == 0))
tst - nrow(set) == length(dbl_sub) # should be true, we only want to remove problem rows

# Final tests
set %>% 
  distinct(fishing_event_id, subsample) %>% 
  count(fishing_event_id) %>% 
  filter(n > 1) %>% 
  pull(fishing_event_id) # should be 0

# **FLAG 2021** subsample == 0 no longer means hksretriev == hksobs. UGH!!

# (pre-2021) All sets that are subsample == 0 should have the same number of hks observed
# as were retrieved
set %>% 
  filter(subsample == 0) %>%
  filter(hksobs != hksretriev) %>% 
  mutate(diff = hksretriev - hksobs) %>% 
  distinct(fishing_event_id, year, diff, hksobs, hksretriev, purpose) %>% 
  arrange(diff)# %>% View()# this should technically be 0, but we'll forgive these 2 sets
  # distinct(year) %>% arrange(year) # this should technically be 0, but we'll forgive these 2 sets

# (2021) after doing some explorations in new_data_problems.R, it looks like
# past data assumed hksobs equaled hksretriev...these values now are consistent
# with just hksobs. for now, fix hksretriev equal to hksobs for subsample = 0 in
# order to make the rest of the code run...
set <- set %>% 
  mutate(hksretriev = ifelse(subsample == 0, hksobs, hksretriev))

# (2021) these sets aren't present...
# removed_data <- removed_data %>% 
#   bind_rows(data.frame(fishing_event_id = dbl_sub,
#                        entire_set_removed = FALSE,
#                        reason = "sets w/ subsample = 0 & 1, removed rows where subsample = 0"))

# Duplicate species codes ----

# rmlist object from IPHC_RPN_CPUE_functions_v3.R converted to fishing_event_id
# format. All of these have >= 490 hooks retrieved, and >= hooks observed and are
# effective == Y. These were removed because of duplicate species counts. We
# found 5 more of these cases to be removed.
rmlist <- c("1999_2134","1999_5258","2000_4058","2000_4157","2000_4324","2000_7085","2000_7089","2000_7090","2000_7093",
          "2004_6144","2008_1061","2008_1070","2008_7022","2009_4031","2009_5019","2010_6070","2010_7083","2013_4347",
          "2015_5162")

dup_spp <- set %>% 
  group_by(fishing_event_id) %>% 
  count(spp_common) %>% 
  filter(n > 1) %>% 
  pull(fishing_event_id)

setdiff(dup_spp, rmlist) # new sets with duplicate species (none in 2021)

# Remove all these sets due to data entry errors
set <- set %>% filter(!(fishing_event_id %in% dup_spp))

# (2021) these sets aren't present...
# removed_data <- removed_data %>% 
#   bind_rows(data.frame(fishing_event_id = dup_spp,
#                        entire_set_removed = TRUE,
#                        reason = "sets w/ duplicate species counts"))

# Ineffective hooks ----

# In order to calculate CPUE or RPNs, we need to adjust for hooks that
# effectively weren't fishing. In the past this has included:
# spp_iphc spp_common          
# 307 Bent/Broken/Missing 
# 306 Unknown/Unspecified 
# 301 Missing Hook/Gangion
# 302 Bent Hook       

# Check annually for new codes that may be also be ineffective
set %>% filter(between(spp_iphc, 300, 399)) %>% distinct(spp_iphc, spp_common) 
set %>% distinct(spp_iphc, spp_common) #%>% View()

# Assume old code defns are correct, create new col obs_ineffhks = # ineffective
# hks by subsample rate
set <- set %>% 
  filter(spp_iphc %in% c(301, 302, 306, 307)) %>% 
  group_by(fishing_event_id) %>%  
  dplyr::summarise(obs_ineffhks = sum(obs_catch)) %>% 
  ungroup() %>% 
  right_join(set) %>% 
  # Account for situations where there were no ineffective hooks
  mutate(obs_ineffhks = ifelse(is.na(obs_ineffhks), 0, obs_ineffhks),
         # Observed effective hooks 
         obs_effhks = hksobs - obs_ineffhks)

# if subsample = 1 (20 hk count), use ratio of ineffective hooks to observed
# hooks to calculate total ineffective hooks for the set. This will need to be
# changed if the Yelloweye sets with different subsample rates get used in the
# future.
set <- set %>%
  # ex = extrapolated
  mutate(ex_ineffhks = ifelse(subsample == 1, 
                                (obs_ineffhks / hksobs) * hksretriev,
                              obs_ineffhks),
         ex_effhks = hksretriev - ex_ineffhks) 

# This should be 0. If it's not, there are likely duplicates that need to be
# fixed and added to removed_data list
set %>% 
  distinct(fishing_event_id, ex_ineffhks) %>% 
  count(fishing_event_id) %>% 
  filter(n > 1) %>% 
  pull(fishing_event_id) 

# Depth bins ----

# Add in depth in meters and depth bins
set <- set %>% 
  mutate(avgdep_m = avgdep * 1.8288,
         depbin_m = case_when(avgdep_m < 100 ~ 0,
                              avgdep_m >= 100 & avgdep_m < 200 ~ 100,
                              avgdep_m >= 200 & avgdep_m < 300 ~ 200,
                              avgdep_m >= 300 & avgdep_m < 400 ~ 300,
                              avgdep_m >= 400 ~ 400))

# NMFS areas ----

# Original code from Jordan Watson

sp_data <- readOGR(dsn = "data/shapefiles/gf95_nmfs",layer = "gf95_nmfs_polygon")

plot(sp_data)
axis(1); axis(2)

# Create smaller set dataframe without extra cols
set_sm <- set %>% 
  select(lon = startlon, lat = startlat, fishing_event_id) %>% 
  data.frame()

# Setting existing coordinate as lat-long system
coords <- SpatialPointsDataFrame(coords = set_sm[, c(1, 2)], 
                                  data = set_sm,
                                  proj4string = CRS("+proj=longlat"))

# Transforming coordinate to UTM using the built in projection data from the sp.data shapefile.
# If you aren't familiar, you can open the .prj file as text to see what info this includes.
coords <- spTransform(coords, CRS(proj4string(sp_data)))

points(coords[ , c(1, 2)], col = "red") # this shows where those new stations are on the map

# Run the over command. Could do this not as a list - probably an extra step
# here that could be omitted in the future.
coords <- over(sp_data, coords, returnList = TRUE)          
# tst <- over(coords, sp_data)          

# Convert the output list to a data frame where the name of the index value
# that was matched is saved as a field called "index"
coords <- Reduce(rbind, lapply(names(coords), function(x) coords[[x]] %>% 
                               transmute(lon, lat, fishing_event_id, index = x)))

# Join spatial data from shapefile (which includes the nmfs area names). Note
# that the index values start with a zero because the shapefile matching
# function apparently counts the first row of data as the zero'th row
coords_nmfs <- sp_data@data %>% 
  rownames_to_column(var = "index") %>% 
  mutate(index = as.character(index),
         nmfs = NMFS_AREA) %>% 
  dplyr::select(nmfs, index) %>% 
  full_join(coords)

#the above creates a lot of zero rows, so clean all that out
coords_nmfs <- coords_nmfs[!is.na(coords_nmfs$fishing_event_id), ]

#what remains should only be stations that are within NMFS areas, take a look to double check
plot(sp_data)
axis(1); axis(2)

coords_nmfs2 <- SpatialPointsDataFrame(coords = coords_nmfs[ , c(3, 4)], 
                                 data = coords_nmfs,
                                 proj4string = CRS("+proj=longlat"))

coords_nmfs2 <- spTransform(coords_nmfs2, CRS(proj4string(sp_data)))
points(coords_nmfs2[ , c(1, 2)], col = "red") # should = NMFS areas

# tmp <- coords_nmfs2[coords_nmfs2$fishing_event_id %in% check_3A_CAN,]
# points(tmp[ , c(1, 2)], col = "green") # this shows where those new stations are on the map

# check for fishing events that didn't get NMFS area information assigned
# properly
unique(coords_nmfs[coords_nmfs$nmfs == 0, ]$fishing_event_id)
plot(sp_data)

# NMFS Area 659 (Inside), correct in 2018All_species_full_survey.csv
# points(coords_nmfs2[coords_nmfs2$fishing_event_id == "2018_3216", c(1,2)], pch = 20, col = "green")
# points(coords_nmfs2[coords_nmfs2$fishing_event_id == "2000_3032", c(1,2)], pch = 20, col = "red")
# points(coords_nmfs2[coords_nmfs2$fishing_event_id == "2010_3032", c(1,2)], col = "pink")
# coords_nmfs[coords_nmfs$fishing_event_id %in% c("2018_3216", "2000_3032", "2010_3032"), ]$nmfs <- 659 (old)

# manually assigned in 2021 by plotting each of these points manually, example code above.
coords_nmfs[coords_nmfs$fishing_event_id %in% c("2021_3230", "2018_3216", "2000_3032"), ]$nmfs <- 659 # SEI
coords_nmfs[coords_nmfs$fishing_event_id %in% c("2021_6607", "2017_6604"), ]$nmfs <- 541 # eastern AI

# NFMS Area 610 (WGOA), correct in 2018All_species_full_survey.csv. (old)
# points(coords_nmfs2[coords_nmfs2$fishing_event_id == "2005_6055", c(1,2)], pch = 20, col = "orange")
# coords_nmfs[coords_nmfs$fishing_event_id %in% c("2005_6055"), ]$nmfs <- 610

# Identified as Canada in 2018All_species_full_survey.csv. This should be NMFS
# area 541 
# points(coords_nmfs2[coords_nmfs2$fishing_event_id == "2017_6604", c(1,2)], pch = 20, col = "blue")
# coords_nmfs[coords_nmfs$fishing_event_id %in% c("2017_6604"), ]$nmfs <- 541

# next verify that all of the station dumped in the above steps are actually in
# Canada
lost_st <- merge(set_sm, coords_nmfs, by = c("fishing_event_id", "lon", "lat"), all = TRUE)
lost_st <- lost_st[is.na(lost_st$nmfs), ]
nrow(set_sm) - nrow(coords_nmfs) == nrow(lost_st) # should be TRUE

plot(sp_data)
axis(1); axis(2)

lost_st <- SpatialPointsDataFrame(coords = lost_st[ ,c(2, 3)], 
                                 data = lost_st,
                                 proj4string = CRS("+proj=longlat"))
temp_lost <- spTransform(lost_st, CRS(proj4string(sp_data)))
points(temp_lost[ , c(1,2)], col="red") # should = Canada

# if none of the above stations appear to be within NMFS areas (will deal with
# Puget Sound later in the code) then assign all of them to NMFS area = 0
lost_st$nmfs <- 0

# combine NMFS area stations with Canada stations
coords_canada <- as.data.frame(lost_st) %>% 
  select(nmfs, index, lon, lat, fishing_event_id)
coords <- rbind(coords_nmfs, coords_canada)

# Are any fishing events linked to multiple NMFS areas?
coords %>% 
  group_by(fishing_event_id) %>% 
  summarize(n_nmfs_areas = n_distinct(nmfs)) %>% 
  filter(n_nmfs_areas > 1) -> problem_stations
problem_stations #%>% View() #should be none

# Combine NMFS areas into full data set, get rid of extraneous rows created
# during spatial matching process
set <- coords %>% 
  distinct(fishing_event_id, NMFS_AREA = nmfs) %>% 
  right_join(set)

# FMP ----

# Join FMP, sub area, mgmt area info
set <- set %>% left_join(fmp)

# Puget Sound & West Coast ----

# Puget Sound gets lumped into Canada... this logic should work, but dbl check
# that all these are in IPHC stat area 50
psound_ls <- set %>% filter(NMFS_AREA == 0 & station < 2000) %>% pull(fishing_event_id)
set %>% filter(fishing_event_id %in% psound_ls) %>% distinct(iphcstat) # should be 50 only

# assign FMP info manually 
set <- set %>% 
  mutate(NMFS_AREA = ifelse(fishing_event_id %in% psound_ls, 679, NMFS_AREA), # where did 679 come from?
         FMP_sub_area = ifelse(fishing_event_id %in% psound_ls, "WA", FMP_sub_area),
         NMFS_mgmt_area = ifelse(fishing_event_id %in% psound_ls, "WA", NMFS_mgmt_area),
         FMP = ifelse(fishing_event_id %in% psound_ls, "WA", FMP))

# Code to plot Puget Sound stations:
# pugetsound <- coords %>% 
#   distinct(fishing_event_id, nmfs, lon, lat) %>% 
#   right_join(set) %>% #dim()
#   filter(nmfs == 0 & station < 2000) 
# dim(pugetsound)
# pugetsound %>% distinct(iphcstat, iphcreg)
# plot(sp_data)
# axis(1); axis(2)
# pugetsound2 <- SpatialPointsDataFrame(coords = pugetsound[ ,c(2, 3)], 
#                                   data = pugetsound,
#                                   proj4string = CRS("+proj=longlat"))
# pugetsound2 <- spTransform(pugetsound2, CRS(proj4string(sp_data)))
# points(pugetsound2[ , c(1,2)], col="green")
# pugetsound %>% distinct(iphcstat, iphcreg)
# set %>% filter(iphcstat == 50) %>% dim

# Make all west coast areas one
set <- set %>% 
  mutate(FMP = ifelse(FMP %in% c("ORCA", "WA", "WAOR") | iphcreg == "2A", "WC", FMP),
         FMP_sub_area = ifelse(FMP_sub_area %in% c("ORCA", "WA", "WAOR") | iphcreg == "2A", "WC", FMP_sub_area),
         NMFS_mgmt_area = ifelse(NMFS_mgmt_area %in% c("ORCA", "WA", "WAOR") | iphcreg == "2A", "WC", NMFS_mgmt_area),)

# Other checks ----

# These are other checks that were in the original Survey_database_2018update.R
# script

# duplicate fishing events, or stations that occurred more than once in a year
set %>% 
  distinct(fishing_event_id, hauldate) %>% 
  count(fishing_event_id) %>% 
  filter(n > 1) # should be zero

# nmfs = 0 should only be Canada
set %>% filter(NMFS_AREA == 0) %>% distinct(FMP) 

# should only be station 2172, which is always in GOA waters and right on the
# Dixon Entrance line -- leave as is
set %>% filter(iphcreg == "2B" & NMFS_AREA == 650) %>% distinct(station)

# These stations in Inside waters SEAK have been problematic in the past. Double
# check that there are no errors.
set %>% filter(station %in% c(3023, 3122) & NMFS_AREA == 0) 

# Oddball areas ----

set %>% distinct(iphcreg, NMFS_mgmt_area) # make sure everything looks correct, exceptions below

# Notes:

# (1) [new in 2022 data] iphcreg=="4B" not defined as NMFS_mgmt_area=="AI"
set %>% filter(iphcreg=="4B" & NMFS_mgmt_area=="AI") %>% nrow
set %>% filter(iphcreg=="4B" & is.na(NMFS_mgmt_area)) %>% nrow
set %>% filter(iphcreg=="4B" & is.na(NMFS_mgmt_area)) 
# don't know why these are the way they are. fix them.
set <- set %>% 
  mutate(NMFS_mgmt_area = ifelse(iphcreg=="4B" & is.na(NMFS_mgmt_area), "AI", NMFS_mgmt_area))

# (2) Prince William Sound defined as iphcreg == "3A" & NMFS_mgmt_area == "INSIDE")

# (3) IPHC 2C areas in Canada: these are stations that hug the SEAK / Canadian
# boundary. These aren't errors
check_2C_CAN <- set %>% filter(iphcreg == "2C" & NMFS_mgmt_area == "CAN")
tst <- coords %>%
  filter(fishing_event_id %in% unique(check_2C_CAN$fishing_event_id))
# tst2 <- SpatialPointsDataFrame(coords = tst[ , c(3, 4)],
#                                data = tst,
#                                proj4string = CRS("+proj=longlat"))
# tst2 <- spTransform(tst2, CRS(proj4string(sp_data)))
# plot(sp_data)
# axis(1); axis(2)
# points(tst2[ , c(1, 2)], col = "green")

# (4) IPHC 3A areas in Canada? This was an early issue that should be fixed now.
# check_3A_CAN <- set %>% filter(iphcreg == "3A" & NMFS_mgmt_area == "CAN")
# tst <- coords %>%
#   filter(fishing_event_id %in% unique(check_3A_CAN$fishing_event_id))
# tst2 <- SpatialPointsDataFrame(coords = tst[ , c(3, 4)],
#                                data = tst,
#                                proj4string = CRS("+proj=longlat"))
# tst2 <- spTransform(tst2, CRS(proj4string(sp_data)))
# points(tst2[ , c(1, 2)], col = "purple")

# Area size info ----

# Extract cols used for analysis
areasize <- areasize %>% 
  select(FMP_sub_area, RPN_strata, fmp_depth = concat_meters, area_kmsq)

# Join to set data using unique combinations of FMP_sub_area and depth bin,
# formerly concat_meters
set <- set %>% 
  mutate(fmp_depth = paste0(FMP_sub_area, depbin_m)) %>% 
  left_join(areasize) 

# Check - the only RPN_strata with NAs should be Canada (CAN), West Coast (WC),
# and Inside waters SEAK (INSIDE)
set %>% 
  filter(is.na(RPN_strata)) %>%
  distinct(FMP_sub_area) %>% 
  arrange(FMP_sub_area) %>% 
  pull(FMP_sub_area) == c("CAN", "INSIDE", "WC") # should be TRUE, TRUE, TRUE but its not

# New in 2022, there is an NA in FMP_sub_area... it's the same five sets that
# affected issue number 1 above where iphcreg=="4B" not defined as
# NMFS_mgmt_area=="AI"
set %>% filter(is.na(RPN_strata) & is.na(FMP_sub_area))
set <- set %>% 
  mutate(FMP_sub_area = ifelse(is.na(RPN_strata) & is.na(FMP_sub_area), "AI", FMP_sub_area))
set %>% 
  filter(is.na(RPN_strata) & year==2022 & FMP_sub_area == "AI") 

# manually fix these five rows for 2022, knowing that the IPHC will likely fix
# these errors in the future.
set <- set %>% 
  mutate(FMP = ifelse(is.na(RPN_strata) & year==2022 & FMP_sub_area == "AI", "AI", FMP),
         fmp_depth = ifelse(is.na(RPN_strata) & year==2022 & FMP_sub_area == "AI", "AI400", fmp_depth),
         area_kmsq = ifelse(is.na(RPN_strata) & year==2022 & FMP_sub_area == "AI", 12978, area_kmsq),
         RPN_strata = ifelse(is.na(RPN_strata) & year==2022 & FMP_sub_area == "AI", "AI300500", RPN_strata))
  
# new 2022 (for 2021 data): changes to survey design ----

# reductions in BS, AI, increases in
names(set)

set %>% 
  group_by(year, FMP_sub_area) %>% 
  summarize(n_sets = length(unique(fishing_event_id))) %>% 
  ungroup() %>% 
  complete(year, FMP_sub_area, fill = list(n_sets = 0)) %>% 
  # filter(!FMP_sub_area %in% c('WC', 'INSIDE')) %>% 
  ggplot(aes(x = year, y = n_sets)) +
  geom_point() + 
  geom_line() +
  facet_wrap(~FMP_sub_area)

unique(set$RPN_strata)

set %>% 
  filter(!FMP_sub_area %in% c('WC', 'INSIDE', 'CAN', 'BS', 'AI')) %>%
  group_by(year, RPN_strata) %>% 
  summarize(n_sets = length(unique(fishing_event_id))) %>% 
  ungroup() %>% 
  complete(year, RPN_strata, fill = list(n_sets = 0)) %>% 
  ggplot(aes(x = year, y = n_sets)) +
  geom_point(size = 2) + 
  geom_line() +
  facet_wrap(~RPN_strata)

ggplot(set %>% 
         filter(!FMP_sub_area %in% c('WC', 'CAN')) %>%
         filter(year >= 2016) %>% 
         mutate(startlon = ifelse(startlon > 0, startlon - 360, startlon))) +
  geom_point(aes(x = startlon, y = startlat, col = FMP_sub_area)) +
  facet_wrap(~year)

# Issues:
# increase 2019-2021 in CG0
# decrease 2020-2021 in CG100
# decrease 2020-2021 in CG200

# Write data ----

# set data
write_csv(set, paste0("output/", YEAR, "/final_iphc_survey_", FIRST_YEAR, "_", YEAR, ".csv"))

# removed data
removed_data %>% count(reason)
removed_data <- removed_data %>% distinct()
write_csv(removed_data, paste0("output/", YEAR, "/removed_data_", FIRST_YEAR, "_", YEAR, ".csv"))
