# CPUE for ADFG SEAK Groundfish Project
# Contacts: jane.sullivan@noaa.gov 
# Last update: Oct 2020

# Set up ----

libs <- c("tidyverse", "rgdal", "boot")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# Create a subdirectory for ADFG annual output
out_path <- paste0("output/", YEAR, "/ADFG")
dir.create(out_path)

# User defined variable
FIRST_YEAR <- 1998 # first year of survey data used (currently exclude 1997)
YEAR <- 2021 # most recent year of data

OBS_OR_EXTRAP = "EXTRAP" # tmp flag to use observed or extrapolated cpue
ITER = 1500

# Read data ----

# Cleaned IPHC survey data (all species), output from 1_compile_raw_iphc_data.R
set <- read_csv(paste0("output/", YEAR, "/final_iphc_survey_", FIRST_YEAR, "_", YEAR, ".csv"),
                guess_max = 1e5) %>% 
  filter(year >= FIRST_YEAR)

# Shapefiles ----

# Contact for shapefiles: Kellii.Wood@alaska.gov

# land <- readOGR(dsn = "data/shapefiles/p3alaska_NAD83stateplane",layer = "p3alaska_NAD83stateplane")
sp_data <- readOGR(dsn = "data/shapefiles/SEAK_MGMNT_AREAS",layer = "SEAK_MGMNT_AREAS")

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

points(coords[ , c(1, 2)], col = "red", pch = 20) # this shows where those new stations are on the map

# Run the over command. Could do this not as a list - probably an extra step
# here that could be omitted in the future.
coords <- over(sp_data, coords, returnList = TRUE)          
# tst <- over(coords, sp_data)          

# Convert the output list to a data frame where the name of the index value
# that was matched is saved as a field called "index"
coords <- Reduce(rbind, lapply(names(coords), function(x) coords[[x]] %>% 
                                 transmute(lon, lat, fishing_event_id, index = x)))

# Join spatial data from shapefile (which includes the adfg area names). 
coords_adfg <- sp_data@data %>% 
  rownames_to_column(var = "index") %>% 
  mutate(index = as.character(index),
         adfg_area = G_SEL_AREA) %>% 
  dplyr::select(adfg_area, index) %>% 
  full_join(coords)

#the above creates a lot of zero rows, so clean all that out
coords_adfg <- coords_adfg[!is.na(coords_adfg$fishing_event_id), ]

#what remains should only be stations that are within NMFS areas, take a look to double check
plot(sp_data)
axis(1); axis(2)

coords_adfg2 <- SpatialPointsDataFrame(coords = coords_adfg[ , c(3, 4)], 
                                       data = coords_adfg,
                                       proj4string = CRS("+proj=longlat"))

coords_adfg2 <- spTransform(coords_adfg2, CRS(proj4string(sp_data)))
points(coords_adfg2[ , c(1, 2)], col = "red") # should = adfg areas

# check for fishing events that didn't get adfg area information assigned
# properly
unique(coords_adfg[coords_adfg$nmfs == 0, ]$fishing_event_id) # should be 0

# Are any fishing events linked to multiple adfg areas?
coords_adfg %>% 
  group_by(fishing_event_id) %>% 
  dplyr::summarize(n_areas = n_distinct(adfg_area)) %>% 
  filter(n_areas > 1) -> problem_stations
problem_stations #%>% View() #should be none

# Combine adfg areas into full data set, get rid of extraneous rows created
# during spatial matching process
set <- coords_adfg %>% 
  distinct(fishing_event_id, adfg_area) %>%
  right_join(set) 

# Hold data
full_set <- set %>% 
  filter(!is.na(adfg_area)) 

write_csv(full_set, paste0("output/", YEAR, "/ADFG/seak_iphc_clean_1998_", YEAR, ".csv"))

# depth profiles - Yelloweye
full_set %>% 
  filter(species == "Rockfish_Yelloweye") %>%
  distinct(adfg_area, fishing_event_id, avgdep_m) %>% 
  mutate(set = "Sets where YE were present") %>% 
  bind_rows(full_set %>% 
              distinct(adfg_area, fishing_event_id, avgdep_m) %>% 
              mutate(set = "All sets")) %>% 
  ggplot(aes(x = avgdep_m, col = set, fill = set)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~adfg_area) +
  labs(x = "Average depth of set (m)", y = "Density", col = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave(paste0("output/", YEAR, "/ADFG/depth_distributions_yelloweye.png"))

# Depth profiles
full_set %>% 
  filter(species == "Rockfish_Yelloweye") %>%
  distinct(adfg_area, fishing_event_id, avgdep_m) %>% 
  mutate(set = "Sets where YE were present") %>% 
  ggplot(aes(x = avgdep_m, col = set, fill = set)) +
  geom_histogram(alpha = 0.4) +
  facet_wrap(~adfg_area) +
  labs(x = "Average depth of set (m)", y = "Count", col = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave(paste0("output/", YEAR, "/ADFG/depth_distributions_yelloweye.png"))
# look up table ensure all sets are accounted for, even when the species
# wasn't observed
full_fishing_events <- full_set %>% 
  distinct(fishing_event_id, adfg_area, year, 
           obs_effhks, obs_ineffhks, ex_effhks, ex_ineffhks)

# For species/assessment that exist across the full range of the IPHC survey,
# use the full data set. Otherwise, define subsets before running the functions.

# CPUE fxn ----

calc_iphc_indices <- function(COMMON_NAME) { # species of interest
  
  # Create a year/species subdirectory
  out_path <- paste0("output/", YEAR, "/ADFG/", COMMON_NAME)
  dir.create(out_path)
  
  # isolate species of interest, join
  dat <- set %>% 
    filter(species == COMMON_NAME) %>% 
    # complete data set with 0 catch observations
    right_join(fishing_events) %>% 
    replace_na(list(species = COMMON_NAME, obs_catch = 0)) 
  
  # station level CPUE and extrapolated catch
  dat <- dat %>% 
    mutate(obs_cpue = obs_catch / obs_effhks,
           ex_catch = obs_cpue * ex_effhks) %>% 
    group_by(fishing_event_id) %>% 
    # FLAG Use observed or extrapolated catch for the CPUE/RPN calculations?
    # Currently use observed in RPN and extrapolated in CPUE
    mutate(catch_used = ifelse(OBS_OR_EXTRAP == "OBS", obs_catch, ex_catch),
           effhks_used = ifelse(OBS_OR_EXTRAP == "OBS", obs_effhks, ex_effhks)) %>% 
    ungroup()
  
  # CPUE ----
  
  # The CPUE analysis is conducted at different spatial scales depending
  # on what's desired
  cpue_dat <- dat %>% mutate(area_combo = "ADFG management area", area = adfg_area) %>% 
    filter(!is.na(adfg_area)) %>% 
    select(-FMP, -NMFS_mgmt_area, -FMP_sub_area, -RPN_strata, -adfg_area, -obs_ineffhks, -ex_ineffhks)
  
  
  # FLAG Get rid of this chunk once we've troubleshooted these issues
  area_dat <- cpue_dat  %>% 
    # FLAG - in the original code the mean of the tot_eff_hks was taken, comment
    # saying that's necessary when running code for more than one spp. I'm not
    # sure about that. There should be only one row per species/species group per
    # fishing event, we have a check for that in the cleaning script. 
    # distinct(area_combo, species, fishing_event_id, year, area, catch, effhks) %>% 
    group_by(area_combo, species, year, area) %>% 
    dplyr::summarise(n_stations = length(unique(fishing_event_id)),
                     n_pos_catch = length(which(catch_used > 0)), # sum(nobs > 0))
                     area_catch = sum(catch_used),
                     # FLAG - the original code sums the tot_ineff_hks and
                     # then subtracts them from the extrapolated effective hooks
                     # in the myCPUE function (line ~ 227). We've already done
                     # that step (eff_hks = hkretriev - ex_ineff) so I think this is
                     # doubling the ineffective hook correction.
                     area_effhks = sum(effhks_used)) %>% 
    ungroup() %>% 
    mutate(area_cpue = area_catch / area_effhks)
  
  # Bootstrap CPUE ----
  
  cpue_results <- cpue_dat %>% 
    group_by(species, area_combo, year, area) %>% 
    summarize(n_stations = length(unique(fishing_event_id)), # number of sets/stations
              n_pos_catch = length(which(catch_used > 0)), # number of sets/stations with positive catch
              area_cpue = sum(catch_used) / sum(effhks_used)) # raw mean CPUE
  
  # Create flag to not bootstrap if sample sizes are too small or catch is 0 
  cpue_dat <- cpue_dat %>% 
    group_by(species, area_combo, year, area) %>%
    mutate(boot_tst = ifelse(length(unique(fishing_event_id)) > 1 & sd(obs_catch) > 0 & sum(obs_catch) > 0, 1, 0)) %>% 
    ungroup()
  
  # Convert to list so you can bootstrap across multiple groups simultaneously
  # using purrr package
  nest_cpue <- cpue_dat %>% 
    filter(boot_tst == 1) %>% 
    tidyr::nest(data = c(-species, -area_combo, -year, -area))
  
  # lapply(nest_cpue, head)
  
  calc_cpue <- function(d, i) {
    sum(d$catch_used[i]) / sum(d$effhks_used[i])
  }
  
  nest_cpue <- nest_cpue %>% 
    mutate(boot_cpue = map(.x = data, ~ boot(data = .x, statistic = calc_cpue, R = ITER)), # bootstrap object (contains stats and replicates)
           boot_area_cpue = map(.x = boot_cpue, ~ mean(.x$t)), # bootstrap mean cpue
           boot_bias = map(.x = boot_cpue, ~ mean(.x$t) - .x$t0), # bias (difference between bootstrap and original mean)
           boot_sd = map(.x = boot_cpue, ~ sd(.x$t)), # std error of bootstrap estimate = sd of bootstrap realizations (t)
           boot_ci = map(.x = boot_cpue, ~ boot.ci(.x, conf = 0.95, type = "bca")), # boot CI object
           boot_lci = map(.x = boot_ci, ~ .x$bca[[4]]), # lower 2.5% limit
           boot_uci = map(.x = boot_ci, ~ .x$bca[[5]])) # upper 97.5% limit
  
  # plots <- map(.x = nest_cpue$boot_cpue, ~ plot(.x)) # diagnostic plots
  
  # Drop boot objects and raw data, save output
  cpue_results <- cpue_results %>% 
    left_join(nest_cpue %>%
                select(-data, -boot_cpue, -boot_ci) %>%
                tidyr::unnest(cols = c(boot_area_cpue, boot_sd, boot_lci, boot_uci, 
                                       boot_sd, boot_bias))) %>% 
    replace(is.na(.), 0)  # replace 0s as needed
  
  cpue_results %>% write_csv(paste0(out_path, "/CPUE_", COMMON_NAME, "_", YEAR,
                                    # "_", OBS_OR_EXTRAP, 
                                    ".csv"))
  
  nest_results <- cpue_results %>% 
    # mutate(area = factor(area, levels = c("BSAI","BS","AI","GOA","WGOA","CGOA","EGOA","INSIDE","CAN","WC"))) %>% 
    droplevels() %>% 
    nest(data = c(-area_combo)) %>% 
    mutate(plot = map2(.x = data, .y = area_combo, 
                       ~ ggplot(data = .x, aes(x = year, y = boot_area_cpue, col = area)) +
                         geom_point() +
                         geom_line() +
                         geom_errorbar(aes(ymin = boot_lci, ymax = boot_uci), width = 0.2) +
                         facet_wrap(~ area, scales = "free_y", ncol = 1) +
                         ggtitle(label = paste0(COMMON_NAME, " CPUE (+/- 95% bootstrap CI)"), 
                                 subtitle = paste0("by ", .y)) +
                         labs(x = NULL, y = "Number per effective hook") +
                         theme_bw() +
                         theme(legend.position = "none")),
           plot_names = map(.x = area_combo, ~ paste0(COMMON_NAME, " CPUE ", .x,
                                                      # " ", OBS_OR_EXTRAP, 
                                                      ".pdf")))
  
  # Save plots (purrr::walk applies function)
  walk2(.x = nest_results$plot_names, .y = nest_results$plot, 
        ~ ggsave(filename = paste0(out_path, "/", .x), plot = .y, height = 10, width = 6))
  
  print(nest_results$plot)
}

# Yelloweye rockfish ----
full_set %>% filter(spp_race %in% c(30470) | grepl(c("yelloweye|Yelloweye"), species)) %>% 
  count(species, spp_iphc, spp_race) # %>% print(n = Inf)
set <- full_set %>% mutate(species = ifelse(spp_race %in% c(30470), "Yelloweye rockfish", species))
set %>% filter(species == "Yelloweye rockfish") %>% count(species, adfg_area) # filter out NSEI and NSEO?
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Yelloweye rockfish")

# Lingcod ----
full_set %>% filter(spp_race %in% c(21910) | grepl(c("lingcod|Lingcod"), species)) %>% 
  count(species, spp_iphc, spp_race)
set <- full_set %>% mutate(species = ifelse(spp_race %in% c(21910), "Lingcod", species))
set %>% filter(species == "Lingcod") %>% count(species, adfg_area) # filter out NSEI, NSEO, SSEI?
set <- set %>% filter(!(FMP_sub_area %in% c("BS", "AI", "WGOA"))) # low sample sizes
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Lingcod")

# Pacific cod ----
full_set %>% filter(grepl(c("Cod|cod|Pcod|pcod"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(species == "Cod_Pacific", "Pacific cod", species)) #%>% 
set %>% filter(species == "Pacific cod") %>% count(species, adfg_area) 
set <- set %>% filter(adfg_area != "NSEO") # very low sample size
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Pacific cod")

# Sablefish ----
full_set %>% filter(species == "Sablefish") %>% distinct(species, spp_iphc, spp_race) # check codes
set <- full_set %>% filter(adfg_area %in% c("NSEI", "SSEI"))
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Sablefish")

# Skates ----

# Maybe an index for skates is useful?

full_set %>% filter(grepl(c("skate|Skate"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(grepl(c("skate|Skate"), species), "All skates", species))
set %>% filter(species == "All skates") %>% count(species, adfg_area)
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "All skates")

# Longnose skate ----
full_set %>% filter(grepl(c("Longnose"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(spp_race == 440, "Longnose skate", species))
set %>% filter(species == "Longnose skate") %>% count(species, adfg_area) 
set <- set %>% filter(adfg_area != "NSEO") # low sample size
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Longnose skate")

# Spiny dogfish ----
full_set %>% filter(grepl(c("Spiny|dogfish"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(spp_race == 310, "Spiny dogfish", species))
set %>% filter(species == "Spiny dogfish") %>% count(species, adfg_area) 
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Spiny dogfish")

# Redbanded rockfish ----
full_set %>% filter(grepl(c("Redbanded"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(spp_race == 30475, "Redbanded rockfish", species))
set %>% filter(species == "Redbanded rockfish") %>% count(species, adfg_area) 
set <- set %>% filter(!adfg_area %in% c("NSEO", "IBS")) # low sample sizes
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Redbanded rockfish")

# Big skate ----
full_set %>% filter(grepl(c("Big"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(spp_race == 420, "Big skate", species))
set %>% filter(species == "Big skate") %>% count(species, adfg_area) 
set <- set %>% filter(adfg_area %in% c("EYKT", "IBS")) # only in a couple areas
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Big skate")

# Shortspine thornyhead ----
full_set %>% filter(grepl(c("Shortspine"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(spp_race == 30020, "Shortspine thornyhead", species))
set %>% filter(species == "Shortspine thornyhead") %>% count(species, adfg_area) 
set <- set %>% filter(adfg_area %in% c("NSEI", "SSEI")) # only in a couple areas
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Shortspine thornyhead")

# Quillback rockfish ----
full_set %>% filter(grepl(c("Quillback"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(spp_race == 30320, "Quillback rockfish", species))
set %>% filter(species == "Quillback rockfish") %>% count(species, adfg_area) 
set <- set %>% filter(adfg_area %in% c("SSEI")) # may only be useful for SSEI
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Quillback rockfish")

# Silvergray rockfish ----
full_set %>% filter(grepl(c("Silvergray"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(spp_race == 30100, "Silvergray rockfish", species))
set %>% filter(species == "Silvergray rockfish") %>% count(species, adfg_area) 
set <- set %>% filter(adfg_area %in% c("SSEO")) # may only be useful for SSEO
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Silvergray rockfish")

# Shortraker rockfish ----
full_set %>% filter(grepl(c("Shortraker"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(spp_race == 30576, "Shortraker rockfish", species))
set %>% filter(species == "Shortraker rockfish") %>% count(species, adfg_area) 
set <- set %>% filter(adfg_area %in% c("IBS", "NSEI", "SSEI")) # may only be useful for a few areas
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Shortraker rockfish")

# Rougheye rockfish ----
full_set %>% filter(grepl(c("Rougheye"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(spp_race == 30050, "Rougheye rockfish", species))
set %>% filter(species == "Rougheye rockfish") %>% count(species, adfg_area) 
set <- set %>% filter(adfg_area %in% c("NSEI", "SSEI")) # may only be useful for NSEI/SSEI
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Rougheye rockfish")

# Sleeper shark ----
full_set %>% filter(grepl(c("Sleeper"), species)) %>% count(species, spp_iphc, spp_race) # codes
set <- full_set %>% mutate(species = ifelse(spp_race == 320, "Sleeper shark", species))
set %>% filter(species == "Sleeper shark") %>% count(species, adfg_area) 
set <- set %>% filter(adfg_area %in% c("NSEI")) # may only be useful for NSEI
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "Sleeper shark")

# Other -----

# all spp
full_set %>%  count(species, spp_iphc, spp_race) %>% 
  filter(n > 200) %>% 
  arrange(-n) #%>% print(n = Inf)

# rockfish
full_set %>% filter(grepl(c("rockfish|Rockfish"), species)) %>% 
  count(species, spp_iphc, spp_race) %>% 
  arrange(-n) %>% print()

# DSR ----
full_set %>% filter(grepl(c("Yelloweye|Quillback|Copper|Rosethorn|China|Canary|Tiger"), species)) %>% count(species, spp_iphc, spp_race) # codes
dsr_codes <- full_set %>% filter(grepl(c("Yelloweye|Quillback|Copper|Rosethorn|China|Canary|Tiger"), species)) %>% distinct(spp_race) %>% pull # codes
set <- full_set %>% mutate(species = ifelse(spp_race %in% dsr_codes, "DSR", species))
set %>% filter(species == "DSR") %>% count(species, adfg_area) 
fishing_events <- full_fishing_events %>% filter(fishing_event_id %in% unique(set$fishing_event_id))
calc_iphc_indices(COMMON_NAME = "DSR")
