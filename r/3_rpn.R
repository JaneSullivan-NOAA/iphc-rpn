# IPHC Relative Population Numbers by Species/Complex
# Cindy Tribuzio, Jane Sullivan
# Aug 2020

libs<-c("tidyverse","boot")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )])>0){install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE )])}
lapply(libs,library,character.only=T)

# '%nin%' <- Negate('%in%')

# Set up ----

FIRST_YEAR <- 1998
YEAR <- 2018

# Function specific inputs 
RACE_CODE = 20510
COMMON_NAME = "Sablefish" # "Sablefish (Blackcod)"
ITER = 1500
SAVE_OUTPUT = TRUE

# Read data ----

set <- read_csv(paste0("output/", YEAR, "/final_iphc_survey_", FIRST_YEAR, "_", YEAR, ".csv")) %>% 
  select(fishing_event_id, year, NMFS_mgmt_area, FMP, RPN_strata, area_kmsq, 
         hksobs, nobs, obs_ineffhks, ex_effhks, ex_ineffhks, species)

# look up table ensure all sets are accounted for, even when the species
# wasn't observed
fishing_events <- set %>% 
  distinct(fishing_event_id, year, NMFS_mgmt_area, 
           FMP, RPN_strata, area_kmsq, hksobs, obs_ineffhks, ex_effhks, ex_ineffhks)

# Subset ----

# isolate species of interest, join
dat <- set %>% 
  filter(species == COMMON_NAME) %>% 
  # complete data set with 0 catch observations
  right_join(fishing_events) %>% 
  replace_na(list(species = COMMON_NAME, nobs = 0))

# Area combos ----

# The CPUE analysis is conducted at different spatial scales depending
# on what's desired
dat <- dat %>% mutate(area_combo = "FMP_withINSIDE",
                      area = FMP) %>% 
  bind_rows(dat %>% mutate(area_combo = "FMP_withoutINSIDE",
                           area = FMP) %>% 
              filter(NMFS_mgmt_area != "INSIDE")) %>% 
  bind_rows(dat %>% mutate(area_combo = "NMFS_mgmt_areas",
                     area = NMFS_mgmt_area)) %>% 
  select(-FMP, -NMFS_mgmt_area)

# CPUE/Catch ----

station_dat <- dat %>% 
  mutate(obs_effhks = hksobs - obs_ineffhks,
         obs_cpue = nobs / obs_effhks,
         ex_catch = obs_cpue * ex_effhks)

area_dat <- station_dat  %>% 
  # FLAG - in the original code the mean of the tot_eff_hks was taken, comment
  # saying that's necessary when running code for more than one spp. I'm not
  # sure about that. There should be only one row per species/species group per
  # fishing event
  distinct(area_combo, species, fishing_event_id, year, area, ex_effhks, nobs, ex_catch) %>%
  group_by(area_combo, species, year, area) %>% 
  dplyr::summarise(n_set = length(unique(fishing_event_id)),
                   n_pos_catch = length(which(nobs > 0)), # sum(nobs > 0))
                   area_ex_catch = sum(ex_catch),
                   # FLAG - the original code sums the tot_ineff_hks and
                   # then subtracts them from the extrapolated effective hooks
                   # in the myCPUE function (line ~ 227). We've already done
                   # that step (eff_hks = hkretriev - ex_ineff) so I think this is
                   # doubling the ineffective hook correction.
                   area_ex_effhks = sum(ex_effhks)) %>% 
  ungroup() %>% 
  mutate(mean_cpue = area_ex_catch / area_ex_effhks)

# Bootstrap CPUE ----

# TODO : figure out what to do if sample sizes are too small or catch is 0

calc_cpue <- function(d, i) {
  sum(d$tot_catch[i]) / sum(d$tot_eff_hks[i])
}

# Convert to list so you can bootstrap across multiple groups simultaneously
# using purrr package
nested_dat <- station_dat %>% 
  tidyr::nest(data = c(-species, -area_combo, -year, -area))

lapply(nested_dat, head)

nested_dat <- nested_dat %>% 
  mutate(boot_cpue = map(.x = data, ~ boot(data = .x, statistic = calc_cpue, R = ITER)),
         boot_mean_cpue = map(.x = boot_cpue, ~ mean(.x$t)), # bootstrap mean
         boot_bias = map(.x = boot_cpue, ~ mean(.x$t) - .x$t0), # bias (difference between bootstrap and original mean)
         boot_sd = map(.x = boot_cpue, ~ sd(.x$t)), # std error of bootstrap estimate = sd of bootstrap realizations (t)
         boot_ci = map(.x = boot_cpue, ~ boot.ci(.x, conf = 0.95, type = "bca")),
         boot_lci = map(.x = boot_ci, ~ .x$bca[[4]]), # lower 2.5% limit
         boot_uci = map(.x = boot_ci, ~ .x$bca[[5]])) # upper 97.5% limit

nested_dat2 <- nested_dat %>%
  select(-data, -boot_cpue, -boot_ci) %>% 
  unnest(cols = c(species, boot_mean_cpue, boot_lci, boot_uci, boot_bias, boot_sd)) %>% 
  left_join(area_dat)
View(nested_dat2)
