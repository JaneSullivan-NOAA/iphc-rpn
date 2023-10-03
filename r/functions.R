# Function that calculates CPUE at multiple spatial scales and Relative
# Population Numbers (RPN) using IPHC FISS data
# Contacts: jane.sullivan@noaa.gov or cindy.tribuzio@noaa.gov
# Last updated: Sep 2020

calc_iphc_indices <- function(COMMON_NAME) { # species of interest
  
  # COMMON_NAME = "Sablefish"
  options(warn=-1) # turns warnings off...
  # options(warn=0) # probably not a good idea, use this code to turn warnings back on
  
  # Create a year/species subdirectory
  out_path <- paste0("output/", YEAR, "/", COMMON_NAME)
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
  cpue_dat <- dat %>% mutate(area_combo = "FMP (with Inside waters)", area = FMP) %>% 
    bind_rows(dat %>% mutate(area_combo = "FMP (without Inside waters)", area = FMP) %>% 
                filter(NMFS_mgmt_area != "INSIDE")) %>% 
    bind_rows(dat %>% mutate(area_combo = "NMFS management area", area = NMFS_mgmt_area)) %>% 
    select(-FMP, -NMFS_mgmt_area, -FMP_sub_area, -RPN_strata, -obs_ineffhks, -ex_ineffhks)
  
  
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
                                       boot_sd, boot_bias))) %>% #View
    replace(is.na(.), 0)  # replace 0s as needed
  
  cpue_results %>% write_csv(paste0(out_path, "/CPUE_", COMMON_NAME, "_", YEAR, "_", OBS_OR_EXTRAP, ".csv"))
  
  nest_results <- cpue_results %>% 
    mutate(area = factor(area, levels = c("BSAI","BS","AI","GOA","WGOA","CGOA","EGOA","INSIDE","CAN","WC"))) %>% 
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
                         scale_y_continuous(labels = scales::comma) +
                         theme(legend.position = "none")),
           plot_names = map(.x = area_combo, ~ paste0(COMMON_NAME, " CPUE ", .x, " ", OBS_OR_EXTRAP, ".pdf")))
  
  # Save plots (purrr::walk applies function)
  walk2(.x = nest_results$plot_names, .y = nest_results$plot, 
        ~ ggsave(filename = paste0(out_path, "/", .x), plot = .y, height = 10, width = 6))
  
  print(nest_results$plot)
  
  # RPN ----
  
  # FLAG - Currently bootstrap at strata level, then sum up to FMP. Alternative:
  # bootstrap at FMP_sub_area using RPN_strata as strata in the boot fxn
  
  rpn_dat <- dat %>% 
    filter(!is.na(RPN_strata)) %>% 
    select(-c(NMFS_mgmt_area, FMP))
  
  # summarize
  rpn_results <- rpn_dat %>%
    group_by(species, year, FMP_sub_area, RPN_strata) %>%
    summarize(n_stations = length(unique(fishing_event_id)), # number of sets/stations
              n_pos_catch = length(which(catch_used > 0)), # number of sets/stations with positive catch
              strata_rpn = sum(catch_used) / sum(effhks_used) * unique(area_kmsq)) # raw mean RPN
  
  # Create flag to not bootstrap if sample sizes are too small or catch is 0 
  rpn_dat <- rpn_dat %>% 
    group_by(species, year, FMP_sub_area, RPN_strata) %>%
    mutate(boot_tst = ifelse(length(unique(fishing_event_id)) > 1 & sd(obs_catch) > 0 & sum(obs_catch) > 0, 1, 0)) %>% 
    ungroup()
  
  # Convert to list so you can bootstrap across multiple groups simultaneously
  # using purrr package
  nest_rpn <- rpn_dat %>% 
    filter(boot_tst == 1) %>% 
    tidyr::nest(data = c(-species, -year, -FMP_sub_area, -RPN_strata))
  
  # lapply(nest_rpn, head) # visualize list structure
  
  calc_rpn <- function(d, i) sum(d$catch_used[i]) / sum(d$effhks_used[i]) * unique(d$area_kmsq)
  
  nest_rpn <- nest_rpn %>% 
    mutate(boot_rpn = map(.x = data, ~ boot(data = .x, statistic = calc_rpn, R = ITER)), # bootstrap object (contains stats and replicates)
           boot_strata_rpn = map(.x = boot_rpn, ~ mean(.x$t)), # bootstrap mean rpn
           boot_bias = map(.x = boot_rpn, ~ mean(.x$t) - .x$t0), # bias (difference between bootstrap and original mean)
           boot_sd = map(.x = boot_rpn, ~ sd(.x$t)), # std error of bootstrap estimate = sd of bootstrap realizations (t)
           boot_ci = map(.x = boot_rpn, ~ boot.ci(.x, conf = 0.95, type = "bca")), # boot CI object
           boot_lci = map(.x = boot_ci, ~ .x$bca[[4]]), # lower 2.5% limit
           boot_uci = map(.x = boot_ci, ~ ifelse(is.logical(.x), 0, .x$bca[[5]]))) # upper 97.5% limit
  
  # plots <- map(.x = nest_rpn$boot_rpn, ~ plot(.x)) # diagnostic plots
  
  # Drop boot objects and raw data, save output
  rpn_results <- rpn_results %>% 
    left_join(nest_rpn %>%
                select(-data, -boot_rpn, -boot_ci) %>%
                tidyr::unnest(cols = c(boot_strata_rpn, boot_sd, boot_lci, boot_uci, 
                                       boot_sd, boot_bias))) %>% 
    replace(is.na(.), 0) %>% # replace 0s as needed
    # sum over FMP
    group_by(species, FMP_sub_area, year) %>% 
    mutate(fmp_rpn = sum(boot_strata_rpn),
           fmp_lci = sum(boot_lci),
           fmp_uci = sum(boot_uci)) %>% 
    arrange(year, FMP_sub_area, RPN_strata)
  
  rpn_results %>% write_csv(paste0(out_path, "/RPN_", COMMON_NAME, "_", YEAR, "_", OBS_OR_EXTRAP, ".csv"))
  
  nest_results <- rpn_results %>% 
    distinct(species, year, FMP_sub_area, fmp_rpn, fmp_lci, fmp_uci) %>% 
    mutate(FMP_sub_area = factor(FMP_sub_area, levels = c("BS","AI","WGOA","CGOA","WY","EY/SE"))) %>% 
    droplevels() %>% 
    nest(data = c(-species)) %>% 
    mutate(plot = map(.x = data, 
                      ~ ggplot(data = .x, aes(x = year, y = fmp_rpn, col = FMP_sub_area)) +
                        geom_point() +
                        geom_line() +
                        geom_errorbar(aes(ymin = fmp_lci, ymax = fmp_uci), width = 0.2) +
                        facet_wrap(~ FMP_sub_area, scales = "free_y", ncol = 1) +
                        ggtitle(label = paste0(COMMON_NAME, " Relative Population Number\n(+/- 95% bootstrap CI)")) +
                        labs(x = NULL, y = "RPN") +
                        theme_bw() +
                        scale_y_continuous(labels = scales::comma) +
                        theme(legend.position = "none")),
           plot_names = map(.x = species, ~ paste0(COMMON_NAME, "_IPHC_RPN.pdf")))
  
  # Save plots (purrr::walk applies function)
  walk2(.x = nest_results$plot_names, .y = nest_results$plot, 
        ~ ggsave(filename = paste0(out_path, "/", .x), plot = .y, height = 10, width = 6))
  
  print(nest_results$plot)
}

# Future development ----

# Potential development using data.table to explore in future years: 
# https://stackoverflow.com/questions/48886326/r-bootstrap-weighted-mean-by-group-with-data-table

# rpn_dat <- dat %>% 
#   filter(!is.na(RPN_strata)) %>% 
#   select(-c(NMFS_mgmt_area, FMP))
# 
# # summarize
# rpn_results <- rpn_dat %>%
#   group_by(species, year, FMP_sub_area, RPN_strata) %>%
#   summarize(n_stations = length(unique(fishing_event_id)), # number of sets/stations
#             n_pos_catch = length(which(catch_used > 0)), # number of sets/stations with positive catch
#             strata_rpn = sum(catch_used) / sum(effhks_used) * unique(area_kmsq)) # raw mean RPN
# 
# # Create flag to not bootstrap if sample sizes are too small or catch is 0 
# rpn_dat <- rpn_dat %>% 
#   group_by(species, year, FMP_sub_area, RPN_strata) %>%
#   mutate(boot_tst = ifelse(length(unique(fishing_event_id)) > 1 & sd(obs_catch) > 0 & sum(obs_catch) > 0, 1, 0)) %>% 
#   ungroup()
# 
# # Convert to list so you can bootstrap across multiple groups simultaneously
# # using purrr package
# nest_rpn <- rpn_dat %>% 
#   filter(boot_tst == 1) %>% 
#   tidyr::nest(data = c(-species, -year, -FMP_sub_area))
# 
# library(data.table)
# 
# tst <- nest_rpn %>% 
#   mutate(dt = map(.x = data, ~ data.table(.x)))
# 
# lapply(nest_rpn, head) # visualize list structure
# 
# calc_rpn2 <- function(d, i) {
#   d[i, (m = sum(catch_used) / sum(effhks_used))]
#   # sum(d$catch_used[i]) / sum(d$effhks_used[i]) * unique(d$area_kmsq)
# }
# 
# tst <- tst$dt[[1]]
# 
# tst[, list(list(boot(.SD, calc_rpn2, R = 5000)), by = RPN_strata)]$V1 #, j = tst[, 3 , drop = FALSE])