# IPHC started hosting their survey data on the web. There are some differences
# in these data and the data previously obtained from IPHC in spreadsheets.
# Contact Jane.sullivan@noaa.gov
# Original code Nov 2021, updated Feb 2022

# In iphc-rpn/output:
# 2021/ = 1998-2021 new data from web
# 2021_old/ = 1998-2019 old data from spreadsheets, 2020-2021 new data from web
# 2018/ and 2019/ = these results were derived from old spreadsheet data

libs <- c("tidyverse", "fs", "readxl", "purrr", "RODBC") #, "zoo"
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

s18 <- read_csv(paste0("data/iphc_clean/clean_iphc_survey_1997_2018.csv"),
                       guess_max = 1e5) %>% 
  filter(between(year, 1998,2018)) %>% 
  filter(!iphcreg %in% c('2A', '2B'))

s19 <- read_csv(paste0("data/iphc_clean/clean_iphc_survey_1997_2019.csv"),
                       guess_max = 1e5) %>% 
  filter(between(year, 1998,2018)) %>% 
  filter(!iphcreg %in% c('2A', '2B'))

s20 <- read_csv(paste0("data/iphc_clean/clean_iphc_survey_1998_2020.csv"),
                       guess_max = 1e5) %>% 
  filter(between(year, 1998,2018)) %>% 
  filter(!iphcreg %in% c('2A', '2B'))

s21 <- read_csv(paste0("data/iphc_clean/clean_iphc_survey_1998_2021.csv"),
                       guess_max = 1e5) %>% 
  filter(between(year, 1998,2018)) %>% 
  filter(!iphcreg %in% c('2A', '2B'))


s18 %>% filter(effective == 'Y' & subsample == 0) %>%  filter(hksobs != hksretriev) %>% count(iphcreg, purpose)
s19 %>% filter(effective == 'Y' & subsample == 0) %>%  filter(hksobs != hksretriev) %>% count(iphcreg, purpose)
s20 %>% filter(effective == 'Y' & subsample == 0) %>%  filter(hksobs != hksretriev) %>% count(iphcreg, purpose)
s21 %>% filter(effective == 'Y' & subsample == 0) %>%  filter(hksobs != hksretriev) %>% count(iphcreg, purpose)

# note that purpose code definitions changed between 2020 and 2021... ??

#
fid21 <- s21 %>% 
  filter(effective == 'Y' & subsample == 0) %>% filter(hksobs != hksretriev) %>% 
  distinct(fishing_event_id, hksobs, hksretriev) %>% 
  rename(hksobs21 = hksobs, hksretriev21 = hksretriev)

fid21 %>% 
  left_join(s19, by = 'fishing_event_id') %>% 
  mutate(hksobs_equal = ifelse(hksobs == hksobs21, 1, 0),
         hksretriev_equal = ifelse(hksretriev == hksretriev21, 1, 0)) %>%# nrow()
  # filter(hksretriev_equal == 1) # NONE
  filter(hksobs_equal == 1) %>% 
  select(fishing_event_id, hksobs21, hksretriev21, hksobs, hksretriev) # all except one entry that has an ineffective code of 'ST', fishing_event_id == '2018_3208'
  View()

s21 %>% filter(between(year, 1998,2018)) %>% group_by(year) %>% summarize(n21=n()) %>% 
  left_join(s20%>% filter(between(year, 1998,2018)) %>% group_by(year) %>% summarize(n20=n())) %>% 
  left_join(s19%>% filter(between(year, 1998,2018)) %>% group_by(year) %>% summarize(n19=n())) %>% 
  left_join(s18%>% filter(between(year, 1998,2018)) %>% group_by(year) %>% summarize(n18=n())) %>% 
  View()


# compare outputs 
compare_rpns <- function(COMMON_NAME = 'Spiny dogfish',
                         AREAS = c('CGOA', 'WY', 'EY/SE'),
                         OUTPATH = 'output/compare_web_data',
                         TOLERANCE = 5) {# percent change difference to detect, output
  
  old <- read_csv(paste0('output/2021_old/', COMMON_NAME, "/RPN_", COMMON_NAME, '_2021_EXTRAP.csv')) %>% 
    mutate(version = 'old_spreadsheet_data') %>% 
    filter(year <= 2019)
  
  new <- read_csv(paste0('output/2021/', COMMON_NAME, "/RPN_", COMMON_NAME, '_2021_EXTRAP.csv')) %>% 
    mutate(version = 'new_web_data') 
  
  # bind_rows(old, new) %>% 
  #   filter(FMP_sub_area %in% AREAS) %>% 
  #   ggplot(aes(x = year)) +
  #   geom_point(aes(y = fmp_rpn, col = version, shape = version)) +
  #   geom_line(aes(y = fmp_rpn, col = version, lty = version, group = version)) +
  #   geom_errorbar(aes(ymin = fmp_lci, ymax = fmp_uci, col = version, lty = version)) +
  #   facet_wrap(~FMP_sub_area, ncol = 1) +
  #   scale_y_continuous(labels = scales::comma) +
  #   labs(x = 'Year', y = 'RPN', title = paste0(COMMON_NAME, ' RPNs'))
  
  bind_rows(old, new) %>% 
    filter(FMP_sub_area %in% AREAS) %>% 
    ggplot(aes(x = year)) +
    geom_ribbon(aes(ymin = fmp_lci, ymax = fmp_uci, 
                    fill = version, #col = version, 
                    lty = version),
                col = NA,
                alpha = 0.25) +
    # geom_errorbar(aes(ymin = fmp_lci, ymax = fmp_uci, col = version)) +
    geom_point(aes(y = fmp_rpn, col = version, shape = version), size = 2) +
    geom_line(aes(y = fmp_rpn, col = version, lty = version, group = version), size = 1) +
    facet_wrap(~FMP_sub_area, ncol = 1, scales = 'free_y') +
    scale_y_continuous(labels = scales::comma) +
    labs(x = NULL, y = NULL, title = paste0(COMMON_NAME, ' RPNs from the IPHC survey')) +
    scale_fill_manual(values = c("#30D2EB", "#FFBB21")) +
    scale_colour_manual(values = c("#30D2EB", "#FFBB21")) +
    theme_minimal(base_size = 14)
  
  ggsave(paste0(OUTPATH, '/', COMMON_NAME, '.png'),
         dpi = 300, units = 'in', height = 10, width = 9)
  
  bind_rows(old, new) %>%
    filter(FMP_sub_area %in% AREAS) %>% 
    filter(year <= 2019) %>% 
    dplyr::distinct(version, species, year, FMP_sub_area, fmp_rpn) %>% 
    pivot_wider(id_cols = c(species, year, FMP_sub_area),
                names_from = version, values_from = fmp_rpn) %>% 
    mutate(percent_change = 100 * ((new_web_data - old_spreadsheet_data) / old_spreadsheet_data)) %>% 
    filter(abs(percent_change) >= TOLERANCE) %>% 
    write_csv(paste0(OUTPATH, '/different ', COMMON_NAME, '.csv'))
}

# Sablefish ----

compare_rpns(COMMON_NAME = 'Sablefish', AREAS = c('INSIDE','WGOA', 'CGOA', 'WY', 'EY/SE', 'BS', 'AI'))

compare_rpns(COMMON_NAME = 'Pacific cod', AREAS = c('INSIDE','WGOA','CGOA', 'WY', 'EY/SE', 'BS', 'AI'))

compare_rpns(COMMON_NAME = 'Common sharks', AREAS = c('WGOA','CGOA', 'WY', 'EY/SE', 'BS', 'AI'))

compare_rpns(COMMON_NAME = 'Spiny dogfish', AREAS = c('INSIDE','WGOA','CGOA', 'WY', 'EY/SE'))

compare_rpns(COMMON_NAME = 'Shortraker rockfish', AREAS = c('INSIDE','WGOA', 'CGOA', 'WY', 'EY/SE', 'BS', 'AI'))

compare_rpns(COMMON_NAME = 'Shortspine thornyhead', AREAS = c('INSIDE','WGOA', 'CGOA', 'WY', 'EY/SE', 'BS', 'AI'))

compare_rpns(COMMON_NAME = 'Sleeper shark', AREAS = c('INSIDE','WGOA', 'CGOA', 'WY', 'EY/SE', 'BS', 'AI'))

compare_rpns(COMMON_NAME = 'Lingcod', AREAS = c('INSIDE','WGOA', 'CGOA', 'WY', 'EY/SE'))

compare_rpns(COMMON_NAME = 'Yelloweye rockfish', AREAS = c('INSIDE','WGOA', 'CGOA', 'WY', 'EY/SE'))

compare_rpns(COMMON_NAME = 'Rougheye Blackspotted', AREAS = c('INSIDE','WGOA', 'CGOA', 'WY', 'EY/SE', 'BS', 'AI'))

compare_rpns(COMMON_NAME = 'Arrowtooth flounder', AREAS = c('INSIDE','WGOA', 'CGOA', 'WY', 'EY/SE', 'BS', 'AI'))

compare_rpns(COMMON_NAME = 'Greenland turbot', AREAS = c('BS', 'AI'))

compare_rpns(COMMON_NAME = 'Redbanded rockfish', AREAS = c('INSIDE', 'CGOA', 'WY', 'EY/SE', 'BS', 'AI'))

# read excel
rpns <- dir_ls(path = "output/compare_web_data", regexp = "*.csv") %>% 
  map(read_csv) %>% 
  bind_rows() 
  
rpns %>% 
  group_by(species) %>% 
  dplyr::summarise(n = n()) %>% 
  arrange(-n) %>% 
  print(n=Inf)
  
rpns %>% 
  group_by(year) %>% 
  dplyr::summarise(n = n())%>% 
  arrange(-n)%>% 
  print(n=Inf)
  
rpns %>% 
  group_by(FMP_sub_area) %>% 
  dplyr::summarise(n = n())%>% 
  arrange(-n)%>% 
  print(n=Inf)
  

