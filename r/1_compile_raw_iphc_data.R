# Compile raw IPHC survey data
# Contacts: jane.sullivan@noaa.gov or cindy.tribuzio@noaa.gov
# Last update: July 2020

# Step one in creating a fully reproducible abundance index using the IPHC
# setline survey data is to compile raw survey data into a usable csv format.
# Here we read raw set information and bycatch info from Excel spreadsheets,
# make sure data types and column names are consistent, join the data, and save
# it as a single csv. The original xlsx and xls files were received from C.
# Tribuzio via Google Drive on Jul 16, 2020.

# Set up ----

libs <- c("tidyverse", "fs", "readxl")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

# list all xls/xlxs files in folder
fs::dir_ls(path = "data/iphc_raw")

# Set Info ----

set_old <- dir_ls(path = "data/iphc_raw", regexp = "97-06|07-08|09|2010|2011|2012|2013") %>% 
  map(read_excel, na = c("NULL", ""),
      # Need to define column types, otherwise can't read in ineffective codes
      # properly for some reason
      col_types = c("numeric", "text",
                    "numeric", "numeric", "numeric", "numeric", "numeric",
                    "numeric", "numeric", "numeric", "numeric", "numeric",
                    "date", "numeric", "text", "numeric",
                    "numeric", "numeric", "numeric", "numeric", "numeric",
                    "numeric", "numeric", "numeric", "numeric", "numeric",
                    "text", "text", "text")) %>% 
  # Rename columns when appropriate
  map_if(~ "vslcde" %in% names(.x), ~.x %>% dplyr::rename("Vessel" = "vslcde"), .depth = 2) %>% 
  map_if(~ "LegalHalNo" %in% names(.x), ~.x %>% dplyr::rename("O32 HalNo" = "LegalHalNo"), .depth = 2) %>% 
  map_if(~ "SublegalHalNo" %in% names(.x), ~.x %>% dplyr::rename("U32 HalNo" = "SublegalHalNo"), .depth = 2) %>% 
  map_if(~ "LegalHalWeight" %in% names(.x), ~.x %>% dplyr::rename("O32 Hal Weight" = "LegalHalWeight"), .depth = 2) %>% 
  map_if(~ "SublegalHalWeight" %in% names(.x), ~.x %>% dplyr::rename("U32 Hal Weight" = "SublegalHalWeight"), .depth = 2) %>% 
  # collapse into dataframe
  bind_rows()

set <- dir_ls(path = "data/iphc_raw", regexp = "97-06|07-08|09|2010|2011|2012|2013", invert = TRUE) %>% 
  map(read_excel, na = c("NULL", "")) %>% 
  # Get rid of soak time variable
  purrr::map_if(~ "Soak Time" %in% names(.x), ~.x %>% dplyr::select(-`Soak Time`), .depth = 2) %>% 
  # Rename columns when appropriate
  map_if(~ "IPHC Reg Area" %in% names(.x), ~.x %>% dplyr::rename(`Reg Area` = `IPHC Reg Area`), .depth = 2) %>% 
  # collapse into dataframe
  bind_rows()

set <- bind_rows(set_old, set) 

names(set) <- c("year", "vessel", "station", "setno", "startlat", "startlon", "startdep", "endlat", "endlon", "enddep",
                "midlat", "midlon", "hauldate", "iphcstat", "iphcreg", "avgdep", "lglhalno", "sublglhalno", "lglhalwt", "sublglhalwt", 
                "sktsset", "sktshaul", "avghkperskt", "effskts", "hksretriev","hksobs","purpose","effective", "ineffcode") 

# Create new id: unique set and year combinations
set <- set %>% dplyr::mutate(fishing_event_id = paste(year, station, sep = "_"))

# Bycatch ----

# Function to read in multiple sheets from multiple excel files
read_multiple_excel <- function(path, string) {
  path %>%
    excel_sheets() %>% 
    set_names() %>% 
    # "string" is a specific pattern of sheets you want to read in
    .[grep(x = ., pattern = string)] %>% 
    map_df(read_excel, path = path, na = c("NULL", ""))
}

# Read all the bycatch sheets 
bycatch <- dir_ls(path = "data/iphc_raw") %>% 
  map(read_multiple_excel, string = "Bycatch") %>%  
  # Get rid of columns that only show up some times
  map_if(~ "concat" %in% names(.x), ~.x %>% select(-concat), .depth = 2) %>% 
  map_if(~ "Region" %in% names(.x), ~.x %>% select(-Region), .depth = 2) 

# Make sure column names are in the right order (it's ok that vslcde and Vessel
# are names differently because they're in the right position)
bycatch %>% map(names)

# Write new names, bind rows/collapse list, create new id: unique set and year
# combinations
bycatch <- bycatch %>% 
  map_df(setNames, c("year", "vessel", "station", "setno", "spp_iphc", "spp_common", "spp_sci", "nobs", "subsample")) %>% 
  bind_rows() %>% 
  mutate(fishing_event_id = paste(year, station, sep = "_"))

# Combine and save ----

# Combine data set info and bycatch data
set %>% 
  # get rid of cols that won't get used in rpn analysis
  select(-c(startdep, endlat, endlon, enddep, midlat, midlon)) %>% 
  left_join(bycatch, by = c("year", "vessel", "station", "setno", "fishing_event_id")) %>% 
  arrange(year, iphcreg) %>% 
  write_csv(paste0("data/iphc_clean/clean_iphc_survey_", min(set$year), "_", max(set$year), ".csv"))
