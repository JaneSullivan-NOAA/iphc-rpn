*****UPDATED July 13, 2020********************



#########################################################
Readme for getting IPHC survey data from IPHC
and prepared for R, then to calculate RPNs
#########################################################

GETTING DATA:
1) Find last years .xlsx file sent by Aaron Ranta
	"IPHCSurvey_AllSpeciesYYYY.xlsx" with YYYY being the year of the survey update, usually 2 years prior to current year
2) Email Aaron Ranta at IPHC, after January IPHC meeting is over
	request updated data from previous years survey
3) Double check when data comes in to ensure no column heading or format changes. 
	It's usually pretty consistent and Aaron is fast, but good to double check everything

PREPARING DATA: These are the onerous steps, but super critical to diligently double check the data files
1) Find last years survey update code
	"Survey_database_YYYYupdate.R" with YYYY being the year of the survey update, usually 2 years prior to current year
	"stations_lookup.csv" - this is a master list, it may have to be updated each year, depending on if new stations are added.
	
2) Set up a logical working directory flow for yourself and put all of this years data/code in your workflow
3) Run database code. This is not a true db anymore, the name is a carry over form when it was all in Access.
	Most of this code should be consistent from year to year
	However, it needs to be run through carefully to ensure no errors in data oddities
	Everything should be noted in the code, but it will kick out things like duplicate stations
	or odd codes that haven't been used in previous years
4) Copy over output file "YYYYAll_species_full_survey.csv" to RPN work flow, as needed.

CREATING RPNS:
1) Create workflow and add data files:
	"YYYYAll_species_full_survey.csv"
	"areasize_2015.csv" - this does NOT need to be updated each year
	"species_code_conv_table.csv"
2) Find R codes:
	"IPHC_RPN_CPUE_functions_v3.R"
	"IPHC_RPN_CPUE.R"
3) Run IPHC_RPN_CPUE.R as needed for species. It kicks out lots of tables of results and some graphics.


####################################################################
Special stuff for Pcod for current years assessments

IPHC will provide a preliminary dataset for JUST Pcod 
to make current year RPNs
####################################################################

Contact Aaron Ranta about mid-Sept to find out when to get Pcod data. Send him last years as an example.
"IPHC Survey - Pacific Cod YYYY - prelim.xlsx" YYYY is the current survey year

1) Run through "PCOD_currentyr_prelim.R", this is probably glitchy, but worked ok last year.


###################################################################
This whole project needs to be cleaned up and made more efficient,
then written up in a tech memo. As part of that, we need to investigate
hook competition and any critical changes to the survey structure that
may influence results. IPHC uses a hook competition correction,
but it may or may not matter for our species. Same with bait changes,
expansion of the standard survey grid, etc. The last thing on the
RPN list is to discuss with Aaron data prior to 1998 and if we can 
figure out how to standardize it. If not, can we still use it,
but as different time series?














NEW VERSION:
1) Copy "Survey_database_update.R" and "EDIT_locations_YYYYupdate.csv" to current years folder, from previous year folder
2) Copy over GIS coverages from previous year folder
2) Open "IPHC Survey - AllSpeciesYYYY.xlsx" and save the "Set Info" tab as a separate "SET_INFO_YYYY.csv
	hope to fix this in the future, but ineffcode format screws up import if importing from .xlsx
2) Open and change the files to be read in to the correct "all_dat" and current year data
3) Work through the R script





#DO THESE STEPS AFTER R STEPS
In Access
1) Open IPHC database.......SAVE A COPY WITH UPDATED YEARS BEFORE MANIPULATING OLD DB
2) Make sure headings on "DAT_Haul Info" match exactly those on the update sheet, check order of columns too (this table was rebuilt for the 2013 update)
3) Right click on DAT_Haul Info-raw>import>text file
4) Search for file (for example: \\nmfs.local\AKC-ABL\Users\cindy.tribuzio\My Documents\IPHC\Survey\Data from IPHC\2015 Update\IPHC_setinfo_2015.csv)
5) Select "Append a copy of the records to table: DAT_Haul Info_raw">OK
	-comma separated
	-click that the first row contains field names
	-scan through columns to make sure they are OK

7) Check to make sure it came in, review and correct any errors (some errors may pop up if a cell doesn't have a value, e.g., no end lat)


###############################################################
Readme for preparing the survey catch data to enter into database
###############################################################

################NEW VERSION
1) continue through "bycatch" section in R (maybe should make this an Rmd doc???)

In access
7) Right click on DAT_Survey Catch Data-raw>import>text
8) Search for file (for example: \\nmfs.local\AKC-ABL\Users\cindy.tribuzio\My Documents\IPHC\Survey\Data from IPHC\2015 Update\IPHC_bycatchinfo_2015.csv)
9) Select "Append a copy of the records to table: DAT_Survey Catch Data-raw">OK
	-comma separated
	-click that the first row contains field names
	-scan through columns to make sure they are OK
10) Check to make sure it came in







####################################################################
README How to get survey data out of access in the correct format
####################################################################
1) Run Step 1, this creates a cross tab table, which won't automatically save as a table in Access, rearranges species catch by columns
2) Run Step 2, this creates the table in the proper format so each row is a station and all species are along column headings
3) Run Step 3, this creates the final table with new columns for commercial depth and area groupings (non-confidential groups)
	NOTE: commercial groupings are year specific and will need to be ammended by hand with each new year of commercial data, 
		it's important to go back and check previous years to make sure things haven't changed)
4) Run Step 4, this removes duplicates, for example in 2010 Yelloweye Rockfish was reported as both a subsample and total count
4) Export Step 4 table (NOTE: this table has an added column called "prop" filled with "blank".  Sarah G. is currently providing the "prop" values
	and they will have to be added separately)


#####################################################################
README How to get survey data ready for R
#####################################################################
1) Open exported Step 4 table
2) Attach Metadata sheets from previous years data files, if desired.  This is nice to keep track of things like duplicates that are removed
3) Fill in "prop" column.  Currently, "prop" is supplied by Sarah G.  Do a vlookup with the file she gives to match up the concats.
4) Check that column headings match with previous years data files (for example "IPHCsurveyDATA.csv" from most recent run folder")
	NOTE: Total number of columns may change if a new species was added to the survey.
5) If desired, remove species columns that don't have any catch at all, not a necessary step though.  Code will still run finen and you can sort
	out the unwanted species later.
6) Save sheet "DATA" as "IPHCsurveyDATA.csv" in the working directory you are working in.
	

Potential unwanted species: 
Barnacles
Bird_Gull_Unident
Bird_Shearwater_Sooty
Bird_Unident
Bryozoa
Coral_Alcyonacea
Coral_Cyclohelia_lancellata
Coral_Distichopora_borealis
Coral_Errinopora
Coral_Gorgonian
Coral_Lilipathes_sp_
Coral_Octocoral
Coral_Stylaster cancellatus
Coral_Stylaster_moseleyanus
Crab_Box
Crab_Dungeness
Crab_Golden_King
Crab_Kelp
Crab_Oregon_Rock
Crab_Pacific_Lyre
Crab_Unident
Dab_Longhead
Dab_Sand
Eelpout
Empty
Fish_Unident
Flatfish_Unident
Flounder_Bering
Fulmar_Northern
Hagfish_Pacific
Hagfish_Unident
Hake_Pacific
Hydroid
Inanimate_Object
Lord_Brown_Irish
Organic_Matter_Unident
Prickleback
Prowfish
Rockfish_Blackgill
Rockfish_Bocaccio
Rockfish_Copper
Rockfish_Darkblotched
Rockfish_Greenspotted
Rockfish_POP
Rockfish_Redstripe
Rockfish_Rosethorn
Rockfish_Sharpchin
Rockfish_Splitnose
Rockfish_Unident
Rockfish_Vermillion
Rockfish_Yellowmouth
Roundfish_Unident
Salmon_Chum
Salmon_King
Salmon_Pink
Salmon_Sockeye
Salmon_Unident
Sand_Dollar
Sandfish_Pacific
Sculpin_Pacific_Staghorn
Sea_Lion_California
Sea_Lion_Stellar
Sea_Mouse
Sea_Onion
Seal_Harbour
Shark_Blue
Shark_Misc
Shark_Sixgill
Shark_Soupfin
Shells
Skate_Bering/Alaska
Skate_Bering/Aleutian
Skate_Black
Skate_Flathead
Skate_Golden
Skate_Okhotsk
Skate_Roughshoulder
Skate_Sandpaper
Skate_Starry
Skate_Whitebrow
Snail_eggs
Snailfish
Sole_Curlfin
Sole_English
Sole_Petrale
Sole_Rex
Sole_Sand
Sole_Slender
Sponge_Glass
Starfish_Blackspined
Thornyhead_Longspine
Unknown_Unspecified
Wolffish_Bering






