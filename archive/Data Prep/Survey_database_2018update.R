libs<-c("tidyverse","reshape2","rgdal","Hmisc","plyr","readxl")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )])>0){install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE )])}
lapply(libs,library,character.only=T)

########################################################
###########################################################
#steps to prepare set info data
#Step 1) contact Aaron Ranta and get the IPHC_AllspeciesXX.xlsx update for the previous years survey
#Step 2) this is the inefficiency step, save each sheet in the above .xlsx as a separate .csv,
 #OR take the time to figure out the formatting to pull directly from the .xlsx (I'm sure it's simple)

YR<-2018 #current survey year data
past<-2017

#bring in this years update, code may change from year to year, depending on how IPHC changes data that they send.
#note that the .csv is a sheet out of the IPHC_AllspeciesXX.xlsx, but reading directly messes up the columns 
set<-read.csv(paste(getwd(),"/SET_INFO_",YR,".csv",sep=""),header=T,stringsAsFactors = F)

#fix set column names 
colnames(set)<-c("yr","vessel","station","setno","startlat","startlon","startdep","endlat","endlong","enddep","midlat","midlong",
                 "hauldate","IPHCstat","IPHCreg","avgdep_fm","lglhalno","subleglhal","lglhalwt","sublglhalw","skatesset","sktshaul",
                 "avghkpersk","soaktime","effskts","hksretriev","hksobs","Purpose","ineffcode","Effective") 
set<-subset(set,select=-c(soaktime))

#add in concat columns
set$concat<-paste(set$yr,set$station,sep="")
#set$concat2<-paste(set$yr,".",set$station,sep="")

#add in depth in meters and depth bins
set$avgdep_m<-set$avgdep_fm*1.82
set$dpbin_fm<-ifelse(set$avgdep_fm<100,0,ifelse(set$avgdep_fm>=200,200,100))
set$dpbin_m<-ifelse(set$avgdep_m<100,0,
                     ifelse(set$avgdep_m>=100&set$avgdep_m<200,100,
                            ifelse(set$avgdep_m>=200&set$avgdep_m<300,200,
                                   ifelse(set$avgdep_m>=300&set$avgdep_m<400,300,400))))

#rearrange columns to match all_dat
set<-set[,c("concat","yr","vessel","station","setno","startlat","startlon","startdep","endlat","endlong","enddep","midlat",
       "midlong","hauldate","IPHCstat","IPHCreg","avgdep_fm","avgdep_m","dpbin_fm","dpbin_m","lglhalno","subleglhal","lglhalwt",
       "sublglhalw","skatesset","sktshaul","avghkpersk","effskts","hksretriev","hksobs","Purpose","ineffcode","Effective" )]

#add in management areas, a master list of stations and areas has been created based on the majority of the 
#years that each station has been within an area.
mgmt_areas<-read.csv(paste(getwd(),"/stations_lookup.csv",sep=""),header=T, stringsAsFactors = F)

marea<-mgmt_areas[,c("station","NMFS_AREA","FMP_sub_area","NMFS_mgmt_area","FMP")]

set2<-left_join(set,marea,by="station")

#need to check that all stations are in the master, there can be new stations
st_test<-set2[is.na(set2$FMP),] #in 2018 there were 184 new stations.....ergh!
nrow(st_test)

###############################################
##############################################


#################################
#ONLY do this section if nrow(st_test)>0
#this section will get the new stations assigned to mgmt areas and add them to the master list
#this chunk was written by Jordan Watson
##################################
#  Read in shapefile
sp.data <- readOGR(dsn="gf95_nmfs",layer="gf95_nmfs_polygon")

plot(sp.data)
axis(1)
axis(2)

#  This step isn't necessary but I created a smaller subset that didn't include all the extra columns. Setno is included
#here becuase ineffective stations can be re-set, so will show up twice
st_test_coords <- st_test %>% 
  dplyr::select(lon=startlon,lat=startlat,station=station,setno=setno) %>% 
  data.frame

# Setting existing coordinate as lat-long system
cord.dec = SpatialPointsDataFrame(coords = st_test_coords[,c(1,2)], 
                                  data=st_test_coords,
                                  proj4string = CRS("+proj=longlat"))

# Transforming coordinate to UTM using the built in projection data from the sp.data shapefile.
# If you aren't familiar, you can open the .prj file as text to see what info this includes.
temp <- spTransform(cord.dec, CRS(proj4string(sp.data)))

points(temp[,c(1,2)],col="red") #this shows where those new stations are on the map

#  Run the over command. Could do this not as a list. I probably create an extra step.
testdat <- over(sp.data, temp,returnList=TRUE)          

#  Convert the output list to a data frame where the name of the index value that was matched is saved as a field called "index"
junk <- Reduce(rbind,lapply(names(testdat),function(x) testdat[[x]] %>% transmute(lon,lat,station,setno,index=x)))

#  Join spatial data from shapefile (which includes the nmfs area names).
#  Note that the index values start with a zero because the shapefile matching function apparently counts the first row of data as the zero'th row, I guess?
data <- sp.data@data %>% 
  rownames_to_column(var="index") %>% 
  mutate(index=(as.character(index)),
         nmfs=NMFS_AREA) %>% 
  dplyr::select(nmfs,index) %>% 
  full_join(junk)

#the above creates a lot of zero rows, so clean all that out
new_st_area<-data[!is.na(data$station),]

#what remains should only be staions that are within NMFS areas, take a look to double check
plot(sp.data)
axis(1)
axis(2)

t2<-new_st_area[,c(3:5)]
cord_newst = SpatialPointsDataFrame(coords = t2[,c(1,2)], 
                                  data=t2,
                                  proj4string = CRS("+proj=longlat"))
temp_new <- spTransform(cord_newst, CRS(proj4string(sp.data)))
points(temp_new[,c(1,2)],col="red")

#also look at data
new_st_area[new_st_area$nmfs==0,]

#station=3216 in SEAK had coord on land, but fits with station numbering for NMFS area 659
#hand change that for 2018 data, sent Aaron Ranta an email on this one, it looks like 
#it should be offshore in 650 based on the station sequence.
new_st_area[new_st_area$nmfs==0,]$nmfs<-659

#next verify that all of the station dumped in the above steps are actually in Canada
lost_st<-merge(st_test_coords,new_st_area,by=c("station","setno","lon","lat"),all=T)
lost_st<-lost_st[is.na(lost_st$nmfs),]
nrow(st_test_coords)-nrow(new_st_area)==nrow(lost_st) #should be TRUE

plot(sp.data)
axis(1)
axis(2)

lost_st = SpatialPointsDataFrame(coords = lost_st[,c(3,4)], 
                                    data=lost_st,
                                    proj4string = CRS("+proj=longlat"))
temp_lost <- spTransform(lost_st, CRS(proj4string(sp.data)))
points(temp_lost[,c(1,2)],col="red")
#if none of the above stations appear to be within NMFS areas (haven't figured out Puget Sound yet)
#then assign all of them to NMFS area = 0
lost_st$nmfs<-0

#combine nmfs area stations with CA stations
CA_st<-as.data.frame(lost_st[,c(5,6,3,4,1,2)])
CA_st<-CA_st[,c(1:6)]

st_update<-rbind(new_st_area,CA_st)

#pull out any duplicates
#stations can be re-set if there are issues, average over station info, it's such a small difference, that it doesn't matter
dups<-as.data.frame(table(st_update$station))
dups<-dups[dups$Freq>1,] 

drops<-unique(dups$Var1)

dup_rows<-st_update[st_update$station%in%drops,] #pulls out rows with dups that we need to work with

st_update_dropped<-st_update[st_update$station%nin%drops,] #removes dups from data

dup_sum<-ddply(dup_rows,c("station"),summarize,nmfs=mean(nmfs),index=mean(index),lon=mean(lon),lat=mean(lat))
dup_set<-ddply(dup_rows,c("station"),summarize,setno=max(setno))
ds<-dup_set$setno

dup_rows2<-dup_rows[dup_rows$station==dup_set[,1]&dup_rows$setno==dup_set[,2],]

st_update<-rbind(st_update_dropped,dup_rows2)


#add new stations to master list
st_update<-st_update[,c(1,5,6,4,3)]
st_test_short<-set[,c("station","setno","avgdep_fm","avgdep_m","dpbin_fm","dpbin_m")]
su<-left_join(st_update,st_test_short,by=c("station","setno"))
colnames(su)[1]<-"NMFS_AREA"

FMP<-read.csv(paste(getwd(),"/FMP_areas_lookup.csv",sep=""),header=T, stringsAsFactors = F)

su2<-merge(su,FMP,by="NMFS_AREA",all.x=T)
su2<-within(su2,rm(setno))
colnames(su2)[3:8]<-c("latavg","lonavg","fmavg","mavg","fmbin","mbin")

up_st_all<-rbind(mgmt_areas,su2)

write.csv(up_st_all,"stations_lookup.csv",row.names=F)

#repeat the first section, then skip to below
#########################################################
#########################################################


####################################################
########################################################
#Fixing duplicate stations

#combine previous years survey data with updated survey dat
all_dat<-read.csv(paste(getwd(),"/HAUL_DAT",past,"update.csv",sep=""),header=T, stringsAsFactors = F)
all_dat<-within(all_dat,rm(concat2))
all_dat2<-rbind(all_dat,set2)
head(all_dat2)

#fill in blanks in ineffcode field and get rid of ":" symobl, ARC can't handle this field otherwise
all_dat2$ineffcode<-as.factor(sub("^$","NONE",all_dat2$ineffcode))
all_dat2$ineffcode<-as.factor(sub(":","",all_dat2$ineffcode))

#make output file for records
write.csv(all_dat2,paste("HAUL_DAT",YR,".csv",sep=""),row.names=F)

#when only doing the annual update, this isn't needed, only needed when including the 1997 data
#fix concat column because the .csv puts the huge 1997 concats into scientific notation (I think it's a csv limitation 
#to 15 digits and not worth fighting to fix at this time),and GIS can't deal with it, so forces them to be zero
#all_dat2$concat<-paste(all_dat2$yr,all_dat2$station,sep="")

#check for duplicate stations within a year, note that previous years have already been taken care of
#any dups should only be from current year
dups<-as.data.frame(table(all_dat2$concat))
dups<-dups[dups$Freq>1,] 
colnames(dups)[1]<-c("concat")

#list of stations that occur more than once a year, these have been addressed and should not show up in future years
#if no dups in annual update, then skip this section and pick up at US/CAN border
#         concat freq
#433      199727021    2
#593      199732012    2
#636      199734013    2
#637      199734014    2
#695      199739014    2
#801   199754316515    2
#6179      20024264    2
#6282      20024367    2
#6283      20024368    2
#6287      20024372    2
#6698      20027024    2
#6764      20027091    3
#6767      20027094    2
#8897      20045190    2
#13777     20084331    2
#15123     20095042    2
#16834     20109508    2
#18252     20121083    2
#18989     20125105    2
#21897     20146133    2
#23704     20164057    2
#24016     20164369    2
#24385     20166133    2
#26645     20183208    2
#27431     20186151    2

dupsdat<-all_dat2[all_dat2$concat %in% dups$concat,c("concat","Effective","ineffcode")]
#take a look a the ineffective codes, make sure nothing is too weird

#if there is an obvious reason to keep one of the duplicates and not the other, do so
#e.g., station is ineffective one time, effective next time, keep the second
#if no obvious reason, remove both duplicates
#MAKE SURE TO ADD NOTES TO IPHC_survey_dat_info_lookup.xlsx file for records

dups2<-dcast(dupsdat,concat~Effective,fun.aggregate=length)

dups3<-dups2[dups2$Y!=1,] #these stations have no obvious means to determine if a station is keep-able, so delete from dataframe
drops<-dups3[,c("concat")]
all_dat3<-all_dat2[!(all_dat2$concat %in% drops),]
#test that it did ok
(nrow(dups3)*2)==(nrow(all_dat2)-nrow(all_dat3)) #this is teh desired difference in rows between all_dat2 and all_dat3 compared to the actual difference
#should be "TRUE"

drops<-dups2[dups2$Y==1,] #list of stations which can drop just the ineffective, those that were duplicated with one effective 
all_dat4<-all_dat3[!(all_dat3$concat %in% drops$concat&all_dat3$Effective=="N"),]
#test it did ok
sum(drops$N)==(nrow(all_dat3)-nrow(all_dat4)) 

#########
########
#If no duplicates:
#all_dat4<-all_dat2

#check CAN/US border stations
#Canada first
CANdat<-all_dat4[all_dat4$IPHCreg=="2B",]
unique(CANdat$NMFS_AREA) #if 0 650, then good to go
CANdat2<-CANdat[CANdat$NMFS_AREA==650,] #check that only station 2172 shows up here,
#station 2172 is always in GOA waters, it's right on the Dixon Entrance line, leave as is

#US stations in CA, 2C first
US2C<-all_dat4[all_dat4$IPHCreg=="2C",]
unique(US2C$NMFS_AREA) #some stations have NMFS_AREA=0, which is CAN
US2C2<-US2C[US2C$NMFS_AREA==0,] 
unique(US2C2$station) #five stations cross border
#14220: only in 1997, leave as is
#3005: in CA waters in most years, leave as is
#3008: in CA all years, way inside waters, leave as is
#3009: in CA all years, way inside waters, leave as is
#3032: in 2000 location is on land, so doesn't get a NMFS_AREA, assign historical areas
#3122: in 2003 gets assigned to CA waters, assign historical areas
#3204: ignore in 2018 update, new station
#3210: ignore in 2018 update, new station
#3244: ignore in 2018 update, new station

#skip these if not in question for the yearly update
st3032dat<-all_dat4[all_dat4$station==3032,] #too look at what the areas should be
all_dat4[all_dat4$station==3032,c("NMFS_AREA")]<-659
#all_dat4[all_dat4$station==3032,c("ADFG_AREA")]<-315502
all_dat4[all_dat4$station==3032,c("FMP_sub_area")]<-"INSIDE"
all_dat4[all_dat4$station==3032,c("NMFS_mgmt_area")]<-"INSIDE"
all_dat4[all_dat4$station==3032,c("FMP")]<-"GOA"

st3122dat<-all_dat4[all_dat4$station==3122,] #too look at what the areas should be
all_dat4[all_dat4$station==3122,c("NMFS_AREA")]<-659
#all_dat4[all_dat4$station==3122,c("ADFG_AREA")]<-345803
all_dat4[all_dat4$station==3122,c("FMP_sub_area")]<-"INSIDE"
all_dat4[all_dat4$station==3122,c("NMFS_mgmt_area")]<-"INSIDE"
all_dat4[all_dat4$station==3122,c("FMP")]<-"GOA"


#US stations in CA, 2A (WA/CA border)
US2A<-all_dat4[all_dat4$IPHCreg=="2A",]
unique(US2A$NMFS_AREA) #some stations have NMFS_AREA=0, which is CAN
US2A2<-US2A[US2A$NMFS_AREA==0,] 
unique(US2A2$station) #17 stations cross border
#1084: waffles on either side, leave as is
#1116-1128: Puget Sound stations, so doesn't get a NMFS_AREA, need to manually fix this, like above 
#1621, 1623 and 1624: Puget Sound stations, so doesn't get a NMFS_AREA, need to manually fix this, like above
#1084: waffles on either side, leave as is
fixlist<-c(1116:1128,1621,1623,1624)
all_dat4[all_dat4$station %in% fixlist,c("NMFS_AREA")]<-679
#all_dat4[all_dat4$station %in% fixlist,c("ADFG_AREA")]<-0
all_dat4[all_dat4$station %in% fixlist,c("FMP_sub_area")]<-"WA"
all_dat4[all_dat4$station %in% fixlist,c("NMFS_mgmt_area")]<-"WA"
all_dat4[all_dat4$station %in% fixlist,c("FMP")]<-"WA"

#double check that NMFS_AREA==0 is only CAN
CANdat3<-all_dat4[all_dat4$NMFS_AREA==0,]
unique(CANdat3$FMP) #should only return "CAN"

############
#################
all_dat_update<-all_dat4[all_dat4$yr==YR,]
write.table(all_dat_update,paste("IPHC_setinfo_",YR,".csv",sep=""),sep=",",row.names=F)
write.table(all_dat4,"IPHC_setinfo_allyrs.csv",row.names=F)
#done with haul data
################################################
#################################################



###############################################################
#############################################################
#steps for preparing bycatch data
bycatch<-read_excel(paste(getwd(),"/IPHCSurvey_AllSpecies",YR,".xlsx",sep=""),paste(YR," Bycatch",sep=""), skip=0) #note that this messes up the ineffectivecode column
#bycatch<-read.csv(paste(getwd(),"/Bycatch_info_2015.csv",sep=""),header=T,stringsAsFactors = F)
colnames(bycatch)
#delete unwanted "Region" column
bycatch<-subset(bycatch,select=-c(Region))
#add in concat, only need the one----maybe remove it totally??
bycatch$concat<-paste(bycatch$Year,bycatch$Station,sep="")
#fix column order
bycatch<-bycatch[,c("concat","Year","Vessel","Station","Set No.","IPHC Species#","Common Name","Species","Number Observed","Sub Sample")]
#fix column names
colnames(bycatch)<-c("concat","Year","Vessel","Station","SetNo","IPHC_species","Comm_Name","Species","Num_Obs","Sub_Samp")

#look for NULL values, particularly in species code
unique(bycatch$IPHC_species) #none in 2016 update, so move on, fix in the future as needed

#remove those duplicated stations that were totally removed from the hauls
#dups<-count(all_dat2,vars=c("concat"))
#dups<-dups[dups$freq>1,] #none in 2018, so move on, fix in the future as needed
#those stations removed for being ineffective will have the bycatch data filtered out later


write.table(as.data.frame(bycatch),paste("IPHC_bycatchinfo_",YR,".csv",sep=""),sep=",",row.names=F)



####################################################
##################################################
###################################################
#DATABASE steps, formerly in ACESS, converted here
sp_codes<-read.csv("species_code_conv_table.csv")

all_bycatch<-read.csv("IPHC_bycatchinfo_all.csv")
colnames(all_bycatch)[6]<-c("IPHC_code")

colnames(bycatch)[6]<-c("IPHC_code")

bc<-rbind(all_bycatch,bycatch)
bc<-bc[bc$IPHC_code!=0,]
write.csv(bc,"IPHC_catch_allyrs.csv",row.names=F)

################################################
#Double check if there are any new species codes in updated survey data
length(unique(bc$IPHC_code))==length(sp_codes$IPHC_code) #If false, need to look at new species codes
td<-unique(bc[,c("IPHC_code","Comm_Name")])
td2<-sp_codes[,c("IPHC_code","Species")]
test<-left_join(td,td2,by="IPHC_code")
t2<-test[rowSums(is.na(test))>0,] #identifies new species codes
#these will need to be fixed by hand because need to add in RACE and OBS codes

#once species code list is fixed, then run the below
sp_codes<-read.csv("species_code_conv_table.csv")

catch<-left_join(bc,sp_codes[,c("IPHC_code","Species")],by="IPHC_code")
catch[,c("Comm_Name","Species.x")]<-list(NULL)
colnames(catch)[9]<-"Species"
colnames(catch)[1:5]<-c("concat","yr","vessel","station","setno")

#get haul data 
#this merges catch and haul AND gets rid of stations dropped due to duplicates
hc<-right_join(catch,all_dat4,by=c("yr","station","setno","concat","vessel")) #hc stands for haul and catch
#the above adds in 36 stations over the survey time series in which we have haul, but not catch data, 
#it's a small enough amount that not worth tracking down, just delete these 
hc<-hc[!(is.na(hc$IPHC_code)),]


#get rid of remainig ineffective stations
hc2<-hc[hc$Effective=="Y",]

#clean out columns
hc3<-hc2[,c("concat","yr","station","startlat","startlon","avgdep_m","dpbin_m",'lglhalno','subleglhal',
       'lglhalwt','sublglhalw','skatesset','sktshaul','avghkpersk','effskts','hksretriev','hksobs',
       'Purpose','ineffcode','Effective','NMFS_AREA','FMP_sub_area','NMFS_mgmt_area',
       'FMP','IPHC_code','Species','Num_Obs','Sub_Samp')]

#add concat FMP sub areas areas and meters
hc3$concat_meters<-paste(hc2$FMP_sub_area,hc2$dpbin_m,sep="")

#export for RPN and CPUE work
write.csv(hc3,paste(YR,"All_species_full_survey.csv",sep=""),row.names=F)




