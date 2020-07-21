library(readxl)
library(Hmisc)
library(plyr)
library(reshape2)
library(ggplot2)
library(tidyverse)

source("IPHC_RPN_CPUE_functions_v3.R")

YR<-2018 #last years survey
past<-2018 #fix code so that years are automated and only ahve to change these two at the beginning
ITER<-1500
sp_code<-21720
outname<-"Pacific Cod"

xlsfile<-paste(getwd(), '/IPHC Survey - Pacific Cod 2019 - prelim.xlsx', sep = "")
SET2019<-read_excel(xlsfile, "2019 Set Info")
Catch2019<-read_excel(xlsfile, "2019 Pacific Cod")

DAT2019<-merge(SET2019,Catch2019,by=c("Year","Vessel","Station","Set No."),all=T)
colnames(DAT2019)[3]<-"station"

#filter out everything that isn't Purpose code "SG", these are the standard grid stations
DAT2019<-DAT2019[DAT2019$Purpose=="SG",]

#remove station 2172 in 2019, something weird in the data
#DAT2019<-DAT2019[DAT2019$Station!=2172,]

#add in management areas, a master list of stations and areas has been created based on the majority of the 
#years that each station has been within an area.
mgmt_areas<-read.csv(paste(getwd(),"/stations_lookup.csv",sep=""),header=T, stringsAsFactors = F)

marea<-mgmt_areas[,c("station","NMFS_AREA","FMP_sub_area","NMFS_mgmt_area","FMP")]

set2<-left_join(DAT2019,marea,by="station")

#need to check that all stations are in the master, there can be new stations
st_test<-set2[is.na(set2$FMP),] #in 2018 there were 184 new stations.....ergh!
nrow(st_test)

DAT3<-set2[!is.na(set2$FMP),]#just pull out new stations and run without

#get rid of southern stations, just separate by anything with station # <3000, or any with FMP_sub_area=CAN or INSIDE
DAT4<-DAT3[DAT3$station>=3000,]
droplist<-c("CAN","INSIDE")
DAT4<-DAT4[DAT4$FMP_sub_area%nin%droplist,]

check<-DAT4[is.na(DAT4$concat_meters),]#all area 2C stations in 2018
#2C should be all EY/SE, so assign those station there

#assign all stations without FMP_sub_area to EY/SE (at least for 2018) and GOA
#DAT4[is.na(DAT4$FMP_sub_area),]$FMP_sub_area<-"EY/SE"
#DAT4[is.na(DAT4$FMP),]$FMP<-"GOA"
#DAT4[is.na(DAT4$NMFS_mgmt_area),]$NMFS_mgmt_area<-"EGOA"

#now have to assign depth bins to those with missing concats
#add in depth in meters and depth bins
DAT4$avgdep_m<-DAT4$`Avg Depth`*1.82
DAT4$dpbin_m<-ifelse(DAT4$avgdep_m<100,0,
                    ifelse(DAT4$avgdep_m>=100&DAT4$avgdep_m<200,100,
                           ifelse(DAT4$avgdep_m>=200&DAT4$avgdep_m<300,200,
                                  ifelse(DAT4$avgdep_m>=300&DAT4$avgdep_m<400,300,400))))
DAT4$concat_meters<-paste(DAT4$FMP_sub_area,DAT4$dpbin_m,sep="")

#convert concat_meters to RPN strata
areasize<-read.csv(paste(getwd(),"/areasize_2015.csv",sep=""), header=T)
DAT5<-merge(DAT4,areasize[,c("concat_meters","RPN_strata","area_kmsq")],by="concat_meters",all.x=T)

#create data for analysis
D6<-DAT5[,c("station","Set No.","RPN_strata","FMP_sub_area","NMFS_mgmt_area","FMP","Hooks Retrieved","Hooks Observed","IPHC Species#","Number Observed","area_kmsq")]
colnames(D6)<-c("station","SetNo","RPN_strata","FMP_sub_area","NMFS_mgmt_area","FMP","hksretriev","hksobs","IPHC_Species","Num_obs","area_kmsq")

#estimate ineffective hooks
ineff<-c(302,307,301,306) #need to revisit this filter, check new codes
inefhks<-ddply(D6[D6$IPHC_Species%in%ineff,],c("SetNo","station"),summarize,inefhks=sum(Num_obs,na.rm=T))
DAT6<-merge(D6,inefhks,by=c("SetNo","station"),all=T)
DAT6[is.na(DAT6$inefhks),]$inefhks<-0

#Set up dataframe so each row is a unique station
D7<-dcast(DAT6,station+SetNo+RPN_strata+FMP_sub_area+NMFS_mgmt_area+FMP+hksretriev+hksobs+area_kmsq+inefhks~IPHC_Species,value.var="Num_obs",fun.aggregate=sum)

#clean out extra columns and duplicates
DAT7<-melt(D7[,1:11],id.vars=c("station","SetNo","RPN_strata","FMP_sub_area","NMFS_mgmt_area","FMP",
                        "hksretriev","hksobs","area_kmsq","inefhks"))
colnames(DAT7)[11:12]<-c("IPHC_Species","Num_obs")
DAT7$Year<-2019
dupst<-ddply(DAT7,"station",summarize,dups=length(Year))
dupst<-dupst[dupst$dups>1,]#none, so skip next line
dupdat<-DAT7[DAT7$station%in%dupst$station,] #no way to determine if there were issues, just remove those stations
D8<-DAT7[DAT7$station%nin%dupst$station,]

D8$ex_eff<-NA
D8$ex_ineff<-NA
if(D8$hksretriev!=D8$hksobs){
  D8$ex_ineff<-(D8$inefhks/D8$hksobs)*D8$hksretriev
  D8$ex_eff<-D8$hksretriev-D8$ex_ineff
} else {
  D8$ex_ineff<-D8$inefhks
  D8$ex_eff<-D8$hksretriev-D8$ex_ineff
}
D8$SetNo<-NULL
colnames(D8)[10:11]<-c("IPHC_code","obs_catch")
DAT8<-D8

#can this be skipped?
#DAT8<-melt(D8,id.vars=c("Year","Station","RPN_strata","FMP_sub_area","NMFS_mgmt_area","FMP","area_kmsq",
                      #  "hksretriev","hksobs","inefhks","ex_eff","ex_ineff","IPHC_Species"))
#colnames(DAT8)[12:13]<-c("IPHC_code","obs_catch")
#DAT8<-DAT8[DAT8$IPHC_code==26,]

#This just gets to the species name
code_list<-read.csv("species_code_conv_table.csv")
species<-code_list[code_list$RACE_CODE%in%sp_code,c("IPHC_code")]
mdat<-DAT8[DAT8$IPHC_code %in% species,]
mdat<-merge(mdat,code_list[,c("IPHC_code","Species")],by="IPHC_code",all.x=T)
species<-unique(mdat$Species)

#list of the various area types
area_list<-c("NMFS_mgmt_area","FMP")

for (k in 1:2){
  print("Calculating number of stations and extrapolated catch/CPUE for each strata")
  loop_dat<-mdat
  colnames(loop_dat)[2]<-"concat"

  #these few steps get the total number of stations out of full data set
  n_station<-ddply(loop_dat,c(as.character(area_list[k])),summarize,n_stations=length(concat)) 
  n_station<-merge(n_station,species,all=T)
  colnames(n_station)[3]<-c("Species")
  
  #extrapolates catch
  loop_dat$obs_CPUE<-loop_dat$obs_catch/(loop_dat$hksobs-loop_dat$inefhks)#species CPUE at this station
  loop_dat$ex_catch<-loop_dat$obs_CPUE*loop_dat$ex_eff #calculates extrapolated total catch in numbers
  loop_dat$ex_catch[is.na(loop_dat$ex_catch)]<-0 #any station with no catch is put as 0
  print(nrow(loop_dat))
  #sum over area (either FMP or sub_area)
  #station_sum<-ddply(loop_dat,c(as.character(area_list[k])),summarize,ex_ineff=mean(ex_ineff,na.rm=T),ex_eff=mean(ex_eff,na.rm=T)) #have to get one value for hooks for each station, this is necessary if running code for more than one species
  st_sum2<-ddply(loop_dat,c(as.character(area_list[k])),summarize,ex_ineff=sum(ex_ineff,na.rm=T),ex_eff=sum(ex_eff,na.rm=T)) #gets one value of eff and ineff hooks for each year and area/strata
  sp_sum<-ddply(loop_dat,c(as.character(area_list[k]),"Species"),summarize,ex_catch=sum(ex_catch,na.rm=T),n_pos_station=sum(obs_catch>0))#this is actually redunant, but left it in incase we do other species in teh future
  
  #add area sizes to summed strata
  area_sum<-merge(sp_sum,n_station,by=c(as.character(area_list[k]),"Species"),all.y=T)
  area_sum<-merge(area_sum,st_sum2,by=c(as.character(area_list[k])),all.x=T)
  area_sum[is.na(area_sum)]<-0
  colnames(area_sum)[1]<-"Area"
  
  
  #calculate CPUE
  area_sum$CPUE<-area_sum$ex_catch/area_sum$ex_eff
  
  #get bootstrap CI for CPUE
  #need to figure out how to select columns
  area<-as.character(area_list[k])
  #function calculates estimates for each area and bootstrap CI
  #change "station" column heading to "concat" so that bootstrap function will run
  #add in year as well
  loop_dat$yr<-2019
  insd<-F #just so we don't have to write a separate CPUE function, no inside waters in this data
  loop_out<-boot_CPUE(DATA=loop_dat,ITER=ITER,AREA=area,INSD=insd,outname=outname,SPECIES="Cod_Pacific")
  print(nrow(loop_out))
  lo<-merge(area_sum,loop_out,by=c("Area","Species"))
  print(nrow(lo))
  lo$Strata<-area_list[k]
  #colnames(lo)[2]<-"Area"
  write.csv(lo,paste("IPHC_bootCPUECI_",outname,area_list[k],2019,".csv",sep=""),row.names=F)
}

#combine all outputs into one file
out1<-read.csv(paste("IPHC_bootCPUECI_",outname,area_list[1],2019,".csv",sep=""))
out2<-read.csv(paste("IPHC_bootCPUECI_",outname,area_list[2],2019,".csv",sep=""))
all_out<-rbind(out1,out2)

write.csv(all_out,paste("IPHC_CPUE_",outname,2019,"SUMMARY.csv",sep=""),row.names=F)

############
#NOW RPNs

#these few steps get the total number of stations out of full data set
n_station<-ddply(mdat,c("RPN_strata"),summarize,n_stations=length(station)) 
n_station<-merge(n_station,unique(areasize[,c(1,6,5)]),by=c("RPN_strata"),all.x=T)

#extrapolates catch
mdat3<-mdat
mdat3$obs_CPUE<-mdat3$obs_catch/(mdat3$hksobs-mdat3$inefhks)#species CPUE at this station
mdat3$ex_catch<-mdat3$obs_CPUE*mdat3$ex_eff #calculates extrapolated total catch in numbers
mdat3$ex_catch[is.na(mdat3$ex_catch)]<-0 #any station with no catch is put as 0

#sum over FMP_sub_area and depth bin
colnames(mdat3)[2]<-"concat"
mdat3$yr<-2019
mdat3$pos<-NA
mdat3$pos<-ifelse(mdat3$obs_catch>0,1,0)
   
#station_sum<-ddply(mdat3,c("yr","concat","FMP_sub_area","RPN_strata"),summarize,obs_ineff=mean(inefhks,na.rm=T),obs_eff=mean(hksobs,na.rm=T)-obs_ineff) #have to get one value for hooks for each station, this is necessary if running code for more than one species
st_sum2<-ddply(mdat3,c("yr","FMP_sub_area","RPN_strata"),summarize,obs_ineff=sum(inefhks,na.rm=T),obs_eff=sum(hksobs-inefhks)) #gets one value of eff and ineff hooks for each year and area/strata
sp_sum<-ddply(mdat3,c("yr","FMP_sub_area","RPN_strata","Species"),summarize,obs_catch=sum(obs_catch,na.rm=T),n_pos_station=sum(pos))

#add area sizes to summed strata
area_sum<-merge(sp_sum,n_station,by=c("FMP_sub_area","RPN_strata"),all.y=T)
area_sum<-merge(area_sum,st_sum2,by=c("yr","FMP_sub_area","RPN_strata"),all.x=T)

#calculate CPUE
area_sum$CPUE<-area_sum$obs_catch/area_sum$obs_eff

#calculate RPN
area_sum$RPN<-area_sum$CPUE*area_sum$area_kmsq

#get bootstrap CI for CPUE and RPN
boot_RPN(mdat3,ITER,AREA=areasize) #when this step is running, it prints a progress bar
boot_out<-read.csv(paste(getwd(),"/CPUEoutmat.csv",sep=""), header=T)


final_out<-merge(area_sum,boot_out,by=c("yr","FMP_sub_area","RPN_strata","Species"))

#calculate RPNs directly from CPUE CI for comparison
final_out$calc_LLRPN<-as.numeric(as.character(final_out$LLCPUE))*final_out$area_kmsq
final_out$calc_ULRPN<-as.numeric(as.character(final_out$ULCPUE))*final_out$area_kmsq

name<-paste("IPHC_RPN_",outname,2019,".csv",sep="")
write.table(final_out,name,sep=",",row.names=F) 


#############################################
#FIGURES
pcod2018<-read.csv("IPHC_RPN_Pacific Cod2018.csv",header=T)
pcod2019<-read.csv("IPHC_RPN_Pacific Cod2019.csv",header=T)
IPHC_pcod<-rbind(pcod2018,pcod2019)

pcod2<-ddply(IPHC_pcod,c("yr","FMP_sub_area","Species"),summarize,tot_RPN=sum(RPN),tot_LL=sum(LLRPN),tot_UL=sum(ULRPN))
pcod2$FMP_sub_area<-factor(pcod2$FMP_sub_area,levels=c("BS","AI","WGOA","CGOA","WY","EY/SE"))


CPUE2018<-read.csv("IPHC_CPUE_Pacific Cod2018SUMMARY.csv",header=T)
CPUE2019<-read.csv("IPHC_CPUE_Pacific Cod2019SUMMARY.csv",header=T)
#new data has columns out of order
CPUE2019<-CPUE2019[,c("yr","Area","Species","ex_catch","n_pos_station","n_stations","ex_ineff",
                      "ex_eff","CPUE","LLCPUE","ULCPUE","SDCPUE","Strata")]
CPUE2019$INSIDE<-"FALSE"

all_CPUE<-rbind(CPUE2018,CPUE2019)
keeps<-c("AI","BS","WGOA","CGOA","EGOA")
AKCPUE<-all_CPUE[all_CPUE$Area%in%keeps,]

AKCPUE$Area<-factor(AKCPUE$Area,levels=c("BS","AI","WGOA","CGOA","EGOA"))

pdf(paste("IPHC_prelim_",outname,".pdf",sep=""))
ggplot(pcod2, aes(x=yr, y=tot_RPN,color=FMP_sub_area,fill=FMP_sub_area)) + 
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=tot_LL,ymax=tot_UL))+
  scale_y_continuous(expand=c(0,0))+
  facet_grid(FMP_sub_area~.,scales="free_y")+
  labs(y="RPN",title=paste(outname," RPNs FMP_sub_area",sep=""))+
  theme_bw()+
  theme(plot.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color = 'black'),
        axis.line.y = element_line(color = 'black'),
        axis.title.y=element_text(colour='black'),
        axis.title.x=element_text(colour='black'),
        axis.text.y=element_text(size=10,colour='black'),
        axis.text.x=element_text(size=8,colour='black'),
        plot.title=element_text(size=rel(1),colour='black'),
        axis.ticks=element_line(colour='black'),
        legend.position="none",
        strip.background=element_blank(),
        strip.text=element_text(size=8,colour='black'))
ggplot(AKCPUE[AKCPUE$Strata=="NMFS_mgmt_area",], aes(x=yr, y=CPUE,color=Area,fill=Area)) + 
        geom_point()+
        geom_line()+
        geom_errorbar(aes(ymin=LLCPUE,ymax=ULCPUE))+
        scale_y_continuous(expand=c(0,0))+
        facet_grid(Area~.,scales="free_y")+
        labs(y="CPUE (#/effhks)",title=paste(outname," CPUEs NMFS_mgmt_area",sep=""))+
        theme_bw()+
        theme(plot.background=element_blank(),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.line.x = element_line(color = 'black'),
              axis.line.y = element_line(color = 'black'),
              axis.title.y=element_text(colour='black'),
              axis.title.x=element_text(colour='black'),
              axis.text.y=element_text(size=10,colour='black'),
              axis.text.x=element_text(size=8,colour='black'),
              plot.title=element_text(size=rel(1),colour='black'),
              axis.ticks=element_line(colour='black'),
              legend.position="none",
              strip.background=element_blank(),
              strip.text=element_text(size=8,colour='black'))
dev.off()
