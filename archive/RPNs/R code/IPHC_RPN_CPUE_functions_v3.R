#####################################
#Functions developed to calculate RPNs
#and CPUE for just GOA/BSAI, and coastwide (CPUE only)
#remember RPNs are additive, so it is run 
#at the highest data resolution, with CPUE 
#estimates at that resolution. CPUE function
#outputs summary graphs too.
#Last updated 2018-8-16
#By: Cindy Tribuzio


libs<-c("ggplot2","reshape2","boot","plyr")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )])>0){install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE )])}
lapply(libs,library,character.only=T)

'%nin%'<-Negate('%in%')
##################################
#CPUE codes
#####################################
IPHC_CPUE<-function(sp_code,outname,YR,ITER,...){
  print("Importing and formating data")
  
  #debug
  sp_code=20510;out=T;outname="Sablefish";YR=YR;ITER=ITER
  
  areasize<-read.csv(paste(getwd(),"/areasize_2015.csv",sep=""), header=T)
  code_list<-read.csv("species_code_conv_table.csv")
  species<-code_list[code_list$RACE_CODE%in%sp_code,c("IPHC_code")]
  
  #import data and prep
  SurveyDat<-read.csv(paste(getwd(),"/",YR,"All_species_full_survey.csv",sep=""), header=T)
  SurveyDat2<-subset(SurveyDat,SurveyDat$hksobs>15) ###removes all bad stations, those with less than 15 hooks retrieved and no data
  SurveyDat2<-SurveyDat2[SurveyDat2$Effective=="Y",] #for simplicity, remove ALL ineffective stations, can revisit this later
  SurveyDat2[is.na(SurveyDat2)]<-0    ###turns all blanks into zeros, kicks out a warning about NA generated
  SurveyDat2<-SurveyDat2[SurveyDat2$yr>1997,]
  #SurveyDat2<-SurveyDat2[SurveyDat2$FMP_sub_area!="INSIDE",]#Get rid of inside waters
  #SurveyDat2<-SurveyDat2[SurveyDat2$FMP=="GOA"|SurveyDat2$FMP=="BSAI",]#Can only do RPNs for GOA/BSAI
  
  #Need to filter out duplicates, especially YE rockfish in 2010, SEAK
  #Bird and Mammal rows first because they are Sub_Sample=0, when all other fish are Sub_Sample=1
  #except in CAN where everything is Sub_Sample=1, but it's just easier to remove all of them, we don't care
  SurveyDat3<-merge(SurveyDat2,code_list[,c("IPHC_code","Ass_usage")],by=c("IPHC_code"),all.x=T)
  SurveyDat4<-SurveyDat3[SurveyDat3$Ass_usage!="Bird",]
  SurveyDat4<-SurveyDat4[SurveyDat4$Ass_usage!="Mammal",]
  #Filer out YE rockfish duplicates in SEAk in 2010
  YElist<-SurveyDat4[SurveyDat4$Sub_Samp==0&SurveyDat4$IPHC_code==89&SurveyDat4$Num_Obs>0,]
  YElist<-YElist[YElist$yr==2010|YElist$yr==2011,]
  YElist<-YElist[YElist$station>=3000&YElist$station<=4050,] #list of all stations to remove
  YElist<-as.numeric(as.character(YElist$concat))
  YElist<-c(YElist,20114090,20114106)
  SurveyDat5<-SurveyDat4[SurveyDat4$concat%nin%YElist,]
  #look for more duplicates
  #dup1<-ddply(SurveyDat5,c("concat","yr","station","Comm_Name"),summarize,dups=length(Num_Obs))
  #dup2<-dup1[dup1$dups>1,] #39 instances with some kind of duplicate recording, 22 stations
  #some stations combined the ineffective and effective station data, 
  #they were deleted by hand in All_species file, fix this next year
  #there are some duplicates that I still don't know what to do with,
  #remove for now, likely don't have a big impact, IPHC has updated data errors
  #fix next year
  rmlist<-c(19992134,19995258,20004058,20004157,20004324,20007085,20007089,20007090,20007093,
            20046144,20081061,20081070,20087022,20094031,20095019,20106070,20107083,20134347,
            20155162)
  SurveyDat6<-SurveyDat5[SurveyDat5$concat%nin%rmlist,]
  
  SurveyDat2<-SurveyDat6
  
  #make all west coast zones into one area
  levels(SurveyDat2$FMP)[levels(SurveyDat2$FMP)=="ORCA"]<-"WC" 
  levels(SurveyDat2$FMP)[levels(SurveyDat2$FMP)=="WA"]<-"WC"
  levels(SurveyDat2$FMP)[levels(SurveyDat2$FMP)=="WAOR"]<-"WC"
  
  levels(SurveyDat2$NMFS_mgmt_area)[levels(SurveyDat2$NMFS_mgmt_area)=="ORCA"]<-"WC" 
  levels(SurveyDat2$NMFS_mgmt_area)[levels(SurveyDat2$NMFS_mgmt_area)=="WA"]<-"WC"
  levels(SurveyDat2$NMFS_mgmt_area)[levels(SurveyDat2$NMFS_mgmt_area)=="WAOR"]<-"WC"
  
  print("Have patience, you must. Large this data file is.")
  #calculate effective hooks
  ineff<-c(302,307,301,306) #need to revisit this filter, check new codes
  inefhks<-ddply(SurveyDat2[SurveyDat2$IPHC_code%in%ineff,],c("concat","Sub_Samp"),summarize,inefhks=sum(Num_Obs))
  
  #create data for analysis
  SD<-dcast(SurveyDat2[,c("concat","yr","NMFS_mgmt_area","FMP","hksretriev",
                          "hksobs","Sub_Samp","Species","Num_Obs")],
            concat+yr+NMFS_mgmt_area+FMP+hksretriev+hksobs+Sub_Samp~Species,value.var="Num_Obs",
            fun.aggregate=sum) #this creates a list of unique concats
  #SD<-SD[,c("concat","yr","FMP_sub_area","concat_meters","hksretriev","hksobs","Sub_Samp",species)]
  SD2<-merge(SD,inefhks,by=c("concat","Sub_Samp"),all=T)##
  SD2[is.na(SD2)]<-0 
  #colnames(SD2)[8]<-c("obs_catch")
  SD2$ex_eff<-NA
  SD2$ex_ineff<-NA
  if(SD2$Sub_Samp==1){
    SD2$ex_ineff<-(SD2$inefhks/SD2$hksobs)*SD2$hksretriev
    SD2$ex_eff<-SD2$hksretriev-SD2$ex_ineff
  } else {
    SD2$ex_ineff<-SD2$inefhks
    SD2$ex_eff<-SD2$hksretriev-SD2$ex_ineff
  }
  #SD2$CPUE<-SD2$obs_catch/(SD2$hksobs-SD2$inefhks) #calculate species specific CPUE
  #SD2$ex_catch<-SD2$CPUE*SD2$ex_eff
  SD3<-melt(SD2,id.vars=c("concat","Sub_Samp","yr","NMFS_mgmt_area","FMP",
                          "hksretriev","hksobs","inefhks","ex_eff","ex_ineff"))
  colnames(SD3)[11:12]<-c("Species","obs_catch")
  
  #list of the various area types
  area_type<-c("NMFS_mgmt_area","FMP")
  inside<-c(T,F)
  area_list<-merge(area_type,inside)
  area_list<-area_list[-c(3),]#this is hard coded
  
  # I know this is clunky, need to fix, but it works for now
  spec_name<-code_list[code_list$IPHC_code%in%species,c("Species")]
  mdat<-SD3[SD3$Species %in% spec_name,]
  mdat<-merge(mdat,code_list[,c("IPHC_code","Species")],by="Species",all.x=T)
  species<-unique(mdat$Species)
  
  spec_CPUE<-matrix(nrow=0,ncol=14)
  for (l in 1:length(species)){
    area_CPUE<-matrix(nrow=0,ncol=14)
    spec_data<-mdat[mdat$Species==species[l],]
    for (k in 1:nrow(area_list)){
      print("Calculating number of stations and extrapolated catch/CPUE for each strata")
      if(area_list[k,2]==F){
        loop_dat<-spec_data[spec_data$NMFS_mgmt_area!="INSIDE",]
        } else {
          loop_dat<-spec_data
        }
      #these few steps get the total number of stations out of full data set
      n_station<-ddply(loop_dat,c("yr",as.character(area_list[k,1])),summarize,n_stations=length(yr)) 
      n_station<-merge(n_station,species,all=T)
      colnames(n_station)[4]<-c("Species")
      #extrapolates catch
      loop_dat$obs_CPUE<-loop_dat$obs_catch/(loop_dat$hksobs-loop_dat$inefhks)#species CPUE at this station
      loop_dat$ex_catch<-loop_dat$obs_CPUE*loop_dat$ex_eff #calculates extrapolated total catch in numbers
      loop_dat$ex_catch[is.na(loop_dat$ex_catch)]<-0 #any station with no catch is put as 0
      print(nrow(loop_dat))
      #sum over area (either FMP or sub_area)
      station_sum<-ddply(loop_dat,c("yr","concat",as.character(area_list[k,1])),summarize,
                         ex_ineff=mean(ex_ineff,na.rm=T),
                         ex_eff=mean(ex_eff,na.rm=T)) #have to get one value for hooks for each station, this is necessary if running code for more than one species
      st_sum2<-ddply(station_sum,c("yr",as.character(area_list[k,1])),summarize,
                     ex_ineff=sum(ex_ineff,na.rm=T),ex_eff=sum(ex_eff,na.rm=T)) #gets one value of eff and ineff hooks for each year and area/strata
      sp_sum<-ddply(loop_dat,c("yr",as.character(area_list[k,1]),"Species"),summarize,
                    ex_catch=sum(ex_catch,na.rm=T),n_pos_station=sum(obs_catch>0))
    
      #add area sizes to summed strata
      area_sum<-merge(sp_sum,n_station,by=c("yr",as.character(area_list[k,1]),"Species"),all.y=T)
      area_sum<-merge(area_sum,st_sum2,by=c("yr",as.character(area_list[k,1])),all.x=T)
      area_sum[is.na(area_sum)]<-0
      colnames(area_sum)[2]<-"Area"
    
      #calculate CPUE
      area_sum$CPUE<-area_sum$ex_catch/area_sum$ex_eff
    
      #get bootstrap CI for CPUE
      #need to figure out how to select columns
      area<-as.character(area_list[k,1])
      insd<-area_list[k,2]
      lspec<-species[l]
      #function calculates estimates for each area and bootstrap CI
      loop_out<-boot_CPUE(SPECIES=lspec,DATA=loop_dat,ITER=ITER,AREA=area,INSD=insd,outname=outname)
      print(nrow(loop_out))
      lo<-merge(area_sum,loop_out,by=c("yr","Area","Species"))
      print(nrow(lo))
      lo$Strata<-area_list[k,1]
      lo$INSIDE<-area_list[k,2]
      colnames(lo)[2]<-"Area"
      write.csv(lo,paste("IPHC_bootCPUECI_",lspec,area_list[k,1],area_list[k,2],YR,".csv",sep=""),row.names=F)
      area_CPUE<-rbind(area_CPUE,lo)
    }
    spec_CPUE<-rbind(spec_CPUE,area_CPUE)
  }

  write.csv(spec_CPUE,paste("IPHC_CPUE_",outname,YR,"SUMMARY.csv",sep=""),row.names=F)
  
  #output figures
  print("Making some RAD figures")
  
  ######
  #PUT these into a loop so all species/areas have a figure
  spec_CPUE$Area<-factor(spec_CPUE$Area,levels=c("BSAI","BS","AI","GOA","WGOA","CGOA","EGOA","INSIDE","CAN","WC"))
  pdf(paste("IPHC_CPUE_",outname,".pdf",sep=""))
  for (g in 1:length(species)){
    gloop_dat<-spec_CPUE[spec_CPUE$Species==species[g],]
    for (b in 1:nrow(area_list)){
      bloop_dat<-gloop_dat[gloop_dat$Strata==area_list[b,1]&gloop_dat$INSIDE==area_list[b,2],]
      print(ggplot(bloop_dat, aes(x=yr, y=CPUE,color=Area,fill=Area)) + 
              geom_point()+
              geom_line()+
              geom_errorbar(aes(ymin=LLCPUE,ymax=ULCPUE))+
              scale_y_continuous(expand=c(0,0))+
              facet_grid(Area~.,scales="free_y")+
              labs(y="CPUE (#/effhks)",title=paste(species[g]," ", area_list[b,1]," INSIDE=",area_list[b,2],sep=""))+
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
                    strip.text=element_text(size=8,colour='black')))
    }
  }
  dev.off()
}

boot_CPUE<-function(SPECIES,DATA,ITER,AREA,INSD,outname){#DATA<-loop_dat, ITER<-1500, AREA<-area, INSD<-insd, outname<-"Sablefish"
  DATA<-DATA[,c("yr",AREA,"Species","concat","ex_ineff","ex_eff","ex_catch")]
  unq_key<-unique(DATA[,c("yr",AREA,"Species")])
  stratnum<-length(unq_key[,1])
  outmat<-matrix(NA,nrow=0,ncol=6)
  colnames(outmat)<-c("Area","yr","Species","LLCPUE","ULCPUE","SDCPUE") 
  write.table(outmat,"CPUEoutmat.csv",sep=",",row.names=F) 
  for (i in 1:stratnum){
    STRATA_dat<-DATA[DATA$yr==unq_key[i,1] & 
                       DATA[,2]==unq_key[i,2]& #this is hard coded
                       DATA$Species==unq_key[i,3],]   ##subsample data by strata
    myCPUE<-function(STRATA_dat,m)sum(STRATA_dat$ex_catch[m])/(sum(STRATA_dat$ex_eff[m])-sum(STRATA_dat$ex_ineff[m]))
    #myRPN<-function(STRATA_dat,m)(sum(STRATA_dat$ex_catch[m])/(sum(STRATA_dat$ex_eff[m])-sum(STRATA_dat$ex_ineff[m])))*mean(STRATA_dat$area_kmsq[m])
    if(sum(STRATA_dat$ex_catch)>0)
      if(length(STRATA_dat$ex_catch)>1)
        if(sd(STRATA_dat$ex_catch)>0){
          myboot<-boot(STRATA_dat,myCPUE,R=ITER)
          bootconfint<-boot.ci(myboot)}
    #mybootRPN<-boot(STRATA_dat,myRPN,R=ITER)
    #bootconfintRPN<-boot.ci(mybootRPN)}
    if(sum(STRATA_dat$ex_catch)==0)
      LLCPUE<-0 else
        if(length(STRATA_dat$ex_catch)==1)
          LLCPUE<-0 else
            if(sd(STRATA_dat$ex_catch)==0)
              LLCPUE<-0 else
                LLCPUE<-bootconfint$bca[4]
    if(sum(STRATA_dat$ex_catch)==0)
      ULCPUE<-0 else
        if(length(STRATA_dat$ex_catch)==1)
          ULCPUE<-0 else
            if(sd(STRATA_dat$ex_catch)==0)
              ULCPUE<-0 else
                ULCPUE<-bootconfint$bca[5]
    if(sum(STRATA_dat$ex_catch)==0)
      SDCPUE<-0 else
        if(length(STRATA_dat$ex_catch)==1)
          SDCPUE<-0 else
            if(sd(STRATA_dat$ex_catch)==0)
              SDCPUE<-0 else
                SDCPUE<-sd(myboot$t)
    sumres<-cbind(STRATA_dat[1,1:3],LLCPUE,ULCPUE,SDCPUE)
    colnames(sumres)[2]<-"Area"
    outmat<-read.csv(paste(getwd(),"/CPUEoutmat.csv",sep=""), header=T)
    outmat<-rbind(outmat,as.matrix(sumres))
    write.table(outmat,"CPUEoutmat.csv",sep=",",row.names=F)
    rm(outmat)
    print(paste(SPECIES," ",AREA," ",INSD," STRATUM=",i,", ",round((i/stratnum)*100,0),"%",sep=""))
  }
  print(paste("Done with ",AREA," ",INSD,sep=""))
  boot_out<-read.csv("CPUEoutmat.csv", header=T) 
  boot_out
} 


###############################################
#RPN Codes
###############################################
#####################################################
#Function calculates the CPUE and RPN for each FMP subarea and depth bin
#working on adding bootstrap functionality

#sp_code is a list of RACE species codes to calculate RPNs for in the form c("XXX","XXX"), if NULL then all species will be calculated
#out=TRUE means that a .csv file will be output to the working directory
#outname is just a group name for the list of species you are selecting, for example outname=Sharks
#ITER is the number of bootstrap iterations to be run
#REBS logical if dealing with Rougheye and blackspotted

IPHC_RPN<-function(sp_code,out=TRUE,outname,YR,ITER,REBS=F){ #YR<-2017  sp_code<-c(20510,21720)
  #bring in data and format it
  print("Importing and formating data")
  
  areasize<-read.csv(paste(getwd(),"/areasize_2015.csv",sep=""), header=T)
  areasize<-areasize[complete.cases(areasize),]
  code_list<-read.csv("species_code_conv_table.csv")
  species<-code_list[code_list$RACE_CODE%in%sp_code,c("IPHC_code")]
  
  #import data and prep
  SurveyDat<-read.csv(paste(getwd(),"/",YR,"All_species_full_survey.csv",sep=""), header=T)
  SurveyDat2<-subset(SurveyDat,SurveyDat$hksobs>15) ###removes all bad stations, those with less than 15 hooks retrieved and no data
  SurveyDat2<-SurveyDat2[SurveyDat2$Effective=="Y",] #for simplicity, remove ALL ineffective stations, can revisit this later
  SurveyDat2[is.na(SurveyDat2)]<-0    ###turns all blanks into zeros
  SurveyDat2<-SurveyDat2[SurveyDat2$yr>1997,]
  SurveyDat2<-SurveyDat2[SurveyDat2$FMP_sub_area!="INSIDE",]#Get rid of inside waters
  SurveyDat2<-SurveyDat2[SurveyDat2$FMP=="GOA"|SurveyDat2$FMP=="BSAI",]#Can only do RPNs for GOA/BSAI
  
  #Need to filter out duplicates, especially YE rockfish in 2010, SEAK
  #Bird and Mammal rows first because they are Sub_Sample=0, when all other fish are Sub_Sample=1
  #except in CAN where everything is Sub_Sample=1, but it's just easier to remove all of them, we don't care
  SurveyDat3<-merge(SurveyDat2,code_list[,c("IPHC_code","Ass_usage")],by=c("IPHC_code"),all.x=T)
  SurveyDat4<-SurveyDat3[SurveyDat3$Ass_usage!="Bird",]
  SurveyDat4<-SurveyDat4[SurveyDat4$Ass_usage!="Mammal",]
  #Filer out YE rockfish duplicates in SEAk in 2010
  YElist<-SurveyDat4[SurveyDat4$Sub_Samp==0&SurveyDat4$IPHC_code==89&SurveyDat4$Num_Obs>0,]
  YElist<-YElist[YElist$yr==2010|YElist$yr==2011,]
  YElist<-YElist[YElist$station>=3000&YElist$station<=4050,] #list of all stations to remove
  YElist<-as.numeric(as.character(YElist$concat))
  YElist<-c(YElist,20114090,20114106)
  SurveyDat5<-SurveyDat4[SurveyDat4$concat%nin%YElist,]
  #look for more duplicates
  #dup1<-ddply(SurveyDat5,c("concat","yr","station","Comm_Name"),summarize,dups=length(Num_Obs))
  #dup2<-dup1[dup1$dups>1,] #39 instances with some kind of duplicate recording, 22 stations
  #some stations combined the ineffective and effective station data, 
  #they were deleted by hand in All_species file, fix this next year
  #there are some duplicates that I still don't know what to do with,
  #remove for now, likely don't have a big impact, IPHC has updated data errors
  #fix next year
  rmlist<-c(19992134,19995258,20004058,20004157,20004324,20007085,20007089,20007090,20007093,
            20046144,20081061,20081070,20087022,20094031,20095019,20106070,20107083,20134347,
            20155162)
  SurveyDat6<-SurveyDat5[SurveyDat5$concat%nin%rmlist,]
  
  #Get the correct strat
  SurveyDat7<-merge(SurveyDat6,areasize[,c("concat_meters","RPN_strata")],by=c("concat_meters"),all.x=T)
  SurveyDat2<-SurveyDat7
  
  print("Have patience, you must. Large this data file is.")
  #calculate effective hooks
  ineff<-c(302,307,301,306) #need to revisit this filter, check new codes
  inefhks<-ddply(SurveyDat2[SurveyDat2$IPHC_code%in%ineff,],c("concat","Sub_Samp"),summarize,inefhks=sum(Num_Obs))
  
  #create data for analysis
  SD<-dcast(SurveyDat2[,c("concat","yr","FMP_sub_area","RPN_strata","hksretriev",
                          "hksobs","Sub_Samp","Species","Num_Obs")],
            concat+yr+FMP_sub_area+RPN_strata+hksretriev+hksobs+Sub_Samp~Species,value.var="Num_Obs",
            fun.aggregate=sum) #this creates a list of unique concats
  #SD<-SD[,c("concat","yr","FMP_sub_area","concat_meters","hksretriev","hksobs","Sub_Samp",species)]
  SD2<-merge(SD,inefhks,by=c("concat","Sub_Samp"),all=T)
  SD2[is.na(SD2)]<-0 
  #colnames(SD2)[8]<-c("obs_catch")
  SD2$ex_eff<-NA
  SD2$ex_ineff<-NA
  if(SD2$Sub_Samp==1){
    SD2$ex_ineff<-(SD2$inefhks/SD2$hksobs)*SD2$hksretriev
  } else {
    SD2$ex_ineff<-SD2$inefhks
  }
  SD2$ex_eff<-SD2$hksretriev-SD2$ex_ineff
  #SD2$CPUE<-SD2$obs_catch/(SD2$hksobs-SD2$inefhks) #calculate species specific CPUE
  #SD2$ex_catch<-SD2$CPUE*SD2$ex_eff
  SD3<-melt(SD2,id.vars=c("concat","Sub_Samp","yr","FMP_sub_area","RPN_strata",
                          "hksretriev","hksobs","inefhks","ex_eff","ex_ineff"))
  colnames(SD3)[11:12]<-c("Species","obs_catch")
  
  
  print("Calculating number of stations, extrapolated catch/CPUE and RPN for each strata")
  #these few steps get the total number of stations out of full data set
  n_station<-ddply(SD2,c("yr","RPN_strata"),summarize,n_stations=length(yr)) 
  #n_station$RPN_strata<-paste(n_station$FMP_sub_area,n_station$dpbin_m,sep="") 
  n_station<-merge(n_station,unique(areasize[,c(1,6,5)]),by=c("RPN_strata"),all.x=T)
  #n_station<-ddply(n_station,c("yr","RPN_strata","FMP_sub_area"),summarize,n_station=sum(n_stations))

  spec_name<-code_list[code_list$IPHC_code%in%species,c("Species")]
  if(!is.null(species)){
    mdat<-SD3[SD3$Species %in% spec_name,]
  }
  if(REBS==T){ #this section pulls out data for Rougheye and Blackspotted rockfish, combines it and replaces in the dataframe
    sumdat<-mdat[mdat$RACE_CODE==30050|mdat$RACE_CODE==30052,]
    sumdat2<-cast(sumdat,concat+yr+comm_area+comm_depth+station+prop+midlat+midlong+hauldate+IPHCstat+IPHCreg+
                    dpbin_fm+dpbin_m+lglhalno+lglhalwt+lglhalwtkg+effskts+hksretriev+hksobs+ineffhks+Purpose+
                    Effective+ineffcode+NMFS_AREA+ADFG_AREA+NMFS_mgmt_area+FMP_sub_area+FMP+Sub_Samp~Species,
                  sum,value="Catch")
    sumdat2[is.na(sumdat2)]<-0
    sumdat2$Catch<-sumdat2$Rockfish_Rougheye+sumdat2$Rockfish_Blackspotted
    sumdat3<-subset(sumdat2,select=-c(Rockfish_Blackspotted,Rockfish_Rougheye))
    sumdat3$RACE_CODE<-0
    sumdat3<-cbind(Species="Rockfish_REBS",sumdat3)
    mdat<-mdat[!(mdat$RACE_CODE==30050|mdat$RACE_CODE==30052),]
    mdat<-rbind(mdat,sumdat3)
  }
  
  mdat<-merge(mdat,code_list[,c("IPHC_code","Species")],by="Species",all.x=T)
  species<-unique(mdat$Species)
  n_station<-merge(n_station,species,all=T)
  colnames(n_station)[6]<-c("Species")

  #adds in area size and gets rid of inside waters
  mdat3<-merge(unique(areasize[,c("RPN_strata","FMP_sub_area","area_kmsq")]),mdat,by=c("RPN_strata","FMP_sub_area"),all.y=T) #brings over area size for each FMP_sub_area and depth bin
  
  #extrapolates catch
  mdat3$obs_CPUE<-mdat3$obs_catch/(mdat3$hksobs-mdat3$inefhks)#species CPUE at this station
  mdat3$ex_catch<-mdat3$obs_CPUE*mdat3$ex_eff #calculates extrapolated total catch in numbers
  mdat3$ex_catch[is.na(mdat3$ex_catch)]<-0 #any station with no catch is put as 0
  
  #sum over FMP_sub_area and depth bin
  station_sum<-ddply(mdat3,c("yr","concat","FMP_sub_area","RPN_strata"),summarize,obs_ineff=mean(inefhks,na.rm=T),obs_eff=mean(hksobs,na.rm=T)-obs_ineff) #have to get one value for hooks for each station, this is necessary if running code for more than one species
  st_sum2<-ddply(station_sum,c("yr","FMP_sub_area","RPN_strata"),summarize,obs_ineff=sum(obs_ineff,na.rm=T),obs_eff=sum(obs_eff,na.rm=T)) #gets one value of eff and ineff hooks for each year and area/strata
  mdat3$pos<-ifelse(mdat3$obs_catch>0,1,0)
  sp_sum<-ddply(mdat3,c("yr","FMP_sub_area","RPN_strata","Species"),summarize,obs_catch=sum(obs_catch,na.rm=T),n_pos_station=sum(pos))
  
  #add area sizes to summed strata
  area_sum<-merge(sp_sum,n_station,by=c("FMP_sub_area","RPN_strata","yr","Species"),all.y=T)
  area_sum<-merge(area_sum,st_sum2,by=c("yr","FMP_sub_area","RPN_strata"),all.x=T)
  #area_sum<-merge(area_sum,unique(areasize[,c(6,5)]),by=c("RPN_strata"),all.x=T)
  area_sum[is.na(area_sum)]<-0
  
  #calculate CPUE
  area_sum$CPUE<-area_sum$obs_catch/area_sum$obs_eff
  
  #calculate RPN
  area_sum$RPN<-area_sum$CPUE*area_sum$area_kmsq
  
  #get bootstrap CI for CPUE and RPN
  boot_RPN(mdat3,ITER,AREA=areasize) #when this step is running, it prints a progress bar
  boot_out<-read.csv(paste(getwd(),"/CPUEoutmat.csv",sep=""), header=T)
  
  print("Finalizing estimates")
  #combine boot strap output with area summed CPUE/RPN results
  final_out<-merge(area_sum,boot_out,by=c("yr","FMP_sub_area","RPN_strata","Species"))
  
  #calculate RPNs directly from CPUE CI for comparison
  final_out$calc_LLRPN<-as.numeric(as.character(final_out$LLCPUE))*final_out$area_kmsq
  final_out$calc_ULRPN<-as.numeric(as.character(final_out$ULCPUE))*final_out$area_kmsq
  
  #final_out<-subset(final_out,select=-c(concat_meters))
  
  #output table
  if(out==T){
    ifelse(is.null(sp_code),name<-"IPHC_RPN_all_species.csv",name<-paste("IPHC_RPN_",outname,YR,".csv",sep=""))
    write.table(final_out,name,sep=",",row.names=F) 
  }
  #print("Making AWESOME summary figures")
  ######
  #PUT these into a loop so all species/areas have a figure
  final_out$FMP_sub_area<-factor(final_out$FMP_sub_area,levels=c("BS","AI","WGOA","CGOA","WY","EY/SE"))
  FMP_dat<-ddply(final_out,c("yr","Species","FMP_sub_area"),summarize,
                 FMP_RPN=sum(RPN),FMP_LL=sum(LLRPN),FMP_UL=sum(ULRPN))
  pdf(paste("IPHC_RPN_",outname,".pdf",sep=""))
  for (g in 1:length(species)){
    gloop_dat<-FMP_dat[FMP_dat$Species==species[g],]
    print(ggplot(gloop_dat, aes(x=yr, y=FMP_RPN,color=FMP_sub_area,fill=FMP_sub_area)) + 
            geom_point()+
            geom_line()+
            geom_errorbar(aes(ymin=FMP_LL,ymax=FMP_UL))+
            scale_y_continuous(expand=c(0,0))+
            facet_grid(FMP_sub_area~.,scales="free_y")+
            labs(y="RPNs",title=paste(species[g],sep=""))+
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
                  strip.text=element_text(size=8,colour='black')))
  }
  dev.off()
}


#The CPUE_boot function is called within the IPCH_RPN function, it just bootstraps the 
#stations within the strata to estimate strata level CI around the CPUE

boot_RPN<-function(DATA,ITER,AREA){ #DATA<-mdat3, ITER<-10000, AREA<-areasize
  DATA<-DATA[,c("yr","FMP_sub_area","RPN_strata","Species","concat","inefhks","hksobs","obs_catch","hksretriev","ex_eff")]
  DATA2<-merge(DATA,AREA[,c("RPN_strata","area_kmsq")],by=c("RPN_strata"))
  unq_key<-unique(DATA[,c("yr","RPN_strata","Species")])
  stratnum<-length(unq_key[,1])
  #Nam<-length(unique(DATA$Species))
  outmat<-matrix(NA,nrow=0,ncol=10)
  colnames(outmat)<-c("RPN_strata","yr","FMP_sub_area","Species","LLCPUE","ULCPUE","SDCPUE","LLRPN","ULRPN","SDRPN") 
  write.table(outmat,"CPUEoutmat.csv",sep=",",row.names=F) 
  for (i in 1:stratnum){
    STRATA_dat<-DATA2[DATA2$yr==unq_key[i,1] & 
                        DATA2$RPN_strata==unq_key[i,2]&
                        DATA2$Species==unq_key[i,3],]   ##subsample data by strata
    myCPUE<-function(STRATA_dat,m)sum(STRATA_dat$obs_catch[m])/(sum(STRATA_dat$hksobs[m])-sum(STRATA_dat$inefhks[m])) #gets obs CPUE, should match what is done above
    myRPN<-function(STRATA_dat,m)(sum(STRATA_dat$obs_catch[m])/(sum(STRATA_dat$hksobs[m])-sum(STRATA_dat$inefhks[m])) #extrapolates to total catch, then to area
                                  *mean(STRATA_dat$area_kmsq[m]))
    if(sum(STRATA_dat$obs_catch)>0)
      if(length(STRATA_dat$obs_catch)>1)
        if(sd(STRATA_dat$obs_catch)>0){
          myboot<-boot(STRATA_dat,myCPUE,R=ITER)
          bootconfint<-boot.ci(myboot)
          mybootRPN<-boot(STRATA_dat,myRPN,R=ITER)
          bootconfintRPN<-boot.ci(mybootRPN)}
    if(sum(STRATA_dat$obs_catch)==0)
      LLCPUE<-0 else
        if(length(STRATA_dat$obs_catch)==1)
          LLCPUE<-0 else
            if(sd(STRATA_dat$obs_catch)==0)
              LLCPUE<-0 else
                LLCPUE<-bootconfint$bca[4]
    if(sum(STRATA_dat$obs_catch)==0)
      ULCPUE<-0 else
        if(length(STRATA_dat$obs_catch)==1)
          ULCPUE<-0 else
            if(sd(STRATA_dat$obs_catch)==0)
              ULCPUE<-0 else
                ULCPUE<-bootconfint$bca[5]
    if(sum(STRATA_dat$obs_catch)==0)
      SDCPUE<-0 else
        if(length(STRATA_dat$obs_catch)==1)
          SDCPUE<-0 else
            if(sd(STRATA_dat$obs_catch)==0)
              SDCPUE<-0 else
                SDCPUE<-sd(myboot$t)
    if(sum(STRATA_dat$obs_catch)==0)
      LLRPN<-0 else
        if(length(STRATA_dat$obs_catch)==1)
          LLRPN<-0 else
            if(sd(STRATA_dat$obs_catch)==0)
              LLRPN<-0 else
                LLRPN<-bootconfintRPN$bca[4]
    if(sum(STRATA_dat$obs_catch)==0)
      ULRPN<-0 else
        if(length(STRATA_dat$obs_catch)==1)
          ULRPN<-0 else
            if(sd(STRATA_dat$obs_catch)==0)
              ULRPN<-0 else
                ULRPN<-bootconfintRPN$bca[5]
    if(sum(STRATA_dat$obs_catch)==0)
      SDRPN<-0 else
        if(length(STRATA_dat$obs_catch)==1)
          SDRPN<-0 else
            if(sd(STRATA_dat$obs_catch)==0)
              SDRPN<-0 else
                SDRPN<-sd(mybootRPN$t)
    if(length(STRATA_dat$obs_catch)==1){
      LLCPUE<-myCPUE(STRATA_dat)
      ULCPUE<-myCPUE(STRATA_dat)
      LLRPN<-myRPN(STRATA_dat)
      ULRPN<-myRPN(STRATA_dat)
      }           
    sumres<-cbind(STRATA_dat[1,1:4],LLCPUE,ULCPUE,SDCPUE,LLRPN,ULRPN,SDRPN)
    outmat<-read.csv(paste(getwd(),"/CPUEoutmat.csv",sep=""), header=T)
    outmat<-rbind(outmat,as.matrix(sumres))
    write.table(outmat,"CPUEoutmat.csv",sep=",",row.names=F)
    rm(outmat)
    print(paste("STRATUM=",i,", ",round((i/stratnum)*100,0),"%",sep=""))
  }
} 



###########################################
##############################################
#Quick check figure codes

IPHC_qc_figs<-function(sp_code,outname,YR){
  RPNdat<-read.csv(paste("IPHC_RPN_",outname,YR,".csv",sep=""),header=T)
  levels(RPNdat$FMP_sub_area)[levels(RPNdat$FMP_sub_area)=="WY"]<-"EGOA" 
  levels(RPNdat$FMP_sub_area)[levels(RPNdat$FMP_sub_area)=="EY/SE"]<-"EGOA"
  Rdat<-ddply(RPNdat,c("yr","FMP_sub_area","Species"),summarize,RPN_sum=sum(RPN))
  Rmm<-ddply(Rdat,c("FMP_sub_area","Species"),summarize,maxR=max(RPN_sum),minR=min(RPN_sum))
  Rdat2<-merge(Rdat,Rmm,by=c("FMP_sub_area","Species"),all.x=T)
  Rdat2$RPNnorm<-(Rdat2$RPN_sum-Rdat2$minR)/(Rdat2$maxR-Rdat2$minR)
  
  FMP_RPN<-ddply(RPNdat,c("yr","FMP_sub_area","Species"),summarize,eff_sum=sum(obs_eff),
                 catch_sum=sum(obs_catch),area_sum=sum(area_kmsq))
  FMP_RPN$CPUE<-FMP_RPN$catch_sum/FMP_RPN$eff_sum
  FMP_RPN$RPN_FMP<-FMP_RPN$CPUE*FMP_RPN$area_sum
  
  FMP_RPN2<-merge(FMP_RPN,Rdat,by=c("yr","FMP_sub_area","Species"),all.x=T)
  FMPmm<-ddply(FMP_RPN2,c("FMP_sub_area","Species"),summarize,maxC=max(RPN_FMP),minC=min(RPN_FMP))
  FMP_RPN3<-merge(FMP_RPN2,FMPmm,by=c("FMP_sub_area","Species"),all.x=T)
  FMP_RPN3$RPN_FMPnorm<-(FMP_RPN3$RPN_FMP-FMP_RPN3$minC)/(FMP_RPN3$maxC-FMP_RPN3$minC)
  
  
  CPUEdat<-read.csv(paste("IPHC_CPUE_",outname,YR,"SUMMARY.csv",sep=""),header=T)
  CPUEdat2<-CPUEdat[CPUEdat$Area!="INSIDE",]
  CPUEdat2<-CPUEdat2[CPUEdat2$Strata=="NMFS_mgmt_area",]
  Cdat<-CPUEdat2[,c("yr","Area","Species","CPUE")]
  colnames(Cdat)[2]<-"FMP_sub_area"
  Cmm<-ddply(Cdat,c("FMP_sub_area","Species"),summarize,maxC=max(CPUE),minC=min(CPUE))
  Cdat2<-merge(Cdat,Cmm,by=c("FMP_sub_area","Species"),all.x=T)
  Cdat2$CPUEnorm<-(Cdat2$CPUE-Cdat2$minC)/(Cdat2$maxC-Cdat2$minC)
  
  RCdat<-merge(Rdat2,Cdat2,by=c("yr","FMP_sub_area","Species"))
  RC2<-melt(RCdat[,c("yr","FMP_sub_area","Species","RPNnorm","CPUEnorm")],id.vars=c("yr","FMP_sub_area","Species"))
  colnames(RC2)[4:5]<-c("Metric","Amount")
  
  FMPdat<-merge(FMP_RPN3[,c("yr","FMP_sub_area","Species","RPN_FMPnorm")],
                Cdat2[,c("yr","FMP_sub_area","Species","CPUEnorm")],
                by=c("yr","FMP_sub_area","Species"))
  FMP2<-melt(FMPdat,id.vars=c("yr","FMP_sub_area","Species"))
  colnames(FMP2)[4:5]<-c("Metric","Amount")
  
  species<-unique(FMP2$Species)
  
  pdf(paste("IPHC_QC_",outname,".pdf",sep=""))
  for (g in 1:length(species)){
    gloop_dat<-FMP2[FMP2$Species==species[g],]
    print(ggplot(gloop_dat,aes(x=yr,y=Amount,color=Metric,shape=Metric,fill=Metric))+
            geom_point()+
            geom_line()+
            labs(y="Normalized RPN and CPUE",title=paste(species[g]," FMP RPN",sep=""))+
            facet_grid(FMP_sub_area~.,scales="free_y"))
    gld2<-RC2[RC2$Species==species[g],]
    print(ggplot(gld2,aes(x=yr,y=Amount,color=Metric,shape=Metric,fill=Metric))+
            geom_point()+
            geom_line()+
            labs(y="Normalized RPN and CPUE",title=paste(species[g]," summed RPN",sep=""))+
            facet_grid(FMP_sub_area~.,scales="free_y"))
  }
  dev.off()
}


###############################################
#Preliminary current year RPN code for PCOD
###############################################
#####################################################
#Function calculates the CPUE and RPN for each FMP subarea and depth bin
#working on adding bootstrap functionality



IPHC_RPN_current<-function(sp_code,out=TRUE,outname,YR,CURRENT,ITER,REBS=F){ #YR<-2017  sp_code<-c(20510,21720)
  #bring in data and format it
  print("Importing and formating data")
  
  areasize<-read.csv(paste(getwd(),"/areasize_2015.csv",sep=""), header=T)
  areasize<-areasize[complete.cases(areasize),]
  code_list<-read.csv("species_code_conv_table.csv")
  species<-code_list[code_list$RACE_CODE%in%sp_code,c("IPHC_code")]
  
  #import previous data and prep
  SurveyDat<-read.csv(paste(getwd(),"/",YR,"All_species_full_survey.csv",sep=""), header=T)
  SurveyDat2<-subset(SurveyDat,SurveyDat$hksobs>15) ###removes all bad stations, those with less than 15 hooks retrieved and no data
  SurveyDat2<-SurveyDat2[SurveyDat2$Effective=="Y",] #for simplicity, remove ALL ineffective stations, can revisit this later
  SurveyDat2[is.na(SurveyDat2)]<-0    ###turns all blanks into zeros
  SurveyDat2<-SurveyDat2[SurveyDat2$yr>1997,]
  SurveyDat2<-SurveyDat2[SurveyDat2$FMP_sub_area!="INSIDE",]#Get rid of inside waters
  SurveyDat2<-SurveyDat2[SurveyDat2$FMP=="GOA"|SurveyDat2$FMP=="BSAI",]#Can only do RPNs for GOA/BSAI
  
  #Need to filter out duplicates, especially YE rockfish in 2010, SEAK
  #Bird and Mammal rows first because they are Sub_Sample=0, when all other fish are Sub_Sample=1
  #except in CAN where everything is Sub_Sample=1, but it's just easier to remove all of them, we don't care
  SurveyDat3<-merge(SurveyDat2,code_list[,c("IPHC_code","Ass_usage")],by="IPHC_code",all.x=T)
  SurveyDat4<-SurveyDat3[SurveyDat3$Ass_usage!="Bird",]
  SurveyDat4<-SurveyDat4[SurveyDat4$Ass_usage!="Mammal",]
  #Filer out YE rockfish duplicates in SEAk in 2010
  YElist<-SurveyDat4[SurveyDat4$Sub_Samp==0&SurveyDat4$IPHC_species==89&SurveyDat4$Num_Obs>0,]
  YElist<-YElist[YElist$yr==2010|YElist$yr==2011,]
  YElist<-YElist[YElist$station>=3000&YElist$station<=4050,] #list of all stations to remove
  YElist<-as.numeric(as.character(YElist$concat))
  YElist<-c(YElist,20114090,20114106)
  SurveyDat5<-SurveyDat4[SurveyDat4$concat%nin%YElist,]
  #look for more duplicates
  #dup1<-ddply(SurveyDat5,c("concat","yr","station","Comm_Name"),summarize,dups=length(Num_Obs))
  #dup2<-dup1[dup1$dups>1,] #39 instances with some kind of duplicate recording, 22 stations
  #some stations combined the ineffective and effective station data, 
  #they were deleted by hand in All_species file, fix this next year
  #there are some duplicates that I still don't know what to do with,
  #remove for now, likely don't have a big impact, IPHC has updated data errors
  #fix next year
  rmlist<-c(19992134,19995258,20004058,20004157,20004324,20007085,20007089,20007090,20007093,
            20046144,20081061,20081070,20087022,20094031,20095019,20106070,20107083,20134347,
            20155162)
  SurveyDat6<-SurveyDat5[SurveyDat5$concat%nin%rmlist,]
  
  #Get the correct strat
  SurveyDat7<-merge(SurveyDat6,areasize[,c("concat_meters","RPN_strata")],by=c("concat_meters"),all.x=T)
  SurveyDat2<-SurveyDat7
  
  print("Have patience, you must. Large this data file is.")
  #calculate effective hooks
  ineff<-c(302,307,301,306) #need to revisit this filter, check new codes
  inefhks<-ddply(SurveyDat2[SurveyDat2$IPHC_code%in%ineff,],c("concat","Sub_Samp"),summarize,inefhks=sum(Num_Obs))
  
  #create data for analysis
  SD<-dcast(SurveyDat2[,c("concat","yr","FMP_sub_area","RPN_strata","hksretriev",
                          "hksobs","Sub_Samp","Species","Num_Obs")],
            concat+yr+FMP_sub_area+RPN_strata+hksretriev+hksobs+Sub_Samp~Species,value.var="Num_Obs",
            fun.aggregate=sum) #this creates a list of unique concats
  #SD<-SD[,c("concat","yr","FMP_sub_area","concat_meters","hksretriev","hksobs","Sub_Samp",species)]
  SD2<-merge(SD,inefhks,by=c("concat","Sub_Samp"),all=T)
  SD2[is.na(SD2)]<-0 
  #colnames(SD2)[8]<-c("obs_catch")
  SD2$ex_eff<-NA
  SD2$ex_ineff<-NA
  if(SD2$Sub_Samp==1){
    SD2$ex_ineff<-(SD2$inefhks/SD2$hksobs)*SD2$hksretriev
  } else {
    SD2$ex_ineff<-SD2$inefhks
  }
  SD2$ex_eff<-SD2$hksretriev-SD2$ex_ineff
  #SD2$CPUE<-SD2$obs_catch/(SD2$hksobs-SD2$inefhks) #calculate species specific CPUE
  #SD2$ex_catch<-SD2$CPUE*SD2$ex_eff
  SD3<-melt(SD2,id.vars=c("concat","Sub_Samp","yr","FMP_sub_area","RPN_strata",
                          "hksretriev","hksobs","inefhks","ex_eff","ex_ineff"))
  colnames(SD3)[11:12]<-c("Species","obs_catch")
  
  
  
  print("Calculating number of stations, extrapolated catch/CPUE and RPN for each strata")
  #these few steps get the total number of stations out of full data set
  n_station<-ddply(SD2,c("yr","RPN_strata"),summarize,n_stations=length(yr)) 
  #n_station$RPN_strata<-paste(n_station$FMP_sub_area,n_station$dpbin_m,sep="") 
  n_station<-merge(n_station,unique(areasize[,c(1,6,5)]),by=c("RPN_strata"),all.x=T)
  #n_station<-ddply(n_station,c("yr","RPN_strata","FMP_sub_area"),summarize,n_station=sum(n_stations))
  
  if(!is.null(species)){
    mdat<-SD3[SD3$IPHC_code %in% species,]
  }
  if(REBS==T){ #this section pulls out data for Rougheye and Blackspotted rockfish, combines it and replaces in the dataframe
    sumdat<-mdat[mdat$RACE_CODE==30050|mdat$RACE_CODE==30052,]
    sumdat2<-cast(sumdat,concat+yr+comm_area+comm_depth+station+prop+midlat+midlong+hauldate+IPHCstat+IPHCreg+
                    dpbin_fm+dpbin_m+lglhalno+lglhalwt+lglhalwtkg+effskts+hksretriev+hksobs+ineffhks+Purpose+
                    Effective+ineffcode+NMFS_AREA+ADFG_AREA+NMFS_mgmt_area+FMP_sub_area+FMP+Sub_Samp~Species,
                  sum,value="Catch")
    sumdat2[is.na(sumdat2)]<-0
    sumdat2$Catch<-sumdat2$Rockfish_Rougheye+sumdat2$Rockfish_Blackspotted
    sumdat3<-subset(sumdat2,select=-c(Rockfish_Blackspotted,Rockfish_Rougheye))
    sumdat3$RACE_CODE<-0
    sumdat3<-cbind(Species="Rockfish_REBS",sumdat3)
    mdat<-mdat[!(mdat$RACE_CODE==30050|mdat$RACE_CODE==30052),]
    mdat<-rbind(mdat,sumdat3)
  }
  mdat<-merge(mdat,code_list[,c("IPHC_code","Species")],by="IPHC_code",all.x=T)
  species<-unique(mdat$Species)
  n_station<-merge(n_station,species,all=T)
  colnames(n_station)[6]<-c("Species")
  
  #adds in area size and gets rid of inside waters
  mdat3<-merge(unique(areasize[,c("RPN_strata","FMP_sub_area","area_kmsq")]),mdat,by=c("RPN_strata","FMP_sub_area"),all.y=T) #brings over area size for each FMP_sub_area and depth bin
  
  #extrapolates catch
  mdat3$obs_CPUE<-mdat3$obs_catch/(mdat3$hksobs-mdat3$inefhks)#species CPUE at this station
  mdat3$ex_catch<-mdat3$obs_CPUE*mdat3$ex_eff #calculates extrapolated total catch in numbers
  mdat3$ex_catch[is.na(mdat3$ex_catch)]<-0 #any station with no catch is put as 0
  
  #sum over FMP_sub_area and depth bin
  station_sum<-ddply(mdat3,c("yr","concat","FMP_sub_area","RPN_strata"),summarize,obs_ineff=mean(inefhks,na.rm=T),obs_eff=mean(hksobs,na.rm=T)-obs_ineff) #have to get one value for hooks for each station, this is necessary if running code for more than one species
  st_sum2<-ddply(station_sum,c("yr","FMP_sub_area","RPN_strata"),summarize,obs_ineff=sum(obs_ineff,na.rm=T),obs_eff=sum(obs_eff,na.rm=T)) #gets one value of eff and ineff hooks for each year and area/strata
  sp_sum<-ddply(mdat3,c("yr","FMP_sub_area","RPN_strata","Species"),summarize,obs_catch=sum(obs_catch,na.rm=T),n_pos_station=sum(obs_catch>0))
  
  #add area sizes to summed strata
  area_sum<-merge(sp_sum,n_station,by=c("FMP_sub_area","RPN_strata","yr","Species"),all.y=T)
  area_sum<-merge(area_sum,st_sum2,by=c("yr","FMP_sub_area","RPN_strata"),all.x=T)
  #area_sum<-merge(area_sum,unique(areasize[,c(6,5)]),by=c("RPN_strata"),all.x=T)
  area_sum[is.na(area_sum)]<-0
  
  #calculate CPUE
  area_sum$CPUE<-area_sum$obs_catch/area_sum$obs_eff
  
  #calculate RPN
  area_sum$RPN<-area_sum$CPUE*area_sum$area_kmsq
  
  #get bootstrap CI for CPUE and RPN
  boot_RPN(mdat3,ITER,AREA=areasize) #when this step is running, it prints a progress bar
  boot_out<-read.csv(paste(getwd(),"/CPUEoutmat.csv",sep=""), header=T)
  
  print("Finalizing estimates")
  #combine boot strap output with area summed CPUE/RPN results
  final_out<-merge(area_sum,boot_out,by=c("yr","FMP_sub_area","RPN_strata","Species"))
  
  #calculate RPNs directly from CPUE CI for comparison
  final_out$calc_LLRPN<-as.numeric(as.character(final_out$LLCPUE))*final_out$area_kmsq
  final_out$calc_ULRPN<-as.numeric(as.character(final_out$ULCPUE))*final_out$area_kmsq
  
  #final_out<-subset(final_out,select=-c(concat_meters))
  
  #output table
  if(out==T){
    ifelse(is.null(sp_code),name<-"IPHC_RPN_all_species.csv",name<-paste("IPHC_RPN_",outname,YR,".csv",sep=""))
    write.table(final_out,name,sep=",",row.names=F) 
  }
  #print("Making AWESOME summary figures")
  
}

