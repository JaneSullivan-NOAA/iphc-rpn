#####################################
#Functions developed to calculate RPNs
#and CPUE for just GOA/BSAI, and coastwide (CPUE only)
#remember RPNs are additive, so it is run 
#at the highest data resolution, with CPUE 
#estimates at that resolution. CPUE function
#outputs summary graphs too.
#Last updated 2019-7-24
#By: Cindy Tribuzio


source("IPHC_RPN_CPUE_functions_v3.R")



############################################################3
#Below are codes to run specific groups of species, or all species combined.

########
#NOTE: CPUE function is not currently set up to accept lists of species, need to run by individual species
#fix next year
#set up new function to run both RPN and CPUE on list of species with out put figures

#Enter current year update
YR<-2018
ITER<-1500

#for sablefish, this one is about 1.5 mins on my machine
IPHC_RPN(sp_code=20510,out=T,outname="Sablefish",YR=YR,ITER=ITER)
IPHC_CPUE(sp_code=20510,outname="Sablefish",YR=YR,ITER=ITER)
IPHC_qc_figs(sp_code=20510,outname="Sablefish",YR=YR)

#for OROX
OROX_code<-c(30340,30400,30410,30370,30120,30170,30200,30320,30475,30430,30270,30560,30100,30190,30380,30350,30220,30470,30600,30240)
IPHC_RPN(sp_code=OROX_code,out=T,outname="OROX",YR=YR,ITER=ITER)
IPHC_CPUE(sp_code=OROX_code,outname="OROX",YR=YR,ITER=ITER)
IPHC_qc_figs(sp_code=OROX_code,outname="OROX",YR=YR)

#for ALL rockfish, the IPHC survey treats RE and BS separate, so the REBS=T statement activates a chunk of code to combine them
#ROX_code<-c(30340,30400,30410,30370,30120,30170,30200,30320,30475,30430,30270,30560,30100,30190,30380,30350,30220,
#30470,30600,30240,30052,30150,30420,30060,30050,30576)
#IPHC_RPN(sp_code=ROX_code,out=T,outname="ROX",YR=YR,ITER=ITER,REBS=T)
#haven't done CPUE for all of the rockfish, probably not necessary

#DSR+redbanded, this is actually redundant with above
DSR_code<-c(30410,30370,30120,30320,30270,30380,30470,30475)
IPHC_RPN(sp_code=DSR_code,out=T,outname="DSR",YR=YR,ITER=ITER)
IPHC_CPUE(sp_code=DSR_code,outname="DSR",YR=YR,ITER=ITER)
IPHC_qc_figs(sp_code=DSR_code,outname="DSR",YR=YR)

#for sharks
shark_code<-c(310,320,232) #only includes main shark species for now
IPHC_RPN(sp_code=shark_code,out=T,outname="sharks",YR=YR,ITER=ITER)
IPHC_CPUE(sp_code=shark_code,outname="sharks",YR=YR,ITER=ITER)
IPHC_qc_figs(sp_code=shark_code,outname="sharks",YR=YR)

#for Pcod
IPHC_RPN(sp_code=21720,out=T,outname="Pacific Cod",YR=YR,ITER=ITER)
IPHC_CPUE(sp_code=21720,outname="Pacific Cod",YR=YR,ITER=ITER)
IPHC_qc_figs(sp_code=21720,outname="Pacific Cod",YR=YR)

#for SR
IPHC_RPN(sp_code=30576,out=T,outname="Shortraker Rockfish",YR=YR,ITER=ITER)
IPHC_CPUE(sp_code=30576,outname="Shortraker Rockfish",YR=YR,ITER=ITER)
IPHC_qc_figs(sp_code=30576,outname="Shortraker Rockfish",YR=YR)

#for SST
IPHC_RPN(sp_code=30020,out=T,outname="Short-spined Thornyhead",YR=YR,ITER=ITER)
IPHC_CPUE(sp_code=30020,outname="Short-spined Thornyhead",YR=YR,ITER=ITER)
IPHC_qc_figs(sp_code=30020,outname="Short-spined Thornyhead",YR=YR)

#for RE/BS
rebs_code<-c(30050,30051,30052)
IPHC_RPN(sp_code=rebs_code,out=T,outname="REBS",YR=YR,ITER=ITER,REBS=T)
#need to double check CPUE code if it needs any adjustments for REBS issues, probably

#for Greenland turbot
IPHC_RPN(sp_code=10115,out=T,outname="Greenland Turbot",YR=YR,ITER=ITER)
IPHC_CPUE(sp_code=10115,outname="Greenland Turbot",YR=YR,ITER=ITER)
IPHC_qc_figs(sp_code=10115,outname="Greenland Turbot",YR=YR)

#for all species, note this is SUPER slow, like takes DAYS
#IPHC_RPN(sp_code=NULL,out=T,YR=YR,ITER=ITER)
#not set up for CPUE yet
