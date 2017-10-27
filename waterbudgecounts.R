library(XLConnect)
library(dplyr)

#compile water budget statistics

setwd("C:\\Users\\CogswellA\\Documents\\AZMP\\Missions\\2017\\2017 Fall") 
missionname<-"fall2017AZMP"

# read in water budget workbook
require(XLConnect)
wb <- loadWorkbook("C:\\Users\\CogswellA\\Documents\\AZMP\\Missions\\2017\\2017 Fall\\Watersamplesanddepths_azmp_Fall_2017_draft.xls")
lst = readWorksheet(wb, sheet = getSheets(wb))


n=length(lst)

c=0
b=0
test=0
chl=0
nuts=0
sal=0
tic=0
poc=0
hplc=0
cyt=0
abs=0
pco2=0



for (i in 1:n){
  
  d<-(as.data.frame(lst[[i]]))
  
  # of CTD's per spreadsheet
  c<-nrow(as.data.frame(na.omit(d$STATION)))+c
  b<-nrow(as.data.frame(na.omit(d$DEPTH)))+b
  
  ##bottle number test
  #test<-sum(na.omit(d$X.BOTTLES))+test
  chl<-(nrow(as.data.frame(na.omit(d$CHL)))*2)+chl
  nuts<-(nrow(as.data.frame(na.omit(d$NUTS)))*2)+nuts
  tic<-nrow(as.data.frame(na.omit(d$TIC.TA)))+tic
  sal<-nrow(as.data.frame(na.omit(d$SAL)))+sal
  poc<-(nrow(as.data.frame(na.omit(d$POC)))*2)+poc
  hplc<-nrow(as.data.frame(na.omit(d$HPLC)))+hplc
  cyt<-nrow(as.data.frame(na.omit(d$CYTO)))+cyt
  abs<-nrow(as.data.frame(na.omit(d$ABS)))+abs
  pco2<-nrow(as.data.frame(na.omit(d$pCO2)))+pco2
}

wbs<-NULL

wbs$CTDS<-c
wbs$CTD_BOTTLES<-b
wbs$CHL_BOTTLES<-chl
wbs$NUTS_BOTTLES<-nuts
wbs$TIC_BOTTLES<-tic
wbs$PCO2_BOTTLES<-pco2
wbs$SAL_BOTTLES<-sal
wbs$POC_BOTTLES<-poc
wbs$HPLC_BOTTLES<-hplc
wbs$CYTO_VIALS<-cyt
wbs$ABS_BOTTLES<-abs


wbs<-as.data.frame(wbs)
date<-format(Sys.Date(),"%Y%m%d")
time<-format(Sys.time(),"%x")
time<-strptime(time,format="%H$M%S")
write.csv(wbs,paste("samplesummary_",missionname,"_",format(Sys.time(), "%Y%m%d_%H%M"),".csv",sep=""),row.names = F)



