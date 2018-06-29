##### Install Packages and load libraries #####
if (!require('oce')) devtools::install_github('dankelley/oce', ref='develop')
if (!require("ocedata")) install.packages("ocedata")


library(oce)
library(ocedata)
#library(rgdal)
#library(dismo)
#library(raster)
#library(maptools) 
#library(rgeos)
#library(mapview)
#library(leaflet)
library(dplyr)
#library(htmltools)
#library(htmlwidgets)
library(tools)
library(lubridate)
#library(xlsx)

##### Set Working Directory and create event parameter from csv #####
ye<-format(Sys.Date(), "%Y")
ye <- as.numeric(ye)
ys <- ye-1
y=c(ys,ye)
sl <- "HL_00"

# Enter your output working directory

lst_tot <- list()

for (i in 1:length(y)) {
  
  
  fwd<-(paste("R:\\Science\\BIODataSvc\\SRC\\BBMP\\COMPASS\\", y[i],"\\", sep="")) #establisheds the working directory for the meta-data summary export

  setwd(fwd)  

  lst_tmp<-list.files(pattern= paste("^.*D",substr(y[i],3,4),"667.*.ODF",sep=""))#list of downcast .cnv for creating section plot

  lst_tot[[i]] <- lst_tmp 
  
}

mdata <- NULL

for (n in 1:length(y)){
  
  fwd<-(paste("R:\\Science\\BIODataSvc\\SRC\\BBMP\\COMPASS\\", y[n],"\\", sep="")) #establisheds the working directory for the meta-data summary export
  
  setwd(fwd)  

for (i in 1:length(lst_tot[[n]])){
  
  tmp_odf<-read.odf(lst_tot[[n]][i])
  tmp_mdata<-NULL
  fname<-as.data.frame(tmp_odf@metadata$filename)
  stime<-as.data.frame(tmp_odf@metadata$startTime)
  ship<-as.data.frame(tmp_odf@metadata$ship)
  lon<-as.data.frame(tmp_odf@metadata$longitude)
  lat<-as.data.frame(tmp_odf@metadata$latitude)
  tmp_mdata<-cbind(fname,stime,ship,lon,lat)
  names(tmp_mdata)<-c("filename","starttime","ship","longitude","latitude")
  
  mdata<-rbind(mdata,tmp_mdata)
  
                            }
}

mdata$year<-substr(mdata$starttime,1,4)

#mdata2<-subset(mdata,mdata$year==y)
mdata2 = mdata[order(mdata[,'starttime']),]
mdata3 = mdata2[!duplicated(mdata2$starttime),]
mdata3$filename<-as.character(mdata3$filename)

#### Compile selected ODF files into locally held list ####

clst<-list(NULL)
for (c in 1:nrow(mdata3)){
  
  tmp_odf<-read.odf(mdata3$filename[c])
  templow <- as.numeric(tmp_odf[['temperature']]) < -2
  temphi <- as.numeric(tmp_odf[['temperature']]) > 30
  sallow <- as.numeric(tmp_odf[['salinity']]) < 28
  salhi <- as.numeric(tmp_odf[['salinity']]) > 36
  tmp_odf[['temperature']][templow] <- "NA" 
  tmp_odf[['temperature']][temphi] <- "NA"
  tmp_odf[['salinity']][sallow] <- "NA" 
  tmp_odf[['salinity']][salhi] <- "NA"
  tmp_odf[['sigmaTheta']][sallow] <- "NA" 
  tmp_odf[['sigmaTheta']][salhi] <- "NA"
  clst[c]<-tmp_odf
  
  
}


#### create section  for Temp, Sal and SigmaT ####

#import the climatology for BBMP
climhl0<-read.csv("R:\\Shared\\Cogswell\\_BIOWeb\\BBMP\\HL_0_2000_2015_Climatology.csv")

#Standard values for the binning similar to Roger Pettipas procedures
#stdvals=(0 10 20 30 50 75 100 125 150 175 200 225 250 300 400 500 600 700 800 1000 1200 1500 2000 2500 3000 3500 4000 4500 5000]
stdvals<-c(0, 10, 20, 30, 40, 50, 60, 70)

# Standard ranges around each standard value - stdvals and stdtols must have equal length
#stdtols=[5  5  5  5  5  5  10  10  10  10  10  10  10  25  25  50  50  50  50  100  100  100  200  200  200  200  200  200  200]
stdtols <- c(5,  5,  5,  5,  5,  5,  5,  5)

ctdmean <- NULL  #this is the empty dataframe that will hold the merged and filtered profiles from the selected area 

for (m in 1:length(clst)){
  
  
  tmp<-clst[[m]]
  tmpP<-as.data.frame(as.numeric(tmp[['pressure']]))
  tmpT<-as.data.frame(as.numeric(tmp[['temperature']]))
  tmpS<-as.data.frame(as.numeric(tmp[['salinity']]))
  tmpSig<-as.data.frame(as.numeric(tmp[['sigmaTheta']]))
  tmpbind<-cbind(tmpP,tmpT, tmpS, tmpSig)
  tmpbind$filename <- basename(tmp@metadata$filename)
  tmpbind$startTime <- tmp@metadata$startTime
  tmpbind$ID <- m
  tmpbind$Month <- as.numeric(substr(tmpbind$startTime,6,7))
  tmpbind$latitude <- as.numeric(tmp[['latitude']])
  tmpbind$longitude <- as.numeric(tmp[['longitude']])
  tmpbind$station <- sl
  tmpbind <- tmpbind[,c(5:11,1:4)]
  names(tmpbind)[8:11] <- c("Pressure", "Temperature", "Salinity", "SigmaT")
  
  tmpsub<-NULL
  tmpmean<-NULL
  
  # This section takes the mean of the data within the profile for specific ranges used by Roger Pettipas
  for (s in 1:length(stdvals)){
    
    svl<-(stdvals[s] - stdtols[s])
    svh<-(stdvals[s] + stdtols[s])
    
    if (svl<max(tmpbind$Pressure)) tmpsub<-subset(tmpbind, tmpbind$Pressure >= svl & tmpbind$Pressure < svh)
    
    if (svl<max(tmpbind$Pressure)) tmpsub$Pressure<-stdvals[s]
    if (svl<max(tmpbind$Pressure)) tmpsub <- as.data.frame (tmpsub %>% 
                                                              group_by(filename, startTime, Month, ID, latitude, longitude,station,Pressure) %>%
                                                              summarise_at(vars(Temperature:SigmaT), mean, na.rm=T))
    
    
    if (svl<max(tmpbind$Pressure)) tmpmean<-rbind(tmpmean, tmpsub)
    
  }
  
  ctdmean <- rbind(ctdmean,tmpmean)
  
  
  
}

#  This section merges the climatology and profile data and calculates the anomaly and standardized anomaly

ctdmean <- ctdmean[complete.cases(ctdmean),]
ctd_clim<-merge(x=climhl0,y=ctdmean, by=c("Month","Pressure"), all.x=T)
ctd_clim <- ctd_clim[complete.cases(ctd_clim),]
ctd_clim <- arrange(ctd_clim,startTime)
ctd_clim <- arrange(ctd_clim,startTime)
ctd_clim$T_anomaly <- (ctd_clim$Temperature-ctd_clim$T_Avg)
ctd_clim$SAL_anomaly <- (ctd_clim$Salinity-ctd_clim$Sal_Avg)
ctd_clim$SIGT_anomaly <- (ctd_clim$SigmaT-ctd_clim$Sigt_Avg)
ctd_clim$T_stdanomaly <- (ctd_clim$Temperature-ctd_clim$T_Avg)/ctd_clim$T_Std
ctd_clim$SAL_stdanomaly <- (ctd_clim$Salinity-ctd_clim$Sal_Avg)/ctd_clim$Sal_Std
ctd_clim$SIGT_stdanomaly <- (ctd_clim$SigmaT-ctd_clim$Sigt_Avg)/ctd_clim$Sigt_Std

ctd_clim_ID <- unique(ctd_clim$ID)

ctd_clim_anom_lst <- list()
mpres <- 0


# basically creates CTD profiles in the OCE format using as.ctd and creates new variables for anomaly calculations above.
for (u in ctd_clim_ID){
  
  ctd_clim_sub <- subset(ctd_clim, ctd_clim$ID==u)
  salinity <- ctd_clim_sub$Salinity
  temperature <- ctd_clim_sub$Temperature
  sigmaT_anom <- ctd_clim_sub$SIGT_anomaly
  salinity_anom <- ctd_clim_sub$SAL_anomaly
  temperature_anom <- ctd_clim_sub$T_anomaly
  sigmaT_stdanom <- ctd_clim_sub$SIGT_stdanomaly
  salinity_stdanom <- ctd_clim_sub$SAL_stdanomaly
  temperature_stdanom <- ctd_clim_sub$T_stdanomaly
  pressure <- ctd_clim_sub$Pressure
  startTime <- unique(ctd_clim_sub$startTime)
  lat <- unique(ctd_clim_sub$latitude)
  lon <- unique(ctd_clim_sub$longitude)
  station <- unique(ctd_clim_sub$station)
  ctd_clim_anom_tmp <- as.ctd(salinity, temperature, pressure, startTime = startTime, longitude = lon, latitude = lat,
                              units=list(pressure=list(unit=expression(dbar), scale="")))
  ctd_clim_anom_tmp <-oceSetData(ctd_clim_anom_tmp, name= "T_anom", value=temperature_anom)
  ctd_clim_anom_tmp <-oceSetData(ctd_clim_anom_tmp, name= "Sal_anom", value=salinity_anom)
  ctd_clim_anom_tmp <-oceSetData(ctd_clim_anom_tmp, name= "SigT_anom", value=sigmaT_anom)
  ctd_clim_anom_tmp <-oceSetData(ctd_clim_anom_tmp, name= "T_stdanom", value=temperature_stdanom)
  ctd_clim_anom_tmp <-oceSetData(ctd_clim_anom_tmp, name= "Sal_stdanom", value=salinity_stdanom)
  ctd_clim_anom_tmp <-oceSetData(ctd_clim_anom_tmp, name= "SigT_stdanom", value=sigmaT_stdanom)
  ctd_clim_anom_lst[u] <- ctd_clim_anom_tmp
  mpres_tmp <- max(ctd_clim_anom_lst[[u]]@data$pressure)
  
  if (mpres_tmp > mpres) mpres <- mpres_tmp
}

# create a section from the ctd list
fsec<-as.section(ctd_clim_anom_lst)

anomnames <- c("T_anom", "Sal_anom", "SigT_anom")

# These are the colour ramps for the anomaly plots - taken directly from Roger Pettipas

blue2<-rgb(0,0,230, maxColorValue=255)
blue3<-rgb(51,51,255, maxColorValue=255)
blue4<-rgb(102,102,255, maxColorValue=255)
blue5<-rgb(151,151,255, maxColorValue=255)
blue6<-rgb(202,202,255, maxColorValue=255)
white1 <- rgb(255,255,255, maxColorValue=255)
white2 <- rgb(255,255,255, maxColorValue=255)
red6 <- rgb(255,202,202, maxColorValue=255)
red5 <- rgb(255,151,151, maxColorValue=255)
red4 <- rgb(255,102,102, maxColorValue=255)
red3 <- rgb(255,51,51, maxColorValue=255)
red2 <- rgb(230,0,0, maxColorValue=255)
red1 <- rgb(157,0,0, maxColorValue=255)

#colour ramp for anomaly
anomramp <- c(blue2, blue3, blue4, blue5, blue6, white1, white2, red6, red5, red4, red3, red2)

#anomaly contour breaks for salinity, sigmaT and temperature.
salseq <- seq(-1.5,1.5,0.25)
sigseq <- seq(-1.5,1.5,0.25)
tempseq <- seq(-6,6,1)

# a list of the anomaly breaks for the loop below.
brkseq <- list(tempseq,salseq,sigseq)

#### Generate the plots ####
fsecgrid <- sectionGrid(fsec, p = stdvals)

nstation <- length(fsecgrid[['station']])
tm <- NULL

for (i in 1:nstation) {
  
  tmp <- ctd_clim_anom_lst[[i]][['startTime']]
  tm[i] <- as.character(tmp) #startime for each of the casts
}

tm <- as.POSIXct(tm, tz="utc")

setwd("R:\\Shared\\Cogswell\\_BIOWeb\\BBMP")
png("CTD_TimeSeriesandAnomalyPlot.png", height=800, width=1800, pointsize = 15)

par(mfrow=c(3,2)) #This creates the template the the following plots are created in.
for (j in 1:length(anomnames)){
  
  tat_1 <- as.POSIXct(paste(y[1],1:12,"01", sep="-"), tz="UTC")
  tat_1 <- as.character(tat_1)
  tat_2 <- as.POSIXct(paste(y[2],1:12,"01", sep="-"), tz="UTC")
  tat_2 <- as.character(tat_2)
  tat_3 <- as.POSIXct(paste(y[2]+1,1,"01", sep="-"), tz="UTC")
  tat_3 <- as.character(tat_3)
  
  tat <- append(tat_1,tat_2)
  tat <- append(tat,tat_3)
  tat <- as.POSIXct(tat, tz="UTC")
  
  marg1 <- c(4,5,3,2.2)
  marg2 <- c(4,5, 3, 6.38)
  lcex <- 0.8
  ladj <- 1.1
  
  plot(fsec, xtype='time', ztype="image", which=j,mar=marg1, ylim=c(70,0),xlim=range(tat),
       showBottom=F,axes=F, ylab="", xlab="", legend="")
  tlabel <- substr(format(tat, '%b'), 1, 1)
  axis(2, at=stdvals, las=1, cex.axis=1.5)
  if (j==1|j==3) axis.POSIXct(3, at=tm, labels=F) else axis (3, at=tm,labels=F, tick=F)
  axis.POSIXct(1, at=tat, labels=tlabel, cex.axis=1.5) 
  mtext("Depth(m)",side=2, line=3, cex=1)
  if (j==1) mtext(expression(paste("T",degree,"C", sep="")), side=1, line=2.0,adj=1.06, cex=lcex, col="black")
  if (j==2) mtext("Sal P.S.U.", side=1, line=2.0,adj=ladj, cex=lcex, col="black")
  if (j==3) mtext("SigT kg/m3", side=1, line=2.0,adj=ladj, cex=lcex, col="black")
  
  abline(v=tat[13], lty=2, col="lightgray")
  if (j==1) mtext(ys, side=3, line=1, cex=1, col="darkgray", adj=0.25)
  if (j==1) mtext(ye, side=3, line=1, cex=1, col="darkgray", adj=0.75)
  
  
  par(new=TRUE)
  plot(fsec, xtype='time', ztype="contour", which=j, mar=marg2, 
       showBottom=F, axes=F,ylab="", xlab="", ylim=c(70,0), xlim=range(tat),xaxt="n", legend="")
  
  
  plot(fsec, xtype='time', ztype="image", which=anomnames[j],
       mar=marg1, ylim=c(70,0),xlim=range(tat),showBottom=F,
       zbreaks=brkseq[[j]], zcol=anomramp, xlab="", ylab="", axes=F, legend="")
  tlabel <- substr(format(tat, '%b'), 1, 1)
  axis(2, at=stdvals, las=1, cex.axis=1.5)
  axis.POSIXct(3, at=tm, labels = F)
  axis.POSIXct(1, at=tat, labels = tlabel, cex.axis=1.5)
  abline(v=tat[13], lty=2, col="black")
  if (j==1) mtext(expression(paste("Anom ",degree, "C")), side=1, line=1.8,adj=1.085, cex=lcex, col="black")
  if (j==2) mtext("Anom P.S.U.", side=1, line=1.8,adj=ladj, cex=lcex, col="black")
  if (j==3) mtext("Anom kg/m3", side=1, line=1.8,adj=ladj, cex=lcex, col="black")
  if (j==1) mtext(ys, side=3, line=1, cex=1, col="darkgray", adj=0.25)
  if (j==1) mtext(ye, side=3, line=1, cex=1, col="darkgray", adj=0.75)
  par(new=TRUE)
  plot(fsec, xtype='time', ztype="contour", which=anomnames[j], 
       mar=marg2, ylim=c(70,0), xlim=range(tat),showBottom=F, 
       axes=F, contourLevels=brkseq[[j]], ylab="", xlab="", legend="")
  
  
  
  
}

le <- length(fsec@data$station)
heading <- as.character(clst[[le]]@metadata$startTime)

mtext(heading,side=3,line=46, cex=1, col="black", adj=-0.25)

dev.off()


