#This script is designed to extract archived BBMP archived ODF's to directory and then,
#extract the date, and parameters at 1, 5, 10 and 60 from 2000 - 2015

#####this section removes all BBMP downcasts from folders in the archive dated by year and puts them into 
#####a single folder dated BBMP in the same directory
#####This should be done once a year in February for the archived data to run the script to create the mean/min/max including the previous year
#####This does not need to be done for every week.

indir="//dcNSbioNA01a/BIODataSvcArc/Archive/ctd/" #directory for CTD archives
outdir="C:/Users/cogswella/Documents/AZMP/CTD_archive/BBMP" #directory for the ouput of the compiled ODF data
dir_curr="//dcnsbiona01a/BIODataSvcSrc/BBMP/COMPASS/" #directory for CTD casts from the current year

ys<-2000 #start year
ye<-(as.numeric(format(Sys.Date(), "%Y")))-1 #end year

#loop identifies and copies all BB CTD files from archive (indir) to outdir
for (i in ys:ye){
  setwd(paste(indir,i,sep=""))  
  list=list.files(pattern="*CTD_BCD.*667.*DN.*.ODF$")
    for (n in 1:length(list)){
      file.copy(from=list[n],to=outdir, overwrite=T,recursive=F)
    }
}


#####This section strips all of the 2, 5, 10 and 60 m data from the weekly ODF files, moves them to their own variable
#####and reshapes them to prepare for export to CSV.  This would have to be run yearly to include the current year in these data


#install.packages("oce")
#install.packages("ocedata")
library(oce)
library(ocedata)
library(dplyr)
library(tidyr)

setwd(outdir)
l=length(dir())

ctd=read.oce(dir()[1])
d<-ctd@metadata$date
datain<-subset(ctd@data,ctd@data$pressure==2|ctd@data$pressure==5|ctd@data$pressure==10|ctd@data$pressure==60)
datain$date<-as.Date(substr(d,1,10))
datain<-select(datain,date,pressure,temperature,salinity,sigmaTheta)
dataout<-tidyr::gather(datain,parameter,value,3:5)

for (n in 2:l-1){
  ctd=read.oce(dir()[n])
  d<-ctd@metadata$date
  datain<-subset(ctd@data,ctd@data$pressure==2|ctd@data$pressure==5|ctd@data$pressure==10|ctd@data$pressure==60)
  datain$date<-as.Date(substr(d,1,10))
  datain<-select(datain,date,pressure,temperature,salinity,sigmaTheta)
  datagather<-tidyr::gather(datain,parameter,value,3:5)
  dataout<-rbind(dataout,datagather)
}

dataout<-dplyr::arrange(dataout,date)
write.csv(dataout,paste(outdir,"/supporting files/BBMP_TS_",ys,"_",ye,".csv",sep=""),row.names=F)

