#install.packages("oce")
#install.packages("ocedata")
library(oce)
library(ocedata)
setwd("C:\\Users\\cogswella\\Documents\\Bedford Basin Monitoring Program\\Data")

# setwd("//dcnsbiona01a/BIODataSvcSrc/BBMP/COMPASS/2016")
# out=c("//dcnsbiona01a/BIODataSvcIn/_BIOWeb/BBMP/2016/","R:/Shared/Cogswell/_BIOWeb/BBMP/2016/")
# outroot=c("//dcnsbiona01a/BIODataSvcIn/_BIOWeb/BBMP/","R:/Shared/Cogswell/_BIOWeb/BBMP/")


odf_file_list = list.files(pattern="*^.*D.*.ODF$")
l=length(list)

for (i in 1:2){    
  for (n in 1:l) {
    ctd=read.oce(list[n])
    png(paste(out[i],"BBMP",substr(ctd@metadata$date,1,11),'.png',sep=""),height=800,width=800)
    plot.new()
    par(oma=c(0,0,2,0))
    par(mfrow=c(2,2)) # four panels, filled in reading order
    plot(ctd,which=1, keepNA=T)
    plot(ctd,which=2)
    plot(ctd,which=3)
    par(mar=c(3.6,3.4,3.5,2))
    plot(ctd@data$fluorometer,ctd@data$pressure, ylim=rev(range(ctd@data$pressure)), type="l",col="green", xlab="Fluorescence [mg/m^3]", ylab="",col.lab="green")
    axis(1, labels = T, col.ticks = "green",col.axis = 'green')
    par(new=T)
    min_oxy=0
    #min_oxy=round(min(ctd@data$oxygen,na.rm=T),0)-1
    max_oxy=round(max(ctd@data$oxygen,na.rm=T),0)
    if(max_oxy>0) plot(ctd@data$oxygen,ctd@data$pressure, ylim=rev(range(ctd@data$pressure)), type="l",col="black", axes=F, xlab="", ylab="Pressure [dbar]") else (mtext("Oxygen was not collected.")) 
    title("Oxygen [ml/l]", line=2, font.main=1, cex.main=1)
    Axis(side=3, x=ctd@data$oxygen, at=c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10))
    #plot(ctd,which=0)
    title(paste("Compass Buoy Station CTD Profile",  ctd@metadata$date,sep=": "),outer=TRUE,cex=1.4) # title for overall plot (filename, here)
    dev.off()
  }
}

for (i in 1:2){
  ctd=read.oce(list[l])
  png(paste(outroot[i],"Recent_Profile.png",sep=""),height=800,width=800)
  plot.new()
  par(oma=c(0,0,2,0))
  par(mfrow=c(2,2)) # four panels, filled in reading order
  plot(ctd,which=1, keepNA=T)
  plot(ctd,which=2)
  plot(ctd,which=3)
  par(mar=c(3.6,3.4,3.5,2))
  plot(ctd@data$fluorometer,ctd@data$pressure, ylim=rev(range(ctd@data$pressure)), type="l",col="green", xlab="Fluorescence [mg/m^3]", ylab="",col.lab="green")
  axis(1, labels = T, col.ticks = "green",col.axis = 'green')
  par(new=T)
  min_oxy=0
  #min_oxy=round(min(ctd@data$oxygen,na.rm=T),0)-1
  max_oxy=round(max(ctd@data$oxygen,na.rm=T),0)
  if(max_oxy>0)plot(ctd@data$oxygen,ctd@data$pressure, ylim=rev(range(ctd@data$pressure)), type="l",col="black", axes=F, xlab="", ylab="Pressure [dbar]") else (mtext("Oxygen was not collected.")) 
  title("Oxygen [ml/l]", line=2, font.main=1, cex.main=1)
  Axis(side=3, x=ctd@data$oxygen, at=c(1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10))
  #plot(ctd,which=0)
  title(paste("Compass Buoy Station CTD Profile",  ctd@metadata$date,sep=": "),outer=TRUE,cex=1.4) # title for overall plot (filename, here)
  dev.off()
  
}