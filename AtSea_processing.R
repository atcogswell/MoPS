##Loading libraries is only necessary on the first run.  Save history so these don't need to be
##run again.

library(rgdal)
library(dismo)
library(raster)
library(maptools) 
library(rgeos)

##Set working directory using:

#wd=choose.dir(default="", caption="Please Select Your Working Directory")

##or set directory of your choice manually.  Data input and output folder.

wd="C:/Users/cogswella/Documents/AZMP/Missions/2015/Spring/At Sea/HUD2015004/Navigation/CruiseTrack"

setwd(wd) #set your working directory

##Choose your input file
file=file.choose()
track=read.csv(file, stringsAsFactors=F)
file2=substr(basename(file), 1, nchar(basename(file)) - 4) 
track=track[,c(1,3,2)]
names(track)=c("GMT","lon_dd","lat_dd")

l=nrow(track)#number of data rows for loop to add fields

lon_dd_e=0
lon_dd_e[1:max(l-1)]=track$lon_dd[-1]
lon_dd_e[max(l)]=track$lon_dd[max(l)]

lat_dd_e=0
lat_dd_e[1:max(l-1)]=track$lat_dd[-1]
lat_dd_e[max(l)]=track$lat_dd[max(l)]

track$lon_dd_e=lon_dd_e
track$lat_dd_e=lat_dd_e

track1=track[,2:3]
track2=track[,1:length(track)]

track3=SpatialPointsDataFrame(track1, track2, coords.nrs = numeric(0),proj4string = CRS("+proj=longlat +datum=WGS84"), match.ID = TRUE, bbox = NULL)
writeOGR(track3, wd, file2, driver="ESRI Shapefile",overwrite_layer=TRUE)

