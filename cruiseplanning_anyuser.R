# - General Comment
## - Section must be run but not edited
### - Section that may have to be run
#### - Parameter that will be necessary to change

# Sections must be run in the order presented:

### 1. Load Packages only if they are not already installed----

install.packages("rgdal")
install.packages("dismo")
install.packages("raster")
install.packages ("maptools")
install.packages ("rgeos")
install.packages ("mapview")

### 2. Loading libraries ---- 

library(rgdal)
library(dismo)
library(raster)
library(maptools) 
library(rgeos)
library(mapview)
library(leaflet)

### 3. set working directories is only necessary on the first run.----  
##Save history so these don't need to be run again.

##Set working directory using:
  
#wd=choose.dir(default="", caption="Please Select Your Working Directory")

##or set directory of your choice manually.  Data input and output folder.

wd="C:/Users/cogswella/Documents/AZMP/Missions/2017/2017 Spring"

setwd(wd) #set your working directory

##Enter path and file name for ascii bathymetry
#rwd=choose.dir(default="", caption="Please Select Your Working Directory")

##or set directory for ascii bathymetry manually.
  
# AZOMP depth raster - GEBCO 1/4 degree (2014)
#rwd="C:/Users/CogswellA/Documents/AZMP/Requests/Ringuette/azomp_depth.asc"

# AZMP depth raster CHS baythymetry
rwd="C:/Users/cogswella/Documents/AZMP/Missions/ArcGIS Projects/BaseLayers/Baythymetry/CHS_AtlanticBathymetricCompilation/chs15sec1.asc"


# run sections 4-8 with section 7 off when 
# sections 2-8 have been run once with section 7 turned on.

#### 4. Enter Start Date ----
#kt=11 # Enter your transit speed in kts
s=ISOdate(2017, 04, 03, 08) #start date and time for mission (Year, month, day, 24hr time)

#### 5. Choose your input file ----
#file=file.choose()
file="HUD2017000_config3.csv"
data=read.csv(file, stringsAsFactors=F)
file2=basename(file)

l=nrow(data)#number of data rows for loop to add fields
data$ID=seq(from=1, to=max(l))

## 6. Distance and time calculations ----

## The Great circle functions modified from script provided from Jae Choi - https://github.com/jae0/ecomod/blob/master/spatialmethods/src/_Rfunctions/geodist.r
great.circle.distance = function (loc1, loc2, R) {
  
  names(loc1) = c("lon", "lat")
  names(loc2) = c("lon", "lat")
  
  if (is.null(R)) R = 6367.436  # radius of earth (geometric mean) km
  # if R=1 then distances in radians
  if (missing(loc2)) loc2 <- loc1
  pi180 = pi/180
  coslat1 = cos(loc1$lat * pi180)
  sinlat1 = sin(loc1$lat * pi180)
  coslon1 = cos(loc1$lon * pi180)
  sinlon1 = sin(loc1$lon * pi180)
  coslat2 = cos(loc2$lat * pi180)
  sinlat2 = sin(loc2$lat * pi180)
  coslon2 = cos(loc2$lon * pi180)
  sinlon2 = sin(loc2$lon * pi180)
  pp =   cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*%
    t(cbind(coslat2 * coslon2, coslat2 * sinlon2, sinlat2))
  
  d = R * acos(ifelse(pp > 1, 1, pp))
  
  return(d)
}

geodist = function (point, locations, method) {
  
  a = 6378.13700          # WGS84 major axis in km
  f = 1/298.257223563   # the WGS84 flattening parameter .. do not simplify as round-off errors are important
  geometric.mean.radius =  sqrt(6378.13700*6356.75231)
  
  if (method == "vincenty") out = vincenty(point, locations, a, f) #you'd have to load subfunction
  if (method == "great.circle") out = great.circle.distance(point, locations, R=geometric.mean.radius)  
  # great.circle method can handle point-vector and vector-vector data
  return(out)
}

##default is great circle... Vincenty is a more accurate version but is not vectorized (yet).

Coords = c("lon_dd", "lat_dd") # order is important
Result = geodist(data[2:l,Coords],data[1:l,Coords], method="great.circle") #output distance in metres on diagonal in matrix in kilometers

##Extracts distance values from "Result" matrix and calculates distance in nautical miles to 1 decimal place
dist_nm=as.data.frame(diag(Result)*0.539957) # extracts values from diagonal geodist output
dist_nm[max(l),]=0
names(dist_nm)=c("dist_nm")
data$dist_nm=round(dist_nm$dist_nm,1)

##Convert Latitude (Lat) from DD to DM
latDeg=floor(data$lat_dd)
latDec=data$lat_dd-latDeg
latmin=latDec*60
latmin=round(latmin,6)
latmin2=1:l
latmin2=as.character(latmin2)
latmin2=ifelse(latmin<10,(latmin2=paste("0",latmin,sep="")),(latmin2=as.character(latmin))) 
data$lat_dm=as.numeric(paste(latDeg,latmin2,sep=""))

##Conver Longitude (Lon) from DD to DM
options(digits=9)
lonneg=data$lon_dd*-1
lonDeg=floor(lonneg)
lonDec=lonneg-lonDeg
lonmin=lonDec*60
lonmin=round(lonmin,6)
lonmin2=1:l
lonmin2=as.character(lonmin2)
lonmin2=ifelse(lonmin<10,(lonmin2=paste("0",lonmin,sep="")),(lonmin2=as.character(lonmin))) 
data$lon_dm=as.numeric(paste(lonDeg,lonmin2,sep=""))

##This formula calculates your transit time using your distance in nautical miles/vessel transit speed
data$trans_hr=round(data$dist_nm/data$kts,2)
data$arrival[1]="start"
data$departure[1]=as.character(s)

for (n in 2:l){
  
  if(n>=2 & n<=(max(l)-1)) data$arrival[n]=as.character(s+(data$trans_hr[n-1]*3600))
  s=s+(data$trans_hr[n-1]*3600)
  if(n>=2 & n<=(max(l)-1)) data$departure[n]=as.character(s+(data$optime[n]*3600))
  s=s+(data$optime[n]*3600)
  if (n==max(l)) data$departure[n]="End" 
  if (n==max(l)) data$arrival[n]=as.character(s+(data$trans_hr[n-1]*3600))

}

## This part is only necessary if you need to convert ESRI GRID format to ASCII format.##
## You could add another grid (e.g., GEBCO) to be used in your calculations but it is  ##
## not necessary.##

#esrigrid2ascii <- function(inputgrid,outputascii,xmin,xmax,ymin,ymax)
#{  x <- raster(inputgrid)
#   aoi <- extent(xmin,xmax,ymin,ymax)
#   x.crop <- crop(x,aoi)
#   writeRaster(x.crop,outputascii,NAflag=-9999)
#   "DONE!"
#}

#esrigrid2ascii("chs15sec1","chs15sec1.asc",-72, -42, 40, 64)

#This is where to ask the user to enter a shapefile output name

## 7. Extract depth from ASCII - turn on and off ----
depth <- readAsciiGrid(rwd, proj4string=CRS("+proj=longlat +datum=WGS84"))#assigns ASCII grid from rwd to variable name
data1=data[,1:2]
data2=data[,3:length(data)]
data3=SpatialPointsDataFrame(data1, data2, coords.nrs = numeric(0),proj4string = CRS("+proj=longlat +datum=WGS84"), match.ID = TRUE, bbox = NULL)
extval=over(data3, depth)

data=cbind(data,extval)
nc=ncol(data)
data[,nc]=data[,nc]*-1
colnames(data)[nc]="depth_m"

## 8. Prepare data for export as a shape file and .csv and remove depth from type "Transit". and create a html plot for export ----
data1=data[,1:2]
data2=data[,1:length(data)]

data3=SpatialPointsDataFrame(data1, data2, coords.nrs = numeric(0),proj4string = CRS("+proj=longlat +datum=WGS84"), match.ID = TRUE, bbox = NULL)

data3$depth_m=ifelse(data3$type=='Transit', 0, data3$depth_m) #This filter just removes depth values from transit points


## this step adds another field to the shapefile for the end coordinates for xy to route calculation in R
lon_dd_e=0
lon_dd_e[1:max(l-1)]=data3$lon_dd[-1]
lon_dd_e[max(l)]=data3$lon_dd[max(l)]

lat_dd_e=0
lat_dd_e[1:max(l-1)]=data3$lat_dd[-1]
lat_dd_e[max(l)]=data3$lat_dd[max(l)]

data3$lon_dd_e=lon_dd_e
data3$lat_dd_e=lat_dd_e


##These next few steps reorder the data for final export as shape file and csv
##You will likely have to change the variables for your export
##The Sys.Date function applies a date and time stamp at the end of your ouput that has the same
##naming convention as your input csv file.

date=Sys.Date()
date
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
date=substrRight(gsub("-","", date),6)
time=format(Sys.time(),"%H%M")#The 3600 value might be necessary to account for daylight savings.
time=as.character(time)
file2=unlist(strsplit(file2,split='.', fixed=TRUE))[1] #splits original file name and only assigns title without the extension
file3=paste(file2,date,time,sep="_")
file4=paste(file3,".csv", sep="")

## writes point shapefile and planning csv
writeOGR(data3, wd, file3, driver="ESRI Shapefile",overwrite_layer=TRUE)
nc=ncol(data3)+2
data4=as.data.frame(data3)
data4=data4[,1:(nc-2)]
##write summary csv that has same order of variables as shapefile
write.csv(data4, file4, row.names=F)

library(htmlwidgets)
#position of transit points
tpts<-subset(data4,data4$type=="Transit")
#position of operations points
opts<-subset(data4,data4$type=="Operations")
data4sel<-as.matrix(data4[,c(1:2)])
#converts data4 points to lines for inclusion in output map
data4ln<-coords2Lines(data4sel, ID=paste(file,"Route",sep=" "))


route<-leaflet(data4) %>%
  fitBounds(min(data4$lon_dd),min(data4$lat_dd),max(data4$lon_dd),max(data4$lat_dd)) %>%
  addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}', 
           attribution = 'Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC')%>%  # Add awesome tiles
  addPolylines(data=data4ln,color="blue",weight=1,popup=paste(file,"Route",sep=" "),group="Route")%>%
  addCircles(lng=tpts$lon_dd,lat=tpts$lat_dd, weight = 2, radius=10, color="black", stroke = TRUE,opacity=0.5,group="Transit Locations",
             fillOpacity = 1,popup=paste ("ID:",tpts$ID,"|", "Station:", tpts$type,"|","Lon:", round(tpts$lon_dd,4), "|","Lat:",round(tpts$lat_dd,4),"|","Arrival:",tpts$arrival,"|","Departure:",tpts$departure, sep=" "))%>%
  addCircles(lng=opts$lon_dd, lat=opts$lat_dd, weight = 5, radius=10, color="yellow",stroke = TRUE, opacity=0.5,group="Operations Locations",
             fillOpacity = 1, popup=paste ("ID:",opts$ID,"|", "Station:", opts$station,"|","Lon:", round(opts$lon_dd,4), "|","Lat:",round(opts$lat_dd,4), "|","Depth(m):",round(opts$depth_m,1),"|", "Arrival:",opts$arrival,"|","Departure:",opts$departure, sep=" "))%>% 
  addLegend("bottomright", colors= c("yellow", "black","blue"), labels=c("Operations","Transit","Route"), title=file)%>% 
  addLayersControl(
  overlayGroups = c("Operations Locations","Transit Locations","Route"),
  options = layersControlOptions(collapsed = TRUE)
  )

route

library(tools)   # unless already loaded, comes with base R
route_html<-paste(file_path_sans_ext(file),"_",as.numeric(format(Sys.Date(), "%Y%m%d")),".html",sep="")

saveWidget(route,route_html)

## 9. Calculate the total days for the mission ####
et<-nrow(data4) #et=end time
print(paste("The mission",file,"is",round(as.numeric(difftime(strptime(data4$arrival[et],"%Y-%m-%d %H:%M:%S"),strptime(data4$departure[1],"%Y-%m-%d %H:%M:%S"))),1), "days long.",sep=" "))

### 10. AZMP specific formats required for reporting and planning purposes ----

##Organize and sort data for form b - uses degree minutes
formb=as.data.frame(data4[,c("station", "lon_dm", "lat_dm", "depth_m", "operation", "optime", "dist_nm", "trans_hr", "loc1", "kts")])
formb=formb[,c("station", "lon_dm", "lat_dm", "depth_m", "operation", "optime", "dist_nm", "trans_hr", "loc1", "kts")]
file5=paste(file2,"formb",date,time,sep="_")
file5=paste(file5,".csv", sep="")
write.csv(formb, file5, row.names=F)

##Organize and sort data for mission plan - uses decimal degrees
mplan=as.data.frame(data4)
mplan=mplan[,c("station", "lon_dd", "lat_dd", "depth_m", "operation", "optime", "dist_nm", "trans_hr", "loc1", "kts")]
file6=paste(file2,"mplan",date,time,sep="_")
file6=paste(file6,".csv", sep="")
write.csv(mplan, file6, row.names=F)

### 11. This section saves you a little time when putting together reports by determining the total operational time at each location and the transit time to the next----
## You will need to organize 1 column in your input csv file - loc1 with "factors" as location numbers in rows that apply to what is being summed.
## Remember not to skip any cells, but if you do please fill the cell with "NA"

ot=aggregate(optime~loc1,data4,sum) #operational time at each location by factor "loc1"
colnames(ot)=c("worklocation", "opstime")
c=aggregate(optime~loc1,data4,NROW) # this basically counts the number of "work locations"


## for loop to determine the number of transit hours at each work location
tt=as.data.frame(sum(data4$trans_hr[1:(c$optime[1]-1)]))
colnames(tt)=c("transittime")
for (n in 2:nrow(c)){
  
  sr=sum(c$optime[1:n-1])+1
  er=sr+c$optime[n]-2
  tt2=sum(data4$trans_hr[sr:er])
  tt=rbind(tt,tt2)
  
}

#tt=tt[tt[,1]!=0,] # takes out sum of zero row for transit time between stations at a location "loc1"
tt=as.data.frame(tt)
colnames(tt)=c("transittime")

tbl=aggregate(data4$trans_hr, by = list(data4$loc1), FUN = tail, n = 1) # determines the time between locations from "loc1"
tbl=tbl[tbl[,1]!=0,]
tbl=as.data.frame(tbl[2])
colnames(tbl)=c("transittimetonextlocation")

lt=cbind(ot,tt, tbl)
lt$totaltimeatlocation=lt[,2]+lt[,3] #summary of trip hours by work location including ops time and transit
lt=lt[c(1,2,3,5,4)]


#formb
#mplan
file7=paste(file2,"_lt","_",date,"_",time,".csv", sep="")
write.csv(lt,file7,row.names=F)
lt
