# Sections will be run in the order presented:
# shift+alt+o to expand all sections
# shift+alt+l to expand current section
# alt+o to collapse all sections
# alt+l to collapse current section

#### packages and libraries (more than is necessary but just in case I need them for future development) ----

if (!require('oce')) devtools::install_github('dankelley/oce')
if (!require('dplyr')) install.packages("dplyr")
if (!require("rgdal")) install.packages("rgdal")
if (!require("dismo")) install.packages("dismo")
if (!require("raster")) install.packages("raster")
if (!require("maptools")) install.packages("maptools")
if (!require("rgeos")) install.packages("rgeos")
if (!require("mapview")) install.packages("mapview")
if (!require("shiny")) install.packages("shiny")
if (!require('devtools')) install.packages('devtools')
if (!require('htmltools')) install.packages('htmltools')
if (!require('leaflet')) devtools::install_github('rstudio/leaflet')


library(oce)
library(ocedata)
library(rgdal)
library(dismo)
library(raster)
library(maptools) 
library(rgeos)
library(mapview)
library(leaflet)
library(dplyr)
library(htmltools)
library(htmlwidgets)
library(tools)

#### Input data to run scrip: Search Area Name, Extent, Start and End Years, Data Output Directories----

sl<-"BBL_07" #Enter the name for your search area

# Enter your search area extent
maxn<-41.967
minn<-41.767
maxw<-(-65.450)
minw<-(-65.250)

# Enter the start and end years of your ODF search from the ODF archive (current year is likely excluded from the database)
ys<-2015
ye<-2015

# Enter your output working directory
wd<-("C:\\Users\\CogswellA\\Documents\\AZMP\\Requests\\Test2\\") #establisheds the working directory for the meta-data summary export

#### select all ODF files into list from folders in archive ----
plist<-list() #creates empty list to accept the filtered ODF files
for (i in ys:ye){
  
  tmpwd<-paste("R:\\Science\\BIODataSvc\\ARC\\Archive\\ctd\\",i,sep="")
  setwd(tmpwd) #working directory is the year within the ctd archive folder within the loop
  dn_ODF_files <- list.files(pattern="*^.*DN.*.ODF$") # only selects down casts using the DN in the file name
  if (i!= ys) {select_ODF_files<-dn_ODF_files[-grep("BCD", dn_ODF_files, fixed=T)]} else {select_ODF_files<-dn_ODF_files} 
  
  ylist<-list()
  x=0
  
  #this loop pulls all odf files from the folder and spatially filters and selects them into ylist
  for (n in 1:length(select_ODF_files)){
    tmp_odf<-read.odf(select_ODF_files[n])
    if (tmp_odf[['latitude']]<=maxn & tmp_odf[['latitude']]>=minn & tmp_odf[['longitude']]>=maxw & tmp_odf[['longitude']]<=minw)
    {x<-x+1;ylist[x]<-tmp_odf}
    
  }
 
  
  plist[paste(i,sep=" ")]<-list(ylist) #plist contains the selected ODF files ordered by year
   
}

setwd(wd)

#### acquire the metadata for the selected odf files for plotting and export ----
pl<-length(plist) #basically how many years in plist
mdata_all_years<-NULL #creates null metadata file to accept filename, starttime, ship, station, lon, lat and depth for each ODF

for (a in 1:pl){
  
  yl<-length(plist[[a]]) #how many odf files within the year
  
  mdata<-NULL
  for (b in 1:yl){
    
    tmp<-plist[[a]][b]
    fname<-as.data.frame(tmp[[1]]@metadata$filename)
    stime<-as.data.frame(tmp[[1]]@metadata$startTime)
    ship<-as.data.frame(tmp[[1]]@metadata$ship)
    station<-as.data.frame(tmp[[1]]@metadata$station)
    lon<-as.data.frame(tmp[[1]]@metadata$longitude)
    lat<-as.data.frame(tmp[[1]]@metadata$latitude)
    mdep<-as.data.frame(tmp[[1]]@metadata$waterDepth)
  
    tmp_mdata<-NULL
    tmp_mdata<-cbind(fname,stime,ship,station,lon,lat,mdep) 
    names(tmp_mdata)<-c("filename","starttime","ship","station","longitude","latitude","depth")
    if (b==1) {mdata<-tmp_mdata} else{mdata<-rbind(mdata,tmp_mdata)}
    
  }
 
  if (a==1) {mdata_all_years<-mdata} else{mdata_all_years<-rbind(mdata_all_years,mdata)}
   
}

#### export file metadata and list of odf files ----

fname_csv<-paste(sl,"_",ys,"_",ye,"_ODFSummary_",as.numeric(format(Sys.time(), "%Y%m%d%H%M%S")),".csv",sep="") #file name for your csv output
write.csv(mdata_all_years,fname_csv, row.names=F) #save your file meta data in .csv format in your wd
fname_rds<-paste(sl,"_",ys,"_",ye,"_ODFList_",as.numeric(format(Sys.Date(), "%Y%m%d%H%M%S")),".rds",sep="") #file name for your odf list output
saveRDS(plist,fname_rds) #save your odf data in list form for easy access in R without compiling

#These portions are useful if you do not want to compile these files again
#mdata_all_years<-read.csv(fname_csv) #load your metadata
#plist<-readRDS(fname_rds) #load your odf list for viewing below

#### plot extent of selections and view metadata----

AZMPcore <- readOGR("C:\\Users\\CogswellA\\Documents\\AZMP\\Missions\\ArcGIS Projects\\AZMP_PMZA_Coordinates.shp")
AZMPcore<- AZMPcore@data
AZMPcore_fixed<-subset(AZMPcore, (!is.na(AZMPcore$station_na)))
AZMPcore_section<-subset(AZMPcore, (is.na(AZMPcore$station_na)))

odfmap<-leaflet(mdata_all_years) %>%
  fitBounds(min(mdata_all_years$longitude),min(mdata_all_years$latitude),max(mdata_all_years$longitude),max(mdata_all_years$latitude)) %>%
  addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}', 
           attribution = 'Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC')%>%  # Add awesome tiles
  addCircles(lng=mdata_all_years$longitude,lat=mdata_all_years$latitude, weight = 5, radius=10, color="black", stroke = TRUE,opacity=0.5,group="CTD Subset Locations",
             fillOpacity = 1,popup=paste ("File Name:",mdata_all_years$filename,"|", "Start Time:", mdata_all_years$starttime,"|","Lon DD:", round(mdata_all_years$longitude,3), "|","Lat DD:",round(mdata_all_years$latitude,3),"|","Depth m:",mdata_all_years$depth,sep=" "))%>%
  addCircles(lng=AZMPcore_section$lon_dd,lat=AZMPcore_section$lat_dd, weight = 10, radius=10, color="yellow", stroke = TRUE,opacity=0.5,group="AZMP Core Section Stations",
             fillOpacity = 1,popup=paste ("Section Name:",AZMPcore_section$section_na,"|", "Station Name:", AZMPcore_section$section_st,"|","Lon DD:", AZMPcore_section$lon_dd, "|","Lat DD:",AZMPcore_section$lat_dd,"|","Depth m:",AZMPcore_section$depth_m,sep=" "))%>%
  addCircles(lng=AZMPcore_fixed$lon_dd,lat=AZMPcore_fixed$lat_dd, weight = 10, radius=10, color="Red", stroke = TRUE,opacity=0.5,group="AZMP Fixed Stations",
             fillOpacity = 1,popup=paste ("Station Name:",AZMPcore_fixed$station_na,"|", "Lon DD:", AZMPcore_fixed$lon_dd, "|","Lat DD:",AZMPcore_fixed$lat_dd,"|","Depth m:",AZMPcore_fixed$depth_m,sep=" "))%>%
  addLabelOnlyMarkers(lng=AZMPcore_section$lon_dd, lat=AZMPcore_section$lat_dd,label =  as.character(AZMPcore_section$section_st),group="Section Labels", 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
  addLabelOnlyMarkers(lng=AZMPcore_fixed$lon_dd, lat=AZMPcore_fixed$lat_dd,label =  as.character(AZMPcore_fixed$station_na),group="Fixed Station Labels", 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))%>%
  addLegend("bottomright", colors= c("black", "yellow", "red"), labels=c("CTD Locations","AZMP Core Section Stations","AZMP Fixed Stations"), title=paste("Map of ODF files near ", sl, " between ",ys, " and ",ye, ", ", " created on ",Sys.Date(), " (", minn,", ",maxn,", ",minw,", ",maxw,").", sep=""),opacity=1)%>% 
  addScaleBar("bottomleft",options=scaleBarOptions(maxWidth=100,imperial=T,metric=T,updateWhenIdle=T))%>%
  addLayersControl(
    overlayGroups = c("CTD Subset Locations", "AZMP Core Section Stations", "AZMP Fixed Stations", "Section Labels", "Fixed Station Labels"),
    options = layersControlOptions(collapsed = TRUE)
  )

odfmap

odf_html<-paste(sl,"_",ys,"_",ye,"_ODFmap_",as.numeric(format(Sys.Date(), "%Y%m%d%H%M%S")),".html",sep="")

saveWidget(odfmap,odf_html)  

#### copy selected ODF files to specified directory and zip them ----


subDir <- paste("ODF_",as.numeric(format(Sys.Date(),"%Y%m%d%H%M%S")),sep="")

if (file.exists(subDir)){
  setwd(file.path(wd, subDir))
} else {
  dir.create(file.path(wd, subDir))
  setwd(file.path(wd, subDir))
  
}

dataout<-getwd()#temporary working directory for odf dataout
ml<-nrow(mdata_all_years)

for (c in 1:ml){
  
  file.copy(mdata_all_years$filename[c],dataout,overwrite = T, recursive=F)
  
}


odf_summary_file <- paste(dataout, "/ODFSUMMARY.tsv", sep = "")
cat(paste("Folder consists of ", 
          ml,
          " ODF files between ",
          ys,
          " and ",
          ye,
          " near ",
          sl,
          " (",
          minn,
          ", ",
          maxn,
          ", ",
          minw,
          ", ",
          maxw,
          ").",
          sep=""), 
    file = odf_summary_file, sep = "\n", append = FALSE)
cat("", file = odf_summary_file, sep = "\n", append = TRUE)
write.table(mdata_all_years, file = odf_summary_file, append = TRUE, quote = TRUE, sep=",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)


files2zip<-dir(dataout,full.names=T)

setwd(wd)
zip(zipfile=paste("ODFZIP_",as.numeric(format(Sys.Date(),"%Y%m%d%H%M%S")),sep=""),files=files2zip)
