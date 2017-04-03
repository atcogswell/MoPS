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



loc<-data.frame(fname= character(0), dir=character(0),date= character(),lon= numeric(0), lat = numeric(0),depth=numeric(0))


for (n in 1999:2016){
setwd(paste("//dcnsbiona01a/BIODataSvcArc/Archive/ctd/",n,"/",sep=""))
lst1<-list.files(pattern="*^.*DN.*.ODF$")
lst2<-list.files(pattern="*^.*dn.*.odf$")

lst<-append(lst1,lst2)
l<-length(lst)



for (i in 1:l){
  
 tryCatch({
   
  ctd<-read.ctd.odf(lst[i])
  lon<-ctd@metadata$longitude
  lat<-ctd@metadata$latitude
  dep<-ctd@metadata$waterDepth
  d<-ctd@metadata$startTime
  fname<-lst[i]
  station<-ctd@metadata$station
  tmp<-NULL
  tmp$fname<-fname
  tmp$dir<-getwd()
  tmp$datetime<-d
  tmp$lon<-lon
  tmp$lat<-lat
  tmp$depth<-dep
  tmp<-as.data.frame(tmp)
  loc<-rbind(tmp,loc)},error=function(e){cat("ERROR :",conditionMessage(e), " ",fname, "/n")}) 
  
  }
}


loc2<-loc
loc2$date<-as.Date(format(loc2$datetime,"%Y-%m-%d"))
loc2$year<-as.numeric(format(loc2$date,"%Y"))
loc2$month<-as.numeric(format(loc2$date,"%m"))
loc2day<-as.numeric(format(loc2$date,"%d"))
locbb<-subset(loc2,loc2$lat> 44.6637 & loc2$lat< 44.7292 & loc2$lon< -63.6042 & loc2$lon> -63.6776)
# of Bedford Basin CTD profiles total
bn<-nrow(locbb)

loco<-anti_join(loc2, locbb)
on<-nrow(loco)
##write summary csv that has same order of variables as shapefile

setwd("C:/Users/CogswellA/Documents/AZMP/Bedford Basin Monitoring Program/2017/Website")

write.csv(locbb, "BedfordBasinOccupations.csv", row.names=F)

library(htmlwidgets)


route<-leaflet(loc) %>%
  fitBounds(-63.6776,44.6637,-63.6042,44.7292) %>%
  addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}', 
           attribution = 'Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC')%>%  # Add awesome tiles
  addCircles(lng=locbb$lon, lat=locbb$lat, weight = 5, radius=10, color="red",stroke = TRUE, opacity=.5,group="CTD Bedford Basin",
             fillOpacity = 1, popup=paste ("File Name:",locbb$fname,"|","Date:",locbb$datetime,"|","Latitude:",locbb$lat,"|","Longitude:",locbb$lon,"|","Depth:",locbb$depth,"m", "|","Directory:",locbb$dir, "|", bn, "CTD(s) in Bedford Basin 1999 - 2009 in ODF archives", sep=" "))%>%
  addCircles(lng=loco$lon, lat=loco$lat, weight = 5, radius=10, color="yellow",stroke = TRUE, opacity=.5,group="CTD Other",
             fillOpacity = 1, popup=paste ("File Name:",loco$fname,"|","Date:",loco$datetime,"|","Latitude:",loco$lat,"|","Longitude:",loco$lon,"|","Depth:",loco$depth,"m", "|", "Directory:",loco$dir,"|", on, "CTDs outside Bedford Basin 1999 - 2009 in ODF archives",sep=" "))%>% 
  addLegend("bottomright", colors= c("Red","yellow"), labels=c((paste("CTD Bedford Basin:",bn,sep=" ")), (paste("CTD Other:",on,sep=" "))), title=paste("1999 - 2016 CTD Map created on ",Sys.Date(),".",sep=""),opacity=1)%>% 
  addScaleBar("bottomleft",options=scaleBarOptions(maxWidth=150,imperial=T,metric=T,updateWhenIdle=T))%>%
  addLayersControl(
    overlayGroups = c("CTD Bedford Basin","CTD Other"),
    options = layersControlOptions(collapsed = TRUE)
  )

route

library(tools)   # unless already loaded, comes with base R
route_html<-"BBCTD_99_16.html"

saveWidget(route,route_html)
