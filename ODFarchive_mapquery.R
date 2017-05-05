
rm(list=ls())
##Libraries to load ####
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

## Section that compiles all CTDs from the ODF archive and separates out Bedford Basin profiles and maps them  ####
loc<-data.frame(fname= character(0), dir=character(0),date= character(),lon= numeric(0), lat = numeric(0),depth=numeric(0))


for (n in 1999:2016){
setwd(paste("R:/Science/BIODataSvc/ARC/Archive/ctd/",n,"/",sep=""))
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

## This section allows us the read in the Bedford Basin profiles from 1999 to 2016 (filterted by locaiton and depth) and maps them color coded by year####
setwd("C:/Users/CogswellA/Documents/AZMP/Bedford Basin Monitoring Program/2017/Website")
bbo<-read.csv("BedfordBasinOccupations.csv")
bbo$datetime<-as.POSIXct(bbo$datetime, format="%d/%m/%Y %H:%M")
bbo<-dplyr::arrange(bbo,datetime)
bbo<-subset(bbo,bbo$depth>=50)
bn<-nrow(bbo)
write.csv(bbo,"BedfordBasinOccupations_gt50m.csv",row.names=F)

library(RColorBrewer)

pal <- brewer.pal(11, "Spectral")

#now make it more continuous 
#as a colorRamp
pal <- colorRampPalette(pal)

#now, map it to your values
library(classInt)

palData <- classIntervals(bbo$year, n=17,style="fixed", fixedBreaks=c(1999:2017))

#note, we use pal(100) for a smooth palette here
#but you can play with this
bbo$colors <- findColours(palData, pal(100))
bbo$freq<-1
bboyc<-aggregate(bbo$freq, by=list(Category=bbo$year), FUN=sum)
bboyc<-dplyr::arrange(bboyc,desc(Category))

route<-leaflet(bbo) %>%
  fitBounds(-63.6776,44.6637,-63.6042,44.7292) %>%
  addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}', 
           attribution = 'Tiles &copy; Esri &mdash; National Geographic, Esri, DeLorme, NAVTEQ, UNEP-WCMC, USGS, NASA, ESA, METI, NRCAN, GEBCO, NOAA, iPC')%>%  # Add awesome tiles
  addCircles(lng=bbo$lon, lat=bbo$lat, weight = 5, radius=10, color=bbo$colors,stroke = TRUE, opacity=.5,group="CTD Bedford Basin",
             fillOpacity = 1, popup=paste ("File Name:",bbo$fname,"|","Date:",bbo$datetime,"|","Latitude:",bbo$lat,"|","Longitude:",bbo$lon,"|","Depth:",bbo$depth,"m", "|","Directory:",bbo$dir, "|", bn, "CTD(s) in Bedford Basin 1999 - 2009 in ODF archives", sep=" "))%>%
  addLegend("bottomright", colors= unique(bbo$colors), labels=paste(unique(bbo$year),bboyc$x,sep=": "), title=paste("1999 - 2016 CTDs ",Sys.Date(),".",sep=""),opacity=1)%>% 
  addScaleBar("bottomleft",options=scaleBarOptions(maxWidth=150,imperial=T,metric=T,updateWhenIdle=T))%>%
  addLayersControl(
    overlayGroups = c("CTD Bedford Basin"),
    options = layersControlOptions(collapsed = TRUE)
  )

route

# plot seciton of desired time and variable for Bedford Basin from profiles ####
# Ask question to Clark about how to adjust x axis and plot anomalies

bbos<-subset(bbo,bbo$year==2012|bbo$year==2013|bbo$year==2014|bbo$year==2015|bbo$year==2016)
t<-nrow(bbos)
ctds<-vector("list",t)


#for (r in 1:nrow(bbo)){
  for (r in (1:t)){
  
  fn<-paste(bbos$dir[r],bbos$fname[r],sep="/")
  ctd<-read.ctd.odf(fn)
  ctds[[r]]<-ctd
  
}

sec<-as.section(ctds)
plot(sec,which="temperature",xtype="time",ztype="image",showBottom=F,axes=F)
axis.POSIXct(1, at=timeat, format='%d-%b')
axis(2, at=pretty(sdepth))

