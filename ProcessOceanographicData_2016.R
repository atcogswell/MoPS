#The master template for weekly BBMP updates
#install.packages("oce") 
#install.packages("ocedata")
library(oce)
library(ocedata)
#test
setwd("//dcnsbiona01a/BIODataSvcSrc/BBMP/COMPASS/2016")
out=c("//Svnsbiofs02/MARSHARED/Shared/Cogswell/_BIOWeb/BBMP/2016/")
outroot=c("//Svnsbiofs02/MARSHARED/Shared/Cogswell/_BIOWeb/BBMP/") 


list=list.files(pattern="*^.*D.*.ODF$")
l=length(list)

  
  for (n in 1:l) {
    od=read.odf(list[n])
    ctd<-read.ctd.odf(list[n])
    png(paste(out,"BBMP",substr(od@metadata$date,1,10),'.png',sep=""),height=800,width=800)
    plot.new()
    par(oma=c(0,0,2,0))
    par(mfrow=c(2,2)) # four panels, filled in reading order
    plot(ctd,which=1,keepNA=T)
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
    title(paste("Compass Buoy Station CTD Profile",  od@metadata$date,sep=": "),outer=TRUE,cex=1.4) # title for overall plot (filename, here)
    dev.off()
  }



  d=read.odf(list[l])
  ctd<-read.ctd.odf(list[l])
  png(paste(outroot,"Recent_Profile.png",sep=""),height=800,width=800)
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
  title(paste("Compass Buoy Station CTD Profile",  od@metadata$date,sep=": "),outer=TRUE,cex=1.4) # title for overall plot (filename, here)
  dev.off()


#### Copy Recent ODF files to directory for website ########

setwd("//dcnsbiona01a/BIODataSvcSrc/BBMP/COMPASS/2016")
out1="//Svnsbiofs02/MARSHARED/Shared/Cogswell/_BIOWeb/BBMP/ODF/2016"

for (n in 1:l) {

  file.copy(from=list[n],to=out1, overwrite=T,recursive=F)  
  
}
  
  

##This section creates the weekly anomaly plots by depth and uses the anomaly file from BBMP_TS_2000_####.csv
##\\dcnsbiona01a\BIODataSvcIn\_BIOWeb\BBMP

#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("oce")
#install.packages("ocedata")
#install.packages("grid")
#install.packages("gridExtra")
#install.packages("Rmisc")


library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(Rmisc)

setwd(paste("//dcnsbiona01a/BIODataSvcSrc/BBMP/COMPASS/",(as.numeric(format(Sys.Date(), "%Y"))),sep=""))
list=list.files(pattern="*^.*D.*.ODF$")
l=length(list)

od<-read.odf(paste(getwd(),"/",list[1],sep=""))
dates<-od@metadata$date
od<-as.data.frame(od@data)
datain<-subset(od,od$pressure==2|od$pressure==5|od$pressure==10|od$pressure==60)
test<-as.Date(substr(dates,1,10))
datain$date<-as.Date(substr(dates,1,10))
datain<-dplyr::select(datain,date,pressure,temperature,salinity,sigmaTheta)
dataout<-tidyr::gather(datain,parameter,value,3:5)

for (n in 2:l){
  od<-read.odf(paste(getwd(),"/",list[n],sep=""))
  dates<-od@metadata$date
  od<-as.data.frame(od@data)
  datain<-subset(od,od$pressure==2|od$pressure==5|od$pressure==10|od$pressure==60)
  test<-as.Date(substr(dates,1,10))
  datain$date<-as.Date(substr(dates,1,10))
  datain<-dplyr::select(datain,date,pressure,temperature,salinity,sigmaTheta)
  datagather<-tidyr::gather(datain,parameter,value,3:5)
  dataout<-rbind(dataout,datagather)
}

dataout$woy<-as.numeric(strftime(as.POSIXlt(dataout$date),format="%W"))
datacurr<-dataout

setwd("//Svnsbiofs02/MARSHARED/Shared/Cogswell/_BIOWeb/BBMP")
datasum<-read.csv("BBMP_TS_2000_2015.csv")
datasum$woy<-as.numeric(strftime(as.POSIXlt(datasum$date),format="%W"))

para<-c("temperature","salinity","sigmaTheta")
cap<-c("Temperature (Celsius)","Salinity (PSU)","Sigma-Theta (kg/m^3)")
ys=2000
Year=as.numeric(format(Sys.Date(), "%Y"))
ye=Year-1
rangelab<-paste(ys,"_",ye)
Year<-as.character(Year)

#parameters for x axis tick marks.
x=4.4167
y=x/2

#Sets the min, max and int used in describing the y axis for each variable depth combination in the loop
gp<-NULL
gp$depth<-c(2,2,2,5,5,5,10,10,10,60,60,60)
gp$var<-c("temperature","salinity","sigmaTheta","temperature","salinity","sigmaTheta","temperature","salinity","sigmaTheta","temperature","salinity","sigmaTheta")
gp$min<-c(0,26.5,20.5,0,28,21.5,0,29,21.5,0,30.5,24.25)
gp$max<-c(20,31,25,18,31,25,18,31.5,25,7,31.75,25.25)
gp$int<-c(2,0.5,0.5,2,0.5,0.5,2,0.5,0.5,0.5,0.25,0.125)
gp<-as.data.frame(gp)
gp$var<-as.character(gp$var)


setwd("//Svnsbiofs02/MARSHARED/Shared/Cogswell/_BIOWeb/BBMP")

for (d in c(2,5,10,60)){
  
  data_sub<-subset(datasum,datasum$pressure==d)
  data_curr_sub<-subset(datacurr,datacurr$pressure==d)
  plots<-list()
  
  ##### Loop for 3 panel plot at the depth specified #####      
  
  for (i in 1:3){
    
    # set x-axis
    x.limits <- c(1,52)
    x.breaks <- seq(2.166667, 52, by=4.333333)
    x.labels <- c("J","F","M","A","M","J","J","A","S","O","N","D")
    
    # set y-axis
    gpsub<-subset(gp,gp$depth==d & gp$var==para[i])
    y.limits <- c(gpsub$min, gpsub$max)
    y.breaks <- seq(gpsub$min, gpsub$max, by=gpsub$int)
    y.labels <- y.breaks
    
    #generates title for middle panel of 3 panel plot
    t<-NULL
    t<-ifelse(i==2,paste(data_curr_sub2[nrow(data_curr_sub2),1]," - ",d,"m",sep=""),"")
    
    ## initialize plot
    p <- ggplot() +
      ggtitle(t)+
      theme(plot.title=element_text(hjust=100))+
      coord_cartesian(xlim=x.limits, ylim=y.limits) +
      scale_x_continuous(name="Month", breaks=x.breaks, labels=x.labels) +
      scale_y_continuous(name=cap[i], breaks=y.breaks, labels=y.labels) +
      scale_linetype_manual(values=c(2,1), guide="legend") +
      scale_shape_manual(values=16, guide="legend") +
      scale_fill_manual(values=c("grey 60","grey20"), guide="legend")
    
    data_subtest<-subset(data_sub,data_sub$parameter==para[i]) #subsets the variable specified by the loop from 2000 - 2015.
    data_curr_sub2<-subset(data_curr_sub,data_curr_sub$parameter==para[i]) #subsets the variable specified by the loop from current year.
    
    #summarize data to create maxes and mins for +/- 1 std and 95% CI
    anomoly<-data_subtest%>%
      dplyr::group_by(woy)%>%
      dplyr::summarise(value2=mean(value,na.rm=T),
                       std=sd(value,na.rm=T),
                       tcount=n())
    anomoly = anomoly[-1,]
    error <- qt(0.975,df=anomoly$tcount-1)*anomoly$std/sqrt(anomoly$tcount)
    anomoly$tcount<-NULL
    names(anomoly)<-c("woy","value","std")
    anomoly<-as.data.frame(anomoly)
    
    anomoly <- rbind(anomoly %>%
                       dplyr::mutate(., lower=value-error, upper=value+error, label1="95% C.I."),
                     anomoly %>%
                       dplyr::mutate(., lower=value-std, upper=value+std, label1="1 S.D."))%>%
      mutate(., label2="Mean (2000-2015)", label3="")
    anomoly$std<-NULL
    anomoly$label4<-NULL
    
    data_curr_sub3<-data_curr_sub2[,c(5,4)]
    data_curr_sub3$lower<-data_curr_sub3$value
    data_curr_sub3$upper<-data_curr_sub3$value
    data_curr_sub3$label1<- Year
    data_curr_sub3$label2<- Year
    data_curr_sub3$label3<- Year
    
    testall<-rbind(data_curr_sub3,anomoly)
    
    #plot ribbons for anomoly
    p <- p + 
      layer(
        data=anomoly,
        mapping=aes(x=woy, ymin=lower, ymax=upper, fill=label1),
        stat="identity",
        geom="ribbon",
        params=list(alpha=0.3),
        position=position_identity()
      )  
    
    #plot line for anomoly
    p <- p + 
      layer(
        data=anomoly,
        mapping=aes(x=woy, y=value, linetype=label2),
        stat="identity",
        geom="line",
        params=list(colour="black"),
        position=position_identity()
      ) 
    
    #plot line for current year
    p <- p + 
      layer(
        data=data_curr_sub3,
        mapping=aes(x=woy, y=value, linetype=label2),
        stat="identity",
        geom="line",
        params=list(colour="grey40"),
        position=position_identity()
      ) 
    
    #plot points for current year
    p <-p + 
      layer(
        data=data_curr_sub3,
        mapping=aes(x=woy, y=value, shape=label3),
        stat="identity",
        geom="point",
        params=list(size=5),
        position=position_identity()
      )
    
    p <- p +
      theme_bw() +
      theme(
        plot.title = element_text(face="bold",size=30,hjust=0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey97"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        # x-axis
        axis.text.x=element_text(colour="black", angle=0, hjust=0.5, vjust=0.5,size=20),
        axis.title.x = element_text(face="bold", size=30,margin=margin(20,0,0,0)),
        # y-axis
        axis.text.y=element_text(colour="black", angle=0, hjust=0.5, vjust=0.5,size=20),
        axis.title.y = element_text(face="bold", size=30,margin=margin(0,20,0,0)),
        # legend
        legend.title=element_blank(),
        legend.position="bottom",
        legend.direction="horizontal",
        legend.box="horizontal",
        legend.text=element_text(size=20),
        legend.key = element_blank()
      )
    
    ## customize legend
    p <- p +
      guides(linetype=guide_legend(label.position="right",
                                   label.hjust=5,
                                   label.vjust=0.5,
                                   keywidth=3,
                                   keyheight=1,
                                   reverse=FALSE,
                                   order=2))
    p<- p +
      guides(shape=guide_legend(label.position="right",
                                label.hjust=0.5,
                                label.vjust=0.5,
                                keywidth=.5,
                                keyheight=1,
                                reverse=FALSE,
                                order=1))
    
    p <- p +
      guides(fill=guide_legend(label.position="right",
                               label.hjust=5,
                               label.vjust=0.5,
                               keywidth=2,
                               keyheight=1,
                               reverse=FALSE,
                               order=3))
    plots[[i]]<-p
    
  }
  
  
  #eventually the output directory will have to be BIO_web under SvcIn
  png(paste("Clim_plus_curr_cond","_",d,"m",'.png',sep=""),height=800,width=2400)
  multiplot(plotlist = plots, cols = 3)
  dev.off()
  
}




##### Sections run at year end:


##### Weekly anomaly bar chart ---- 

setwd("//Svnsbiofs02/MARSHARED/Shared/Cogswell/_BIOWeb/BBMP")
#install.packages("png")
library(png)
library(grid)
img <- readPNG("C:/Users/cogswella/Documents/AZMP/R Code/MoPS/AnomLegHor.png")
logo <- rasterGrob(img,x=0.51,y=0.06,width=0.25)


para2<-c("temperature","salinity","sigmaTheta")
cap2<-c("Temperature","Salinity","Sigma-Theta")

for (d in c(2,5,10,60)){
  
  data_sub<-subset(datasum,datasum$pressure==d)
  data_curr_sub<-subset(datacurr,datacurr$pressure==d)
  plots2<-list()
  
  for (i in 1:3){
    
    data_sub2<-subset(data_sub, data_sub$parameter==para2[i]) #subset the summary data for the ith parameter and the dth depth
    data_sub3<-select(data_sub2,value:woy) #select only the value and week of year from the subsetted summary data
    data_sub4<- data_sub3 %>% dplyr::arrange(woy) #arrange all of the summary data in order of week of year collected
    data_sub5<-data_sub4 %>% dplyr::group_by(woy) %>% dplyr::summarise(value_mean=mean(value, na.rm=T), value_std=sd(value, na.rm=T)) #summarize the summary data parameter value (mean) by week of year
    data_sub5<-as.data.frame(data_sub5) #convert the selected summary data to a data frame
    data_curr_sub2<-subset(data_curr_sub, data_curr_sub$parameter==para2[i])
    
    data_join<-dplyr::right_join(data_curr_sub2,data_sub5,by="woy")
    data_join$zscore<-(data_join$value-data_join$value_mean)/data_join$value_std
    data_join$zround<-round_any(data_join$zscore,0.5,round)
    data_join$zround[data_join$zround>3]= 5
    data_join$zround[data_join$zround< -3]= -5
    
    t<-NULL
    t<-ifelse(i==2,paste(format(Sys.Date(), "%Y")," Weekly Anomaly"," - ",d,"m",sep=""),"")
    
    cols<-c("#000ee5","#2430e8","#4852ec","#6d75f0","#9197f3","#B6BAf7","#dadcfb","#FFFFFF","#ffdada","#ffb6b6","#ff9191","#ff6d6d","#ff4848","#ff2424","#FF0000")
    xbreaks<-seq(0,52)
    xlables<-xbreaks
    xlables[seq(0, 52, 2)] <- ""
    
    p <- ggplot(data_join,aes(x=factor(woy),y=zscore))+
      geom_bar(stat="identity", colour="black", aes(fill=factor(zround))) +
      annotate("rect",xmin=11,xmax=25,ymin=-Inf,ymax=Inf,alpha=0.1)+
      annotate("rect",xmin=38,xmax=51,ymin=-Inf,ymax=Inf,alpha=0.1)+
      ggtitle(t)+
      labs(x="Week of Year", y=cap2[i])+
      #coord_cartesian(ylim=c(-5,5))+
      scale_y_continuous(breaks=seq(-4,4,1))+
      expand_limits(y=c(-4,4))+
      scale_x_discrete(breaks=xbreaks,labels=xlables)+
      #expand_limits(x=c(1,52))+
      scale_fill_manual(name="",limits=c(5,3,2.5,2,1.5,1,0.5,0,-0.5,-1,-1.5,-2,-2.5,-3,-5),
                        values = c("#FF0000","#FF2424","#FF4848","#FF6d6d","#FF9191","#FFB6B6","#FFDADA","#FFFFFF",
                                   "#DADCFB","#B6BAF7","#9197F3","#6D75F0","#4852EC","#2430E8","#000ee5"),
                        labels=c(">3","3","2.5","2","1.5","1","0.5","0","-0.5","-1","-1.5","-2","-2.5","-3","<-3"),guide=F)
    
    
    
    p <- p +
      theme_bw() +
      theme(
        plot.margin=unit(c(1,0.5,4,1),"cm"),
        plot.title = element_text(face="bold",size=25,hjust=0.5),
        panel.grid.major = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey97"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        # x-axis
        axis.text.x=element_text(colour="black", angle=0, hjust=0.5, vjust=0.5,size=14),
        axis.title.x = element_text(face="bold", size=20,margin=margin(10,0,0,0)),
        # y-axis
        axis.text.y=element_text(colour="black", angle=0, hjust=0.5, vjust=0.5,size=14),
        axis.title.y = element_text(face="bold", size=20,margin=margin(0,10,0,0))
        #legend.position="none"
        #legend.direction="vertical",
        #legend.text=element_text(size=10),
        #legend.key = element_blank(),
        
      )+
      annotate("text",x=c(6,18,32,45),y=4, label=c("Winter","Spring", "Summer","Fall"), alpha=0.3, size=6)
    
    
    plots2[[i]]<-p
    
    
  }
  
  
  #eventually the output directory will have to be BIO_web under SvcIn
  png(paste("weekly_anom","_",d,"m",'.png',sep=""),height=800,width=2400)
  multiplot(plotlist = plots2, cols = 3)
  grid.draw(logo)
  dev.off()
  
}

##### Copy Archived ODF files to directory for website - run at end of year ########

#setwd("//dcnsbiona01a/BIODataSvcArc/Archive/ctd/2016")
#out="//dcnsbiona01a/BIODataSvcIn/_BIOWeb/BBMP/ODF/2016"

#list2=list.files(pattern="*CTD_BCD2016667.*DN.*.ODF$")

#l=length(list2)
#for (n in 1:l) {
#  file.copy(from=list2[n],to=out, overwrite=T,recursive=F)
#}
