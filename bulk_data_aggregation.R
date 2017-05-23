#Plotting the weekly CTD Data

#test

library(ggplot2)
library(magrittr)
library(testthat)
library(animate)
library(gganimate)
library(dplyr)
library(oce)
library(ocedata)
library(Hmisc)
library(RColorBrewer)

total_df <- data.frame(pressure = numeric(),
                       temperature = numeric(),
                       conductivity = numeric(),
                       oxygenCurrent = numeric(),
                       oxygenTemperature = numeric(),
                       unknown = numeric(),
                       fluorometer = numeric(),
                       par = numeric(),
                       salinity = numeric(),
                       oxygen = numeric(),
                       sigmaTheta = numeric(),
                       flagArchaic = numeric(),
                       time_string = as.POSIXct(character()),
                       year_time = character(), 
                       month_time = character(),
                       julian_day = numeric())

year_available <- c(1999:2016)

for(j in 1:length(year_available)){
  
    year <- year_available[j]
    # year <- 2003
    
    odf_files <- directory_lister_wrapper(year)
    no_odf_files <- length(odf_files)
    
  for(i in 1:no_odf_files){
      print(i)
      opened_ctd_odf <- read.ctd.odf(odf_files[i])
      odf_df <- as.data.frame(opened_ctd_odf@data)
      
      time_string <- rep(opened_ctd_odf[["startTime"]], nrow(odf_df))
      year_time <- rep(format(opened_ctd_odf[["startTime"]], "%Y") %>% as.numeric(), nrow(odf_df))
      month_time <- rep(format(opened_ctd_odf[["startTime"]], "%m") %>% as.numeric(), nrow(odf_df))
      day_time <- rep(format(opened_ctd_odf[["startTime"]], "%d") %>% as.numeric(), nrow(odf_df))
      julian_day <- rep(yday(opened_ctd_odf[["startTime"]]), nrow(odf_df))

      odf_df1 <- data.frame(time_string, year_time, month_time, day_time, julian_day, odf_df)

      total_df <- bind_rows(odf_df1, total_df)
  }
}


total_df["time"] <- NULL

total_df2 <- total_df[order(total_df$time_string),]
