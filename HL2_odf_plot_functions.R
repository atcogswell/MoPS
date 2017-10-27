## Script for taking 1996-2009 CTD files and plotting the results

if (!require('oce')) devtools::install_github('dankelley/oce')

library(oce)
library(ocedata)
library(magrittr)
library(testthat)

# wrapper for doing a whole year of plots

# function for doing the last plot in the four-panels of plots. 
fluorescence_oxygen_plot <- function(od_i = od, ctd_i = ctd){
  
  # od_i <- read.odf("R:\\Science\\BIODataSvc\\ARC\\Archive\\ctd\\2011\\CTD_HUD2011004_015_01_DN.ODF")
  # ctd_i <- read.ctd.odf("R:\\Science\\BIODataSvc\\ARC\\Archive\\ctd\\2011\\CTD_HUD2011004_015_01_DN.ODF")
  
  fluorescence<-"fluorometer" %in% names(ctd_i@data)
  
  if (fluorescence==T) {
    fluorescence_na <- all(is.na(ctd_i[['fluorometer']]))
  } else {
    fluorescence_na<-"fluorometer" %in% names(ctd_i@data)
  }
  
  if (fluorescence==T) {
    fluorescence_num <- ctd_i[["fluorometer"]] %>% is.numeric()
  }  else {
      fluorescence_num<-"fluorometer" %in% names(ctd_i@data)
  }
  
  if (fluorescence==T) {
    max_fluor <- ctd_i[["fluorometer"]] %>% max(na.rm = TRUE) %>% round(4) > 0
  } else {
    max_fluor <-"fluorometer" %in% names(ctd_i@data)
  }
  
  oxygen<-"oxygen" %in% names(ctd_i@data)
  
  
  if (oxygen==T) {
    oxygen_na <- all(is.na(ctd_i[['oxygen']]))
  } else {
      oxygen_na<-"oxygen" %in% names(ctd_i@data)
      }
  
  if (oxygen==T) {
    max_oxygen <- ctd_i[["oxygen"]] %>% max(na.rm = TRUE) %>% round(0) > 0
  } else {
      max_oxygen <-"oxygen" %in% names(ctd_i@data)
      }
  
   oxygen2<-"oxygen2" %in% names(ctd_i@data)
  
  if (oxygen & oxygen2 & (!max_oxygen|oxygen_na)==T) {ctd_i[['oxygen']]<-ctd_i[['oxygen2']]}
  
   if ("oxygen" %in% names(ctd_i@data)==T) {
     oxygen <- !all(is.na(ctd_i[['oxygen']]))
   } else {
       oxygen<-"oxygen" %in% names(ctd_i@data)
       }
  
  if((fluorescence & !fluorescence_na & fluorescence_num & max_fluor & oxygen & !oxygen_na & max_oxygen==T)){
    plot(ctd_i[["fluorometer"]], ctd_i[["pressure"]],
         ylim = rev(range(ctd_i[["pressure"]])),
         type = "l",
         col = "green",
         xlab = "Fluorescence [mg/m^3]", ylab = "",
         col.lab = "green")
    axis(1, labels = T, col.ticks = "green", col.axis = 'green')
    par(new = TRUE)
    
    # min_oxy = 0
    # max_oxy <- ctd_i[["oxygen"]] %>% max(na.rm = TRUE) %>% round(0)
    plot(x = ctd_i[["oxygen"]], y =  ctd_i[["pressure"]],
         ylim = rev(range(ctd_i[["pressure"]], na.rm = TRUE)),
         type = "l", col = "black", axes = F, xlab = "",
         ylab="Pressure [dbar]")
    
    title("Oxygen [ml/l]", line = 2, font.main = 1, cex.main = 1)
    Axis(side=3, x=ctd_i[["oxygen"]], at = seq(from = 1, to = 10, by = 0.5))
    
  } else if((fluorescence & !fluorescence_na & fluorescence_num & max_fluor & (!oxygen_na|!max_oxygen) == TRUE) | (fluorescence & !fluorescence_na & fluorescence_num & !oxygen == TRUE)){
    
    plot(ctd_i[["fluorometer"]], ctd_i[["pressure"]],
         ylim = rev(range(ctd_i[["pressure"]], na.rm = TRUE)),
         type = "l",
         col = "green",
         xlab = "Fluorescence [mg/m^3]", ylab = "",
         col.lab = "green")
    axis(1, labels = T, col.ticks = "green", col.axis = 'green')
    
    mtext("Oxygen was not collected.", side = 3)
  } else if((!fluorescence & oxygen & max_oxygen & !oxygen_na==T) | ((!fluorescence_na|!fluorescence_num|!max_fluor) & oxygen & max_oxygen & !oxygen_na==T)){
    
    plot(ctd_i[["oxygen"]], ctd_i[["pressure"]],
         ylim = rev(range(ctd_i[["pressure"]], na.rm = TRUE)),
         type = "l", col = "black", axes = FALSE, xlab = "",
         ylab="Pressure [dbar]")
    box()
    axis(2)
    title("Oxygen [ml/l]", line = 2, font.main = 1, cex.main = 1)
    Axis(side=3, x=ctd_i[["oxygen"]], at = seq(from = 1, to = 10, by = 0.5))
    
    mtext("Fluorescence was not collected.", side = 1)
  } else if((fluorescence & oxygen == F) | ((fluorescence_na|!fluorescence_num|!max_fluor) & !oxygen_na==T)){
    
    plot.new()
    mtext("Fluorescence and oxygen were not collected.", side = 3)
  }
  #rm(ctd_i)
}

# four plot function
odf_plot_function <- function(odf_file, year, odf_file_list = odf_files, out_root, 
                              testing_plots = FALSE, recent_plot = FALSE, site_name){
  
  # out_root <- "R:/Shared/Cogswell/_BIOWeb/BBMP/"
  # 
  # setwd("R:/Science/BIODataSvc/SRC/BBMP/COMPASS/2017")
  
  # odf_file <- 20
  # odf_file_list <- odf_file_list_current_year
  
  print(odf_file_list[odf_file])
  od <- read.odf(odf_file_list[odf_file])
  ctd <- read.ctd.odf(odf_file_list[odf_file])
  out_dir <- paste(out_root,
                   year, "/",
                   sep="")
  if(testing_plots){
    out_dir <- c("C:/Users/McCainS/Documents/Test plots/")
  }
  # setwd(out_dir)
  if(!recent_plot){
    png(paste(out_dir, site_name, substr(od[["date"]], 1, 10),"_",substr(od[["date"]], 12, 13),"_",substr(od[["date"]], 15, 16),'.png',sep=""),
        height = 800,
        width = 800)
  } else {
    png(paste(out_root, "Recent_Profile.png", sep = ""),
        height = 800,
        width = 800)
  }
  plot.new()
  par(oma = c(0,0,2,0))
  par(mfrow = c(2, 2)) # four panels, filled in reading order
  
  conductivity<-"conductivity" %in% names(ctd@data)
  conductivity2<-"conductivity2" %in% names(ctd@data)
  conductivity_na<-all(is.na(ctd[['salinity']]))
  if (conductivity == T) {conductivity_na<-all(is.na(ctd[['conductivity']]))}
  if (conductivity_na == TRUE & conductivity == T & conductivity2 == T) {ctd[['conductivity']]<-ctd[['conductivity2']]}
    
  salinity<-"salinity" %in% names(ctd@data)
  salinity2<-"salinity2" %in% names(ctd@data)
  salinity_na<-all(is.na(ctd[['salinity']]))
  
        if (salinity_na == TRUE & salinity == T & salinity2 == T) {ctd[['salinity']]<-ctd[['salinity2']]}
  
  plot(as.ctd(ctd), which = 1, keepNA = T)
  plot(as.ctd(ctd), which = 2, keepNA = T)
  plot(as.ctd(ctd), which = 3)
  par(mar = c(3.6, 3.4, 3.5, 2))
  
  fluorescence_oxygen_plot(od_i = od, ctd_i = ctd)
  
  title(paste("HL2 Station CTD Profile: ", ctd[["date"]], " (", ctd[['cruiseNumber']], ")", sep=""), 
        outer = TRUE, 
        cex = 1.4)# title for overall plot (filename, here)
  
  dev.off()
}

# odf_year_plots(2017)
