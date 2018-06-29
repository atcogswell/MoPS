## Script for taking 1996-2009 CTD files and plotting the results

library(oce)
library(ocedata)
library(magrittr)
library(testthat)

# wrapper for doing a whole year of plots
odf_year_plots <- function(year, 
                           out_root = "R:/Shared/Cogswell/_BIOWeb/BBMP/Profile_Image_Archive/",
                           site_code = "667", site_name = "BBMP"){
  # Within a certain year directory, determines all the ODF Files there.
  
  # year <- 2010
  # site_code <- "667"
  
  odf_files <- directory_lister_wrapper(year = year, site_code = site_code)
  no_odf_files <- length(odf_files)
  #From the index above, plots all ODFs within a year.
  lapply(1:no_odf_files, FUN = odf_plot_function, year = year, odf_file_list = odf_files, out_root = out_root, site_name = site_name)
}

# function for doing the last plot in the four-panels of plots. 
fluorescence_oxygen_plot <- function(od_i = od, ctd_i = ctd){
  
  # od_i <- read.odf("C:/Users/mccains/Documents/Data Testing/CTD_BCD2016667_001_01_DN.ODF")
  # ctd_i <- read.ctd.odf("C:/Users/mccains/Documents/Data Testing/CTD_BCD2016667_001_01_DN.ODF")
  
  fluorescence <- ctd_i[["fluorometer"]] %>% is.numeric()
  max_oxygen <- ctd_i[["oxygen"]] %>% max(na.rm = TRUE) %>% round(0) > 0
  
  if(fluorescence & max_oxygen == TRUE){
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
    
  } else if(fluorescence & !max_oxygen == TRUE){
    
    plot(ctd_i[["fluorometer"]], ctd_i[["pressure"]],
         ylim = rev(range(ctd_i[["pressure"]], na.rm = TRUE)),
         type = "l",
         col = "green",
         xlab = "Fluorescence [mg/m^3]", ylab = "",
         col.lab = "green")
    axis(1, labels = T, col.ticks = "green", col.axis = 'green')
    
    mtext("Oxygen was not collected.", side = 1)
  } else if(!fluorescence & max_oxygen){
    
    plot(ctd_i[["oxygen"]], ctd_i[["pressure"]],
         ylim = rev(range(ctd_i[["pressure"]], na.rm = TRUE)),
         type = "l", col = "black", axes = FALSE, xlab = "",
         ylab="Pressure [dbar]")
    box()
    axis(2)
    title("Oxygen [ml/l]", line = 2, font.main = 1, cex.main = 1)
    Axis(side=3, x=ctd_i[["oxygen"]], at = seq(from = 1, to = 10, by = 0.5))
    
    mtext("Fluorescence was not collected.", side = 1)
  } else if(!fluorescence & !max_oxygen == TRUE){
    
    plot.new()
    mtext("Fluorescence and oxygen were not collected.", side = 1)
  }
  rm(ctd_i)
}

# four plot function
odf_plot_function <- function(odf_file, year, odf_file_list = odf_files, out_root, 
                              testing_plots = FALSE, recent_plot = FALSE, site_name = "BBMP"){
  
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
    png(paste(out_dir, site_name, substr(od[["date"]], 1, 10),'.png',sep=""),
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
  plot(ctd, which = 1, keepNA = T)
  plot(ctd, which = 2)
  plot(ctd, which = 3)
  par(mar = c(3.6, 3.4, 3.5, 2))
  
  fluorescence_oxygen_plot(od_i = od, ctd_i = ctd)
  
  title(paste("Compass Buoy Station CTD Profile",  od[["date"]], sep=": "), 
        outer = TRUE, 
        cex = 1.4)# title for overall plot (filename, here)
  
  dev.off()
}

# test
# odf_year_plots(2017)
