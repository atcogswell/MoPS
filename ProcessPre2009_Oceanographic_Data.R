##Script for taking 1996-2009 CTD files and plotting the results

library(oce)
library(ocedata)
library(magrittr)

odf_year_plots <- function(year){
    # Within a certain year directory, determines all the ODF Files there.
    # year <- 1999
    setwd(paste("R:\\Science\\BIODataSvc\\ARC\\Archive\\ctd\\", year, sep = ""))
    odf_files <- list.files(pattern="*^.*D.*.ODF$")
    if(length(odf_files) == 0){
      odf_files <- list.files(pattern = ".ODF$")
    }
    #there is one weird file in 2002, this line takes that file out
    odf_files <- odf_files[odf_files != "02667011.ODF"]
    only_667 <- grepl(pattern = "667", x = odf_files)
    only_bcd <- grepl(pattern = "BCD", x = odf_files)
    
    bcd_and_667 <- only_667 & only_bcd
    
    odf_files <- odf_files[bcd_and_667]
    # odf_small_files <- list.files(pattern = ".odf$")
    
    # odf_files <- c(odf_files, odf_small_files)
    
    no_odf_files <- length(odf_files)
    
    #From the index above, plots all ODFs within a year.
    lapply(1:no_odf_files, odf_plot_function, year, odf_files)
}

#function for doing the last plot in the four-panels. 
fluorescence_oxygen_plot <- function(od_i = od, ctd_i = ctd){
  
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
        plot(ctd_i[["oxygen"]], ctd_i[["pressure"]],
             ylim = rev(range(ctd_i[["oxygen"]], na.rm = TRUE)),
             type = "l", col = "black", axes = F, xlab = "",
             ylab="Pressure [dbar]")
        
        title("Oxygen [ml/l]", line = 2, font.main = 1, cex.main = 1)
        Axis(side=3, x=ctd_i[["oxygen"]], at = seq(from = 1, to = 10, by = 0.5))
        
      } else if(fluorescence & !max_oxygen == TRUE){
      
        plot(ctd_i[["fluorometer"]], ctd_i[["pressure"]],
             ylim = rev(range(ctd_i[["pressure"]])),
             type = "l",
             col = "green",
             xlab = "Fluorescence [mg/m^3]", ylab = "",
             col.lab = "green")
        axis(1, labels = T, col.ticks = "green", col.axis = 'green')
          
        mtext("Oxygen was not collected.", side = 1)
      } else if(!fluorescence & max_oxygen){
        
        plot(ctd_i[["oxygen"]], ctd_i[["pressure"]],
             ylim = rev(range(ctd_i[["oxygen"]], na.rm = TRUE)),
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

odf_plot_function <- function(odf_file, year, odf_file_list = odf_files){
  # odf_file <- 10
  # odf_file_list <- odf_files
    print(odf_file_list[odf_file])
    od <- read.odf(odf_file_list[odf_file])
    ctd <- read.ctd.odf(odf_file_list[odf_file])
    out_dir <- paste("R:\\Shared\\Cogswell\\_BIOWeb\\BBMP",
                     "\\", year, "\\",
                     sep="")
    setwd(out_dir)
    png(paste(out_dir,"BBMP",substr(od[["date"]], 1, 10),'.png',sep=""),
        height = 800,
        width = 800)
    plot.new()
    par(oma = c(0,0,2,0))
    par(mfrow = c(2, 2)) # four panels, filled in reading order
    plot(ctd, which = 1, keepNA = T)
    plot(ctd, which = 2)
    plot(ctd, which = 3)
    par(mar = c(3.6, 3.4, 3.5, 2))
    
    fluorescence_oxygen_plot(od_i = od, ctd_i = ctd)
    
    print(od[["date"]])

    title(paste("Compass Buoy Station CTD Profile",  od[["date"]], sep=": "), outer = TRUE, cex = 1.4)# title for overall plot (filename, here)
    dev.off()
    setwd(paste("R:\\Science\\BIODataSvc\\ARC\\Archive\\ctd\\", year, sep = ""))
}

# odf_year_plots(year = 2001)


setwd("R:\\Science\\BIODataSvc\\ARC\\Archive\\ctd\\2001")

# tester <- read.odf("CTD_BCD2001667_058_1_DN.ODF")



