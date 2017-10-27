setwd( "C:\\Users\\CogswellA\\Documents\\AZMP\\R Code\\Test")

if (!require('oce')) devtools::install_github('dankelley/oce', force=T)
library(oce)

ftp_name <- "HL2"
site_name<-ftp_name

source("C:\\Users\\CogswellA\\Documents\\AZMP\\R Code\\MoPS\\HL2_FTP_folder_initialization.R")

# Creates file directory structure
directory_structure(parent_directory = ftp_name, first_year = 1999)

# run if you want to populate the folders in your personal drive from scratch using ODF archive data
# won't work for current year

#source("C:\\Users\\CogswellA\\Documents\\AZMP\\R Code\\MoPS\\ODF_Select_Query_HL_02_1999.R")
source("C:\\Users\\CogswellA\\Documents\\AZMP\\R Code\\MoPS\\HL2_odf_plot_functions.R")

# read in metadata from HL2 occupations from 1999 to 2016
fwd<-("C:\\Users\\CogswellA\\Documents\\HL2\\") #establisheds the working directory for the meta-data summary export
setwd(fwd)
mdata_all_years<-read.csv("HL02_1999_2016_ODFSummary_20171018.csv")
#mdata_all_years$starttime<-ymd_hms(mdata_all_years$starttime, tz="America/Halifax")
mdata_all_years<-dplyr::arrange(mdata_all_years,starttime)
mdata_all_years$year<-substr(mdata_all_years$starttime,1,4)


mdatasub<-subset(mdata_all_years,mdata_all_years$year==2011)

#populate the folders with plot profile images
for(i in 1999:2017){
  #Completes all profile image archives.
  odf_files<-subset(mdata_all_years,mdata_all_years$year==i)
  odf_files<-odf_files$filename
  odf_files<-as.character(odf_files)
  no_odf_files <- length(odf_files)
  
  #From the index above, plots all ODFs within a year.
  lapply(1:no_odf_files, FUN = odf_plot_function, year = i, odf_file_list = odf_files, out_root = "C:\\Users\\CogswellA\\Documents\\HL2\\Profile_Image_Archives\\", site_name = site_name)
  

}

#use this to empty folder and start over if necessary.

for(i in 1999:2017){
unlink(paste( "C:\\Users\\CogswellA\\Documents\\HL2\\Profile_Image_Archives\\",i,"\\*",sep=""))

}


