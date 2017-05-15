#create directories for all the old ODF Files

library(magrittr)
library(dplyr)
library(testthat)
library(stringr)


setwd("R:\\Science\\BIODataSvc\\ARC\\Archive\\ctd\\2000")

#function to rename the files to a standard format.
#deprecated since transferring to reading files from Arc instead of Src.
# odf_file_renamer <- function(odf_file_i, file_extension = "ODF"){
#   
#   # odf_file_i <- "CTD_BCD2000667_75_1_DN.ODF"
#   #ensuring that the function argument is a character object (the file name)
#   expect_that(odf_file_i, is_a("character"))
#   
#   '%!in%' <- function(x,y)!('%in%'(x,y))
#   if(file_extension %!in% c("ODF", "csv")){
#     stop("File extension is not available.")
#   }
# 
#   #read in the file as an odf to extract the year
#   odf_read_in <- read.odf(odf_file_i)
#   year_date <- format(odf_read_in[["date"]], "%Y")
#   front_string <- "CTD_BCD"
#   #get the number associated with the file (e.g. 001)
#   cast_number <- substr(odf_file_i, nchar(odf_file_i) - 6, nchar(odf_file_i) - 4)
#   #put the total file name together
#   final_file_name <- paste(front_string, 
#                            year_date, 
#                            "667_",
#                            cast_number,
#                            "_01_DN.",
#                            file_extension,
#                            sep = "")
#   return(final_file_name)
# }

#function for transferring and renaming files from the COMPASS shared files, to the BBMP website folder
transfer_files_odf <- function(year){
  #### 
  #This function copies files from the Arc, to the BBMP website FTP. 
  #Input is just one year.
  ###
  # year <- 
  temp_wd <- paste("R:\\Science\\BIODataSvc\\ARC\\Archive\\ctd\\", year, sep = "")
  setwd(temp_wd)
  odf_files <- list.files(pattern="*^.*D.*.ODF$")
  if(length(odf_files) == 0){
    odf_files <- list.files(pattern = ".ODF$")
  }
  #there is one weird file in 2002, this line takes that file out
  # odf_files <- odf_files[odf_files != "02667011.ODF"]
  #Only files that have 667 in the subject line (666 not accepted)
  only_667 <- grepl(pattern = "667_", x = odf_files)
  only_DN <- grepl(pattern = "_DN", x = odf_files)
  if(year > 1999){
    only_bcd <- grepl(pattern = "BCD", x = odf_files)
    odf_files <- odf_files[only_667 & only_bcd & only_DN]
  } else if (year == 1999){
    only_99667 <- grepl(pattern = "99667", x = odf_files)
    odf_files <- odf_files[only_99667]
  }
  
  no_odf_files <- length(odf_files)
  for(i in 1:no_odf_files){
    start_file <- paste(temp_wd, "\\", odf_files[i], sep = "")
    
    out_file <- paste("R:\\Shared\\Cogswell\\_BIOWeb\\BBMP\\ODF\\",
          year, "\\",
          odf_files[i],
          sep="")
    
    file.copy(from = start_file, to = out_file)
    
    print(out_file)
    
  }
}

# transfer_files_odf(year = 2008)

substr_right <- function(x, n){
  ###
  #Function for taking the substring to the right of string x, n times
  ###
  substr(x, nchar(x) - n + 1, nchar(x))
}

transfer_files_csv <- function(year){
  #### 
  #This function copies files from the Arc, to the BBMP website FTP. 
  #ODF File is converted to a .csv
  #Input is just one year.
  ### 
  # year <- 
  temp_wd <- paste("R:\\Science\\BIODataSvc\\ARC\\Archive\\ctd\\", year, sep = "")
  setwd(temp_wd)
  odf_files <- list.files(pattern="*^.*D.*.ODF$")
  if(length(odf_files) == 0){
    odf_files <- list.files(pattern = ".ODF$")
  }
  #there is one weird file in 2002, this line takes that file out
  # odf_files <- odf_files[odf_files != "02667011.ODF"]
  #Only files that have 667 in the subject line (666 not accepted)
  only_667 <- grepl(pattern = "667", x = odf_files)
  only_bcd <- grepl(pattern = "BCD", x = odf_files)
  only_dn <- grepl(pattern = "DN", x = odf_files)
  if(year > 1999){
    odf_files <- odf_files[only_667 & only_bcd & only_dn]
  } else if (year == 1999){
    only_99667 <- grepl(pattern = "99667", x = odf_files)
    odf_files <- odf_files[only_99667]
  }
  
  no_odf_files <- length(odf_files)
  out_file_dir <- paste("R:\\Shared\\Cogswell\\_BIOWeb\\BBMP\\CSV\\",
          year, "\\",
          sep="")
  expect_true(no_odf_files > 0, info = "No files found.")
  for(i in 1:no_odf_files){
    start_file <- paste(temp_wd, "\\", odf_files[i], sep = "")
    
    setwd(temp_wd)
    # new_odf_file_name <- odf_file_renamer(odf_file_i = odf_files[i], file_extension = "csv")
    new_odf_file_name <- paste(str_sub(odf_files[i], start = 1, end = -4), "csv", sep = "")
    opened_ctd_odf <- read.ctd.odf(odf_files[i])
    setwd(out_file_dir)
    write.ctd(opened_ctd_odf, file = new_odf_file_name)
    print(new_odf_file_name)
  }
}

# transfer_files_csv(2017)
# transfer_files_odf(1999)



