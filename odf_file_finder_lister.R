#ODF file finders and working directory switchers

###
#odf_file_lister: produces a set of odf files that are matched for certain naming conventions. e.g. searches for only Bedford Basin 667
#directory_lister_wrapper: a wrapper function for the odf_file_lister, to switch to the Src instead of Arc if files aren't present.
###

library(oce)
library(ocedata)
library(magrittr)
library(testthat)

# test_wd <- paste("R:/Science/BIODataSvc/ARC/Archive/ctd/", "2011", sep = "")

#goes to a working directory, and finds all appropriate ODF files (searched by code), and returns a list of file names.
odf_file_lister <- function(working_directory, year, site_code){
  expect_true(dir.exists(working_directory), 
              "File folder does not exist in the FTP.")  
  # setwd(working_directory)
  if(!use_src){
    odf_file_list_i <- list.files(pattern = "*^.*D.*.ODF$")
    if(length(odf_file_list_i) == 0){
      odf_file_list_i <- list.files(pattern = ".ODF$")
    }
    only_667 <- grepl(pattern = paste(site_code, "_", sep = ""), x = odf_file_list_i)
    only_DN <- grepl(pattern = "_DN", x = odf_file_list_i)
    if(year > 1999){
      only_bcd <- grepl(pattern = "BCD", x = odf_file_list_i)
      odf_file_list_i <- odf_file_list_i[only_667 & only_bcd & only_DN]
    } else if (year == 1999){
      only_99667 <- grepl(pattern = paste("99", site_code, sep = ""), x = odf_file_list_i)
      odf_file_list_i <- odf_file_list_i[only_99667]
    }
  } else if(use_src){
    odf_file_list_i <- list.files(pattern="*^.*D.*.ODF$")    
  }
  return(odf_file_list_i)
}

# arc_wd_test <- paste("R:/Science/BIODataSvc/ARC/Archive/ctd/", 2017, sep = "")
# odf_tester_s <- odf_file_lister(arc_wd_test, year_i = 2017)

#getting odfs from Arc (preferrably) or Src.
directory_lister_wrapper <- function(year, site_code, 
                                     arc_root = "R:/Science/BIODataSvc/ARC/Archive/ctd/",
                                     src_root = "R:/Science/BIODataSvc/SRC/BBMP/COMPASS/"){
  
  # src_root <- "R:/Science/BIODataSvc/SRC/BBMP/COMPASS/"
  
  # year <- 1999
  # site_code <- "666"
  # arc_root <- "R:/Science/BIODataSvc/ARC/Archive/ctd/"
  
  arc_wd <- paste(arc_root, year, sep = "")
  src_wd <- paste(src_root, year, sep = "")
  
  if(dir.exists(arc_wd)){
    setwd(arc_wd)
    use_src <<- FALSE
    used_directory <<- arc_wd
    odf_files <- odf_file_lister(working_directory = arc_wd, year =  year, site_code = site_code)
    no_odf_files <- length(odf_files)
  }
  
  #conditional if the ODFs are not in the Arc, the ODFs from the Src are taken.
  #use_src is a global variable used to inform functions later in the pipeline.
  if(!dir.exists(arc_wd)){
    setwd(src_wd)
    use_src <<- TRUE
    used_directory <<- src_wd
    odf_files <- odf_file_lister(working_directory = src_wd, year)
  } 
  return(odf_files)
}






