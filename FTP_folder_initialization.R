# function for creating file folder system in a new FTP

library(magrittr)
library(testthat)

# Creates an appropriate directory structure, with ODF, CSV and Profile_Image_Archives as subdirectories. Each has another layer of folders with years. 
directory_structure <- function(parent_directory, first_year = 1999, latest_year, sub_directories){
  
  if(missing(latest_year)){
      latest_year <- format(Sys.Date(), "%Y") %>% as.numeric()
  }
  if(missing(sub_directories)){
      sub_directories <- c("/Profile_Image_Archives/", "/CSV/", "/ODF/")
  } else if(!missing(sub_directories)){
      warning("Creating different directory structure. Ensure that each folder has a '/' before and after name.")
  }
  
  root_dir_created <- paste("~/", parent_directory, "/", sep = "")
  
  expect_is(object = parent_directory, class = "character")
  expect_is(object = first_year, class = "numeric")
  expect_is(object = latest_year, class = "numeric")
  expect_false(dir.exists(root_dir_created), 
               "Parent directory already exists, use a different directory name.")
  
  dir.create(root_dir_created)
  for(sub_directory in 1:length(sub_directories)){
    
    dir.create(path = paste(root_dir_created, sub_directories[sub_directory], sep = ""))
    
    for(year_i in first_year:latest_year){
      dir.create(path = paste("~/", 
                              parent_directory,
                              sub_directories[sub_directory], 
                              year_i, sep = ""))
    }
  }
}
  
# directory_structure(parent_directory = "testing_dir")
