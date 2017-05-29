# function for creating file folder system in a new FTP

library(magrittr)

current_year <- format(Sys.Date(), "%Y") %>% as.numeric()

for(year_i in first_year:current_year){
  write.dir()
}

