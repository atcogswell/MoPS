#Example of populating a new FTP Site

ftp_name <- "HL2_Test3"

#creates file directory structure
directory_structure(parent_directory = ftp_name, first_year = 2010, latest_year = 2011)

#populate the folders
for(i in 2010:2011){
  odf_year_plots(i, out_root = paste("~/", ftp_name, "/Profile_Image_Archives/", sep = ""), site_code = "666")
  transfer_files_csv(year = i, out_root = paste("~/", ftp_name, "/CSV/", sep = ""), site_code = "666")
  transfer_files_odf(year = i, out_root = paste("~/", ftp_name, "/ODF/", sep = ""), site_code = "666")
}
