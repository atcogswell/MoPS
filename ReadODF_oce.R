#### This is a very small sample script that will allow you to import, visualize and explore .ODF files.
#### oce packages and libraries will need to be installed before you can begin.
#### Resources are provided below.


#### 1. Install packages and libraries ####
#install.packages("oce") 
#install.packages("ocedata")
library(oce)
library(ocedata)

#### 2. Resources ####
#https://cran.r-project.org/web/packages/oce/oce.pdf
#http://dankelley.github.io/oce/
?oce::`plot,ctd-method`

#### 3. Read file and plot ####
#choose ODF file and read it using read.odf and read.ctd.odf
df<-file.choose()
odf_r<-read.odf(df)
ctd_r<-read.ctd.odf(df)
oce::plot(ctd_r)

#### 4. View meatadata, data and processing logs ####
ctd_r@metadata
ctd_r@data
ctd_r@processingLog
