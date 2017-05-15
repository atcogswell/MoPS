#Plotting the weekly CTD Data

library(ggplot2)
library(magrittr)
library(animate)
library(gganimate)
library(dplyr)
library(oce)
library(ocedata)
library(RColorBrewer)

setwd("R:\\Science\\BIODataSvc\\SRC\\BBMP\\COMPASS\\2013")

total_df <- data.frame(pressure = numeric(),
                       temperature = numeric(),
                       conductivity = numeric(),
                       # oxygenCurrent = numeric(),
                       # oxygenTemperature = numeric(),
                       # unknown = numeric(),
                       fluorometer = numeric(),
                       par = numeric(),
                       # salinity = numeric(),
                       # oxygen = numeric(),
                       sigmaTheta = numeric(),
                       # flagArchaic = numeric(),
                       start_time = as.POSIXct(character()))

for(year in 2010:2011){
  
  # year <- 2010
  temp_wd <- paste("R:\\Science\\BIODataSvc\\ARC\\Archive\\ctd\\", year, sep = "")
  setwd(temp_wd)
  odf_files <- list.files(pattern="*^.*D.*.ODF$")
  
  #there is one weird file in 2002, this line takes that file out
  odf_files <- odf_files[odf_files != "02667011.ODF"]
  #Only files that have 667 in the subject line (666 not accepted)
  only_667 <- grepl(pattern = "667", x = odf_files)
  only_bcd <- grepl(pattern = "BCD", x = odf_files)
  only_dn <- grepl(pattern = "DN", x = odf_files
  )
  
  odf_files <- odf_files[only_667 & only_bcd & only_dn]
  
  no_odf_files <- length(odf_files)
  
  for(i in 1:no_odf_files){
    print(i)
    opened_ctd_odf <- read.ctd.odf(odf_files[i])
    odf_df <- as.data.frame(opened_ctd_odf@data)
    
    start_time <- rep(opened_ctd_odf[["startTime"]], nrow(odf_df))
    
    odf_df1 <- data.frame(odf_df, start_time)
    odf_df2 <- odf_df1 %>% dplyr::select(pressure, temperature, conductivity, fluorometer, par, sigmaTheta, start_time)
    
    total_df <- rbind(odf_df2, total_df)
  }
}

# colz <- colorRampPalette(c("blue", "red"))(51)
# ani.options(convert = "C:\\Program Files\\ImageMagick-7.0.5-Q16\\convert.exe")

p1 <- total_df %>% 
  filter(start_time > "2010-12-30") %>% 
  ggplot(aes(y = pressure, x = temperature, colour = temperature, 
             frame = start_time %>% as.factor(),
             fill = start_time %>% as.factor())) + 
  geom_path(size = 2, alpha = 0.8) + 
  geom_path(size = 2.5, alpha = 0.6) + 
  geom_path(size = 3, alpha = 0.5) + 
  geom_path(size = 3.5, alpha = 0.4) + 
  geom_path(size = 4, alpha = 0.3) + 
  scale_y_reverse() + 
  theme_classic() +
  theme(legend.position = "none", 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) + 
  labs(x = "Temperature (C)", y = "Depth (m)") + 
  geom_hline(yintercept = 0) +
  # stat_function(fun = function(x)sin(x), colour = "black", size = 1) +
  scale_colour_gradient(low = "blue", high = "red")

gganimate(p1, interval = 0.075)


blah <- total_df %>% 
  filter(start_time > "2010-12-30") %>%
  ggplot(aes(xmin = 1, xmax = 10, ymin= -pressure + 1, ymax = -pressure, 
             fill = temperature, frame = start_time %>% as.factor())) + 
  geom_rect() + 
  geom_area(
    aes(x), data.frame(x = c(1, 10)), 
    inherit.aes = F, 
    stat="function",
    fun = function(x)abs(sin(2*x)) + 0.2, fill = "blue") + 
  coord_cartesian(ylim = c( -max(test_file_df$pressure), 10)) + 
  theme_minimal() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) + 
  labs(x=NULL, y="Meters Below Sealevel") + 
  scale_fill_gradient(low = "dodgerblue4", high = "red")


gganimate(blah, interval = 0.1)
gganimate(blah, interval = 0.05, "bbmp_temp2.gif", saver = "gif")


p2 <- total_df %>% 
  filter(start_time > "2010-12-30") %>% 
  ggplot(aes(y = pressure, x = sigmaTheta, colour = temperature, 
             frame = start_time %>% as.factor(),
             fill = start_time %>% as.factor())) + 
  geom_path(size = 1, alpha = 0.8) + 
  geom_path(size = 1.5, alpha = 0.6) + 
  geom_path(size = 1.7, alpha = 0.2) + 
  scale_y_reverse() + 
  theme_classic() +
  theme(legend.position = "none", 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) + 
  labs(x = "Water Density", y = "Depth (m)") + 
  stat_function(fun = function(x)sin(x), colour = "black", size = 1) +
  scale_colour_gradient(low = "blue", high = "red")

setwd("C:\\Users\\cogswella\\Documents\\Bedford Basin Monitoring Program")

gganimate(p2, interval = 0.05, title_frame = FALSE, "bbmp_temp.gif", saver = "gif")


tw_test_df <- total_df %>% 
  filter(start_time > "2010-12-30")

tw_test <- tween_appear(tw_test_df, time = "start_time", nframes = 100)

p2 <- tw_test %>% 
  ggplot(aes(y = pressure, x = temperature, colour = temperature, 
             frame = start_time %>% as.factor(),
             fill = start_time %>% as.factor())) + 
  geom_path(size = 2, alpha = 0.8) + 
  geom_path(size = 2.5, alpha = 0.6) + 
  geom_path(size = 3, alpha = 0.5) + 
  geom_path(size = 3.5, alpha = 0.4) + 
  geom_path(size = 4, alpha = 0.3) + 
  scale_y_reverse() + 
  theme_classic() +
  theme(legend.position = "none", 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) + 
  labs(x = "Temperature (C)", y = "Depth (m)") + 
  geom_hline(yintercept = 0) +
  # stat_function(fun = function(x)sin(x), colour = "black", size = 1) +
  scale_colour_gradient(low = "blue", high = "red")

gganimate(p2)




