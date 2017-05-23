#New Weekly Plots for BBMP Program

library(ggplot2)
library(dplyr)
library(magrittr)
library(oce)
library(ocedata)

setwd(dir = "C:\\Users\\McCainS\\Documents\\BBMP\\Data")

test_file <- read.odf("CTD_BCD2010667_004_01_DN.ODF")
test_file_df <- as.data.frame(test_file@data)

temp <- test_file_df %>% 
  ggplot(aes(y = pressure, x = temperature, colour = temperature)) + 
  geom_path(size = 2, alpha = 0.8) + 
  scale_y_reverse() + 
  theme_classic() +
  theme(legend.position = "none", 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) + 
  labs(x = "Temperature (°C)", y = "Pressure (dBar)") + 
  geom_hline(yintercept = 0) +
  scale_colour_gradient(low = "blue", high = "red");temp

phyto <- test_file_df %>% 
  ggplot(aes(y = pressure, x = fluorometer, colour = fluorometer)) + 
  geom_path(size = 2, alpha = 0.8) + 
  scale_y_reverse() + 
  theme_classic() +
  theme(legend.position = "none", 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) + 
  labs(x = bquote('Fluorescence (mg' m^-3~')'), y = "Pressure (dBar)") + 
  # bquote('Assimilation ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))
  geom_hline(yintercept = 0) +
  scale_colour_gradient(low = "grey", high = "darkgreen");phyto
  
salt <- test_file_df %>% 
  ggplot(aes(y = pressure, x = salinity, colour = salinity)) + 
  geom_path(size = 2, alpha = 0.8) + 
  scale_y_reverse() + 
  theme_classic() +
  theme(legend.position = "none", 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) + 
  labs(x = "Salinity (practical salinity units)", y = "Pressure (dBar)") + 
  geom_hline(yintercept = 0) +
  scale_colour_gradient(low = "grey", high = "darkblue");salt

oxy <- test_file_df %>% 
  ggplot(aes(y = pressure, x = oxygen, colour = oxygen)) + 
  geom_path(size = 2, alpha = 0.8) + 
  scale_y_reverse() + 
  theme_classic() +
  theme(legend.position = "none", 
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) + 
  labs(x = "Oxygen (mL/L)", y = "Pressure (dBar)") + 
  geom_hline(yintercept = 0) +
  scale_colour_gradient(low = "grey", high = "darkblue");oxy



