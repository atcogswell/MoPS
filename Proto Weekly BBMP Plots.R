#Exploring dynamic data visualizations

library(ggplot2)
library(dplyr)
library(magrittr)
library(oce)
library(ocedata)
library(gganimate)
library(animation)

library(tweenr)


setwd(dir = "C:\\Users\\mccains\\Documents\\Data Testing")

# ani.options(convert = "C:/DFO-MPO/ImageMagick-7.0.5-Q16/convert.exe")

master_df <- read.csv("bbmp_aggregated_profiles.csv")
master_df$week_time <- week(master_df$time_string)
master_df2 <- master_df %>% filter(year_time != "2004")
# 
fluor <- master_df2 %>% group_by(pressure, week_time) %>%
            summarise(week_temp = mean(temperature),
                      week_fluor = mean(fluorometer, na.rm = TRUE),
                      week_salt = mean(salinity),
                      week_oxy = mean(oxygen)) %>%
            ggplot(aes(y = -pressure,
                       x = week_fluor,
                       frame = week_time,
                       xmin = 1,
                       xmax = 20,
                       ymin= -pressure + 1,
                       ymax = -pressure, fill = week_fluor)) +
            # scale_y_reverse() +
            theme_bw() +
            geom_hline(yintercept = 0) +
            geom_rect() +
            geom_area(
              aes(x), data.frame(x = c(1, 20)),
              inherit.aes = F,
              stat="function") +
            coord_cartesian(ylim = c(-max(master_df2$pressure), 0),
                            xlim = c(1, 20)) +
            theme_minimal() +
            # theme(axis.text.x = element_blank(),
            #       axis.ticks.x = element_blank()) +
            labs(x = "Fluorescence", y="Meters Below Sealevel") +
            geom_point() +
            scale_fill_gradient(low = "dodgerblue4", high = "green") +
            ggtitle("Week Number ")


temp <- master_df2 %>% group_by(pressure, week_time) %>%
            summarise(week_temp = mean(temperature, na.rm = TRUE),
                      week_fluor = mean(fluorometer, na.rm = TRUE),
                      week_salt = mean(salinity, na.rm = TRUE),
                      week_oxy = mean(oxygen, na.rm = TRUE)) %>%
            ggplot(aes(y = -pressure,
                       x = week_temp,
                       frame = week_time,
                       xmin = 1,
                       xmax = 20,
                       ymin= -pressure + 1,
                       ymax = -pressure, fill = week_temp)) +
            # scale_y_reverse() +
            theme_bw() +
            geom_hline(yintercept = 0) +
            geom_rect() +
            geom_area(
              aes(x), data.frame(x = c(1, 20)),
              inherit.aes = F,
              stat="function") +
            coord_cartesian(ylim = c(-max(master_df2$pressure), 0),
                            xlim = c(1, 20)) +
            theme_minimal() +
            # theme(axis.text.x = element_blank(),
            #       axis.ticks.x = element_blank()) +
            labs(x = "Temperature", y="Meters Below Sealevel") +
            geom_point() +
            scale_fill_gradient(low = "dodgerblue4", high = "red") +
            ggtitle("Week Number ")

oxy <- master_df2 %>% group_by(pressure, week_time) %>%
            summarise(week_temp = mean(temperature, na.rm = TRUE),
                      week_fluor = mean(fluorometer, na.rm = TRUE),
                      week_salt = mean(salinity, na.rm = TRUE),
                      week_oxy = mean(oxygen, na.rm = TRUE)) %>%
            ggplot(aes(y = -pressure,
                       x = week_oxy ,
                       frame = week_time,
                       xmin = 1,
                       xmax = 10,
                       ymin= -pressure + 1,
                       ymax = -pressure, fill = week_oxy)) +
            # scale_y_reverse() +
            theme_bw() +
            geom_hline(yintercept = 0) +
            geom_rect() +
            geom_area(
              aes(x), data.frame(x = c(1, 20)),
              inherit.aes = F,
              stat="function") +
            coord_cartesian(ylim = c(-max(master_df2$pressure), 0),
                            xlim = c(1, 10)) +
            theme_minimal() +
            # theme(axis.text.x = element_blank(),
            #       axis.ticks.x = element_blank()) +
            labs(x = "Oxygen Concentration", y="Meters Below Sealevel") +
            geom_point() +
            scale_fill_gradient(low = "grey68", high = "blue4") +
            ggtitle("Week Number ")

salt <- master_df2 %>% group_by(pressure, week_time) %>%
            summarise(week_temp = mean(temperature, na.rm = TRUE),
                      week_fluor = mean(fluorometer, na.rm = TRUE),
                      week_salt = mean(salinity, na.rm = TRUE),
                      week_oxy = mean(oxygen, na.rm = TRUE)) %>%
            ggplot(aes(y = -pressure,
                       x = week_salt ,
                       frame = week_time,
                       xmin = 27,
                       xmax = 33,
                       ymin= -pressure + 1,
                       ymax = -pressure, fill = week_salt)) +
            # scale_y_reverse() +
            theme_bw() +
            geom_hline(yintercept = 0) +
            geom_rect() +
            geom_area(
              aes(x), data.frame(x = c(1, 20)),
              inherit.aes = F,
              stat="function") +
            coord_cartesian(ylim = c(-max(master_df2$pressure), 0),
                            xlim = c(27, 33)) +
            theme_minimal() +
            # theme(axis.text.x = element_blank(),
            #       axis.ticks.x = element_blank()) +
            labs(x = "Salinity", y="Meters Below Sealevel") +
            geom_point() +
            scale_fill_gradient(low = "blue", high = "white") +
            ggtitle("Week Number ")

gganimate(salt, cmd.fun = shell, interval = 0.1, saver = "gif", "salt.mp4")
gganimate(oxy, cmd.fun = shell, interval = 0.1, saver = "gif", "oxy.mp4")
gganimate(fluor, cmd.fun = shell, interval = 0.1, saver = "gif", "flu.mp4")
gganimate(temp, cmd.fun = shell, interval = 0.1, saver = "gif", "temp.mp4")










