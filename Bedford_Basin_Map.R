## BBMP Map

library(maps)
library(mapdata)
library(bitops)
library(RCurl)
library(png)
library(RJSONIO)
library(RgoogleMaps)
library(TeachingDemos)
library(dplyr)
library(mapproj)

dev.off()

# png("BBMP_Map.png", width=170, height=150, units="mm", res=2800)
map('worldHires', c('Canada', 'USA'), 
    xlim = c(-67, -62), 
    ylim = c(43, 47), 
    col = "darkseagreen", 
    fill = TRUE,
    border = "darkseagreen", lwd = 1)

par(mgp = c(3,1,0))
text(-64.5, 43.1, 
     expression(italic(' Atlantic Ocean\nOc\u{E9}an Atlantique')), 
     col = "darkslategrey", 
     cex = 1.2)

rect(xleft = -63.75, 
     ybottom = 44.5, 
     xright = -63.4, 
     ytop = 44.8, lwd = 2, border = "gray40")

# axis(1);axis(2)
box(col = "gray30")

par(plt = c(0.67, 0.97, 0.12, 0.4), new = TRUE)
plot.window(xlim=c(130,180),ylim=c(40,70))#necessary to set the place to put the blank polygon
polygon(c(0, 360, 360, 0), c(0, 0, 90, 90), col = "white")
plot.window(xlim = c(-110, -50), 
            ylim = c(40, 70))#NECESSARY OR ELSE THE INSET DOESNT SHOW
##map to inset
map(xlim = c(-110, -50), ylim = c(40, 70), interior = FALSE,
    add = TRUE, fill = TRUE, col = "darkseagreen2", border = "gray35", lwd = 0.1)
polygon(x = c(-67, -62, -62, -67, -67),
        y = c(43, 43, 47, 47, 43), lwd = 2, border = "gray30")
box(col = "gray30")

dev.off()

