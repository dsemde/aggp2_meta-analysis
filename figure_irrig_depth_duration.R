rm(list=ls())

library(car)
library(tidyverse)


sum_data <- read.csv("datasets/irrig_depth_duration.csv")

datasettemp <- split(sum_data, sum_data$TopGroup=="Drip")
drip <- datasettemp$'TRUE'
datasettemp <- split(sum_data, sum_data$TopGroup=="Sprinkler")
sprinkler <- datasettemp$'TRUE'
datasettemp <- split(sum_data, sum_data$TopGroup=="Flood/Furrow")
floodfurr <- datasettemp$'TRUE'

# Set TIFF output parameters
tiff("OC Stock Irrigation Depth Duration.tiff", width = 9, height = 4, units = 'in', res = 300)

par( mar=c(2, 0.2, 0.2, 0.2)
     , mai=c(0.15, 0.1, 0.1, 0.1)  # by inches, inner margin
     , omi = c(0.5, 1.75, 0.1, 0.1)  # by inches, outer margin 
     , mgp = c(0, 0.3, 0) # set distance of axis
     , tcl = 0.4
     # , cex.axis = 1.0
     , mfrow=c(1,3))



#****************************************************************************
# PLOT I - Drip
#****************************************************************************

x_min <- min(drip$Low_Perc, na.rm = T)
x_max <- max(drip$High_Perc, na.rm = T)

drip$col <- as.character(drip$col)

plot(drip$ID ~ drip$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-35, 90)
     , xlab = '', ylab='', main=''
     , col = drip$col
     , pch = 20
     , cex = ifelse (drip$TopGroup == "All data"|drip$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
)

arrows(drip$Low_Perc, drip$ID, drip$High_Perc, drip$ID
       ,code=3,length=0.05,angle=90,drip$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-20, 85, 10), labels = seq(-20, 85, 10)
      , cex=1
      , las = 1)

axis (side = 2, at = drip$ID, labels = paste(drip$SubGroup, sep = "")
      , cex=1
      , las = 2)

text(ifelse(drip$High_Perc > 50, drip$Low_Perc-10, drip$High_Perc+17), drip$ID
     , paste(drip$obs,"/",drip$n_study, sep = ""), cex=0.85, adj=0)

# Separation lines
abline(h=c(6,12), col="black", lty=3, lwd=2)

mtext("Drip Irrigation", side=3, cex=0.75)

mtext(side = 1, text = paste("Soil carbon stocks (% change)", sep=" "), line = 1, cex=0.75, outer = T, adj = 0.5)


#****************************************************************************
# PLOT II - Sprinkler
#****************************************************************************

x_min <- min(sprinkler$Low_Perc, na.rm = T)
x_max <- max(sprinkler$High_Perc, na.rm = T)

sprinkler$col <- as.character(sprinkler$col)

plot(sprinkler$ID ~ sprinkler$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-35, 90)
     , xlab = '', ylab='', main=''
     , col = sprinkler$col
     , pch = 20
     , cex = ifelse (sprinkler$TopGroup == "All data"|sprinkler$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
)

arrows(sprinkler$Low_Perc, sprinkler$ID, sprinkler$High_Perc, sprinkler$ID
       ,code=3,length=0.05,angle=90,sprinkler$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-20, 85, 10), labels = seq(-20, 85, 10)
      , cex=1
      , las = 1)

text(ifelse(sprinkler$High_Perc > 70, sprinkler$Low_Perc-15, sprinkler$High_Perc+10), sprinkler$ID
     , paste(sprinkler$obs,"/",sprinkler$n_study, sep = ""), cex=0.85, adj=0)

# Separation lines
abline(h=c(6,12), col="black", lty=3, lwd=2)

mtext("Sprinkler Irrigation", side=3, cex=0.75)


#****************************************************************************
# PLOT III - Flood/Furrow
#****************************************************************************

x_min <- min(floodfurr$Low_Perc, na.rm = T)
x_max <- max(floodfurr$High_Perc, na.rm = T)

floodfurr$col <- as.character(floodfurr$col)

plot(floodfurr$ID ~ floodfurr$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-35, 90)
     , xlab = '', ylab='', main=''
     , col = floodfurr$col
     , pch = 20
     , cex = ifelse (floodfurr$TopGroup == "All data"|floodfurr$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
)

arrows(floodfurr$Low_Perc, floodfurr$ID, floodfurr$High_Perc, floodfurr$ID
       ,code=3,length=0.05,angle=90,floodfurr$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-20, 85, 10), labels = seq(-20, 85, 10)
      , cex=1
      , las = 1)

text(ifelse(floodfurr$High_Perc > 50, floodfurr$Low_Perc-10, floodfurr$High_Perc+7), floodfurr$ID
     , paste(floodfurr$obs,"/",floodfurr$n_study, sep = ""), cex=0.85, adj=0)

# Separation lines
abline(h=c(6,12), col="black", lty=3, lwd=2)

mtext("Flood/Furrow Irrigation", side=3, cex=0.75)

dev.off()