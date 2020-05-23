rm(list=ls())

library(car)
library(tidyverse)


sum_data <- read.csv("datasets/aridity_depth_duration.csv")

datasettemp <- split(sum_data, sum_data$TopGroup=="Arid")
arid <- datasettemp$'TRUE'
datasettemp <- split(sum_data, sum_data$TopGroup=="Semi-arid")
semi <- datasettemp$'TRUE'
datasettemp <- split(sum_data, sum_data$TopGroup=="Dry sub-humid")
dry <- datasettemp$'TRUE'
datasettemp <- split(sum_data, sum_data$TopGroup=="Humid")
humid <- datasettemp$'TRUE'

# Set TIFF output parameters
tiff("OC Stock Aridity Duration Depth.tiff", width = 12, height = 4, units = 'in', res = 300)

par( mar=c(2, 0.2, 0.2, 0.2)
     , mai=c(0.15, 0.1, 0.1, 0.1)  # by inches, inner margin
     , omi = c(0.5, 1.75, 0.1, 0.1)  # by inches, outer margin 
     , mgp = c(0, 0.3, 0) # set distance of axis
     , tcl = 0.4
     # , cex.axis = 1.0
     , mfrow=c(1,4))



#****************************************************************************
# PLOT I - Arid
#****************************************************************************

x_min <- min(arid$Low_Perc, na.rm = T)
x_max <- max(arid$High_Perc, na.rm = T)

arid$col <- as.character(arid$col)

plot(arid$ID ~ arid$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-45, 90)
     , xlab = '', ylab='', main=''
     , col = arid$col
     , pch = 20
     , cex = ifelse (arid$TopGroup == "All data"|arid$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
)

arrows(arid$Low_Perc, arid$ID, arid$High_Perc, arid$ID
       ,code=3,length=0.05,angle=90,arid$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-40, 90, 10), labels = seq(-40, 90, 10)
      , cex=1
      , las = 1)

axis (side = 2, at = arid$ID, labels = paste(arid$SubGroup, sep = "")
      , cex=1
      , las = 2)

text(ifelse(arid$High_Perc > 85, arid$Low_Perc-7, arid$High_Perc+5), arid$ID
     , paste(arid$n_study), cex=0.85, adj=0)

# Separation lines
abline(h=c(5,10), col="black", lty=3, lwd=2)

mtext("Arid", side=3, cex=0.75)

mtext("Short Duration", side=2, cex=0.75, adj=0.95, outer = T, line = 4)
mtext("Medium Duration", side=2, cex=0.75, outer = T, line = 4)
mtext("Long Duration", side=2, cex=0.75, adj=0.05, outer = T, line = 4)

mtext(side = 1, text = paste("Soil carbon stocks (% change)", sep=" "), line = 1, cex=0.75, outer = T, adj = 0.5)


#****************************************************************************
# PLOT II - Semi-arid
#****************************************************************************

x_min <- min(semi$Low_Perc, na.rm = T)
x_max <- max(semi$High_Perc, na.rm = T)

semi$col <- as.character(semi$col)

plot(semi$ID ~ semi$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-45, 90)
     , xlab = '', ylab='', main=''
     , col = semi$col
     , pch = 20
     , cex = ifelse (semi$TopGroup == "All data"|semi$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
)

arrows(semi$Low_Perc, semi$ID, semi$High_Perc, semi$ID
       ,code=3,length=0.05,angle=90,semi$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-40, 90, 10), labels = seq(-40, 90, 10)
      , cex=1
      , las = 1)

text(ifelse(semi$High_Perc > 85, semi$Low_Perc-7, semi$High_Perc+5), semi$ID
     , paste(semi$n_study), cex=0.85, adj=0)

# Separation lines
abline(h=c(5,10), col="black", lty=3, lwd=2)

mtext("Semi-arid", side=3, cex=0.75)


#****************************************************************************
# PLOT III - Dry sub-humid
#****************************************************************************

x_min <- min(dry$Low_Perc, na.rm = T)
x_max <- max(dry$High_Perc, na.rm = T)

dry$col <- as.character(dry$col)

plot(dry$ID ~ dry$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-45, 90)
     , xlab = '', ylab='', main=''
     , col = dry$col
     , pch = 20
     , cex = ifelse (dry$TopGroup == "All data"|dry$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
)

arrows(dry$Low_Perc, dry$ID, dry$High_Perc, dry$ID
       ,code=3,length=0.05,angle=90,dry$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-40, 90, 10), labels = seq(-40, 90, 10)
      , cex=1
      , las = 1)

text(ifelse(dry$High_Perc > 85, dry$Low_Perc-7, dry$High_Perc+5), dry$ID
     , paste(dry$n_study), cex=0.85, adj=0)

# Separation lines
abline(h=c(5,10), col="black", lty=3, lwd=2)

mtext("Dry sub-humid", side=3, cex=0.75)

#****************************************************************************
# PLOT IV - Humid
#****************************************************************************

x_min <- min(humid$Low_Perc, na.rm = T)
x_max <- max(humid$High_Perc, na.rm = T)

humid$col <- as.character(humid$col)

plot(humid$ID ~ humid$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-45, 90)
     , xlab = '', ylab='', main=''
     , col = humid$col
     , pch = 20
     , cex = ifelse (humid$TopGroup == "All data"|humid$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
)

arrows(humid$Low_Perc, humid$ID, humid$High_Perc, humid$ID
       ,code=3,length=0.05,angle=90,humid$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-40, 90, 10), labels = seq(-40, 90, 10)
      , cex=1
      , las = 1)

text(ifelse(humid$High_Perc > 85, humid$Low_Perc-7, humid$High_Perc+7), humid$ID
     , paste(humid$n_study), cex=0.85, adj=0)

# Separation lines
abline(h=c(5,10), col="black", lty=3, lwd=2)

mtext("Humid", side=3, cex=0.75)

dev.off()