rm(list=ls())

library(car)
library(tidyverse)


sum_data <- read.csv("datasets/texture_depth_duration.csv")

datasettemp <- split(sum_data, sum_data$TopGroup=="Coarse")
coarse <- datasettemp$'TRUE'
datasettemp <- split(sum_data, sum_data$TopGroup=="Medium")
medium <- datasettemp$'TRUE'
datasettemp <- split(sum_data, sum_data$TopGroup=="Fine")
fine <- datasettemp$'TRUE'

# Set TIFF output parameters
tiff("OC Stock Texture Depth Duration.tiff", width = 9, height = 4, units = 'in', res = 300)

par( mar=c(2, 0.2, 0.2, 0.2)
     , mai=c(0.15, 0.1, 0.1, 0.1)  # by inches, inner margin
     , omi = c(0.5, 1.75, 0.1, 0.1)  # by inches, outer margin 
     , mgp = c(0, 0.3, 0) # set distance of axis
     , tcl = 0.4
     # , cex.axis = 1.0
     , mfrow=c(1,3))



#****************************************************************************
# PLOT I - Coarse
#****************************************************************************

x_min <- min(coarse$Low_Perc, na.rm = T)
x_max <- max(coarse$High_Perc, na.rm = T)

coarse$col <- as.character(coarse$col)

plot(coarse$ID ~ coarse$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-45, 90)
     , xlab = '', ylab='', main=''
     , col = coarse$col
     , pch = 20
     , cex = ifelse (coarse$TopGroup == "All data"|coarse$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
)

arrows(coarse$Low_Perc, coarse$ID, coarse$High_Perc, coarse$ID
       ,code=3,length=0.05,angle=90,coarse$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-40, 90, 10), labels = seq(-40, 90, 10)
      , cex=1
      , las = 1)

axis (side = 2, at = coarse$ID, labels = paste(coarse$SubGroup, sep = "")
      , cex=1
      , las = 2)

text(ifelse(coarse$High_Perc > 85, coarse$Low_Perc-7, coarse$High_Perc+4), coarse$ID
     , paste(coarse$n_study), cex=0.85, adj=0)

# Separation lines
abline(h=c(5,10), col="black", lty=3, lwd=2)

mtext("Short Duration", side=2, cex=0.75, adj=0.95, outer = T, line = 4)
mtext("Medium Duration", side=2, cex=0.75, outer = T, line = 4)
mtext("Long Duration", side=2, cex=0.75, adj=0.05, outer = T, line = 4)

mtext("Coarse-texture", side=3, cex=0.75)

mtext(side = 1, text = paste("Soil carbon stocks (% change)", sep=" "), line = 1, cex=0.75, outer = T, adj = 0.5)


#****************************************************************************
# PLOT II - Medium
#****************************************************************************

x_min <- min(medium$Low_Perc, na.rm = T)
x_max <- max(medium$High_Perc, na.rm = T)

medium$col <- as.character(medium$col)

plot(medium$ID ~ medium$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-45, 90)
     , xlab = '', ylab='', main=''
     , col = medium$col
     , pch = 20
     , cex = ifelse (medium$TopGroup == "All data"|medium$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
)

arrows(medium$Low_Perc, medium$ID, medium$High_Perc, medium$ID
       ,code=3,length=0.05,angle=90,medium$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-40, 90, 10), labels = seq(-40, 90, 10)
      , cex=1
      , las = 1)

text(ifelse(medium$High_Perc > 85, medium$Low_Perc-7, medium$High_Perc+5), medium$ID
     , paste(medium$n_study), cex=0.85, adj=0)

# Separation lines
abline(h=c(5,10), col="black", lty=3, lwd=2)

mtext("Medium-texture", side=3, cex=0.75)


#****************************************************************************
# PLOT III - Fine
#****************************************************************************

x_min <- min(fine$Low_Perc, na.rm = T)
x_max <- max(fine$High_Perc, na.rm = T)

fine$col <- as.character(fine$col)

plot(fine$ID ~ fine$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-45, 90)
     , xlab = '', ylab='', main=''
     , col = fine$col
     , pch = 20
     , cex = ifelse (fine$TopGroup == "All data"|fine$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
)

arrows(fine$Low_Perc, fine$ID, fine$High_Perc, fine$ID
       ,code=3,length=0.05,angle=90,fine$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-40, 90, 10), labels = seq(-40, 90, 10)
      , cex=1
      , las = 1)

text(ifelse(fine$High_Perc > 85, fine$Low_Perc-7, fine$High_Perc+5), fine$ID
     , paste(fine$n_study), cex=0.85, adj=0)

# Separation lines
abline(h=c(5,10), col="black", lty=3, lwd=2)

mtext("Fine-texture", side=3, cex=0.75)

dev.off()