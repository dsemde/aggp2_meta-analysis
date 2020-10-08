rm(list=ls())

library(car)
library(tidyverse)


sum_data <- read.csv("datasets/irr_v_nat_aridity_texture_irrigation_depth.csv")

#d <- sum_data %>% group_split(study_duration)
#short <- as.data.frame(d[3])
#medium <- as.data.frame(d[2])
#long <- as.data.frame(d[1])

# Set TIFF output parameters
tiff("Figure test - irr v nat aridity.tiff", width = 4, height = 7, units = 'in', res = 300)

par( mar=c(2, 0.2, 0.2, 0.2)
     , mai=c(0.15, 0.1, 0.1, 0.1)  # by inches, inner margin
     , omi = c(0.5, 1.5, 0.1, 0.5)  # by inches, outer margin 
     , mgp = c(0, 0.75, 0) # set distance of axis
     #, tcl = 0.4
     , cex.axis = 0.75
     #, mfrow=c(1,3)
)



#****************************************************************************
# PLOT I - SHORT DURATION
#****************************************************************************

x_min <- min(sum_data$Low_Perc, na.rm = T)
x_max <- max(sum_data$High_Perc, na.rm = T)

sum_data$col <- as.character(sum_data$col)

plot(sum_data$ID ~ sum_data$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-5, x_max)
     , xlab = '', ylab='', main=''
     , col = sum_data$col
     , pch = 20
     # , cex = 0.75
     , cex = ifelse (sum_data$TopGroup == "All data"|sum_data$TopGroup == "SurfaceSubsurface", 0.75, 0.75) 
     # , xaxs="i"
)

arrows(sum_data$Low_Perc, sum_data$ID, sum_data$High_Perc, sum_data$ID
       ,code=3,length=0.05,angle=90,sum_data$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-5, x_max, 50), labels = seq(-5, x_max, 50)
      , cex=0.75
      , las = 1)

axis (side = 2, at = sum_data$ID, labels = paste(sum_data$SubGroup, sep = "")
      , cex=0.75
      , las = 2)

text(ifelse(sum_data$High_Perc > x_max, sum_data$Low_Perc-10, sum_data$High_Perc+5), sum_data$ID
     , paste(sum_data$n_study), cex=0.75, adj=0)

# Separation lines
abline(h=c(6,12,18), col="black", lty=3, lwd=2)

mtext("0 - 10 cm", side=4, cex=0.75, adj=0.85)
mtext("10 - 20 cm", side=4, cex=0.75, adj=0.60)
mtext("20 - 30 cm", side=4, cex=0.75, adj=0.35)
mtext("30+ cm", side=4, cex=0.75, adj=0.15)
mtext("Aridity", side=2, cex=0.75, line=7)
mtext("Irrigated vs. Natural Vegetation", side=3, cex=0.75)
mtext(side = 1, text = paste("Soil carbon stocks (% change)", sep=" "), line = 1, cex=0.75, outer = T)

dev.off()