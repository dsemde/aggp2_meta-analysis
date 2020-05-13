rm(list=ls())

library(car)
library(tidyverse)


sum_data <- read.csv("datasets/duration_plot_stock_may11.csv")

d <- sum_data %>% group_split(study_duration)
short <- as.data.frame(d[3])
medium <- as.data.frame(d[2])
long <- as.data.frame(d[1])

# Set TIFF output parameters
tiff("OC Stock Duration May 11.tiff", width = 9, height = 4, units = 'in', res = 300)

par( mar=c(2, 0.2, 0.2, 0.2)
     , mai=c(0.15, 0.1, 0.1, 0.1)  # by inches, inner margin
     , omi = c(0.5, 1.75, 0.1, 0.1)  # by inches, outer margin 
     , mgp = c(0, 0.3, 0) # set distance of axis
     , tcl = 0.4
     # , cex.axis = 1.0
     , mfrow=c(1,3))



#****************************************************************************
# PLOT I - SHORT DURATION
#****************************************************************************

x_min <- min(short$Low_Perc, na.rm = T)
x_max <- max(short$High_Perc, na.rm = T)

short$col <- as.character(short$col)

plot(short$ID ~ short$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-35, 70)
     , xlab = '', ylab='', main=''
     , col = short$col
     , pch = 20
     # , cex = 0.75
     , cex = ifelse (short$TopGroup == "All data"|short$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
     # , xaxs="i"
)

arrows(short$Low_Perc, short$ID, short$High_Perc, short$ID
       ,code=3,length=0.05,angle=90,short$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-20, 65, 10), labels = seq(-20, 65, 10)
      , cex=1
      , las = 1)

axis (side = 2, at = short$ID, labels = paste(short$SubGroup, sep = "")
      , cex=1
      , las = 2)

text(ifelse(short$High_Perc > 50, short$Low_Perc-10, short$High_Perc+7), short$ID
     , paste(short$obs,"/",short$n_study, sep = ""), cex=0.85, adj=0)

# Separation lines
abline(h=c(38,34,30), col="black", lty=3, lwd=2)

mtext("Short Duration", side=3, cex=0.75)



#****************************************************************************
# PLOT II - MEDIUM DURATION
#****************************************************************************

x_min <- min(medium$Low_Perc, na.rm = T)
x_max <- max(medium$High_Perc, na.rm = T)

medium$col <- as.character(medium$col)

plot(medium$ID ~ medium$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-35, 70)
     , xlab = '', ylab='', main=''
     , col = medium$col
     , pch = 20
     # , cex = 0.75
     , cex = ifelse (medium$TopGroup == "All data"|medium$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
     # , xaxs="i"
)


arrows(medium$Low_Perc, medium$ID, medium$High_Perc, medium$ID
       ,code=3,length=0.05,angle=90,medium$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-20, 65, 10), labels = seq(-20, 65, 10)
      , cex=1
      , las = 1)



# sort(sum_data$High2_Perc)
text(ifelse(medium$High_Perc > 65, medium$Low_Perc-10, medium$High_Perc+7), medium$ID
     , paste(medium$obs,"/",medium$n_study, sep = ""), cex=0.85, adj=0)

# ?axis ()

# Separation lines
abline(h=c(24,20,16), col="black", lty=3, lwd=2)

mtext("Medium Duration", side=3, cex=0.75)

mtext(side = 1, text = paste("Soil carbon stocks (% change)", sep=" "), line = 1, cex=0.75, outer = T, adj = 0.5)


#****************************************************************************
# PLOT III - LONG DURATION
#****************************************************************************
x_min <- min(long$Low_Perc, na.rm = T)
x_max <- max(long$High_Perc, na.rm = T)

long$col <- as.character(long$col)

plot(long$ID ~ long$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-35, 70)
     , xlab = '', ylab='', main=''
     , col = long$col
     , pch = 20
     # , cex = 0.75
     , cex = ifelse (long$TopGroup == "All data"|long$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
     # , xaxs="i"
)


arrows(long$Low_Perc, long$ID, long$High_Perc, long$ID
       ,code=3,length=0.05,angle=90,long$col,lwd=2)

# Separation lines
abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-20, 65, 10), labels = seq(-20, 65, 10)
      , cex=1
      , las = 1)



# sort(sum_data$High2_Perc)
text(ifelse(long$High_Perc > 62, long$Low_Perc-10, long$High_Perc+14), long$ID
     , paste(long$obs,"/",long$n_study, sep = ""), cex=0.85, adj=0)

# ?axis ()

abline(h=c(10,6,2), col="black", lty=3, lwd=2)

mtext("Long Duration", side=3, cex=0.75)


dev.off()