rm(list=ls())

library(car)
library(tidyverse)


sum_data <- read.csv("datasets/forest_plot_stock_apr20.csv")

d <- sum_data %>% group_split(depth_increment)
d10 <- as.data.frame(d[1])
d20 <- as.data.frame(d[2])
d30 <- as.data.frame(d[3])
d30p <- as.data.frame(d[4])

# Set TIFF output parameters
tiff("OC Stock Meta April 23.tiff", width = 12, height = 4, units = 'in', res = 300)

par( mar=c(2, 0.2, 0.2, 0.2)
     , mai=c(0.15, 0.1, 0.1, 0.1)  # by inches, inner margin
     , omi = c(0.5, 1.75, 0.1, 0.1)  # by inches, outer margin 
     , mgp = c(0, 0.3, 0) # set distance of axis
     , tcl = 0.4
     # , cex.axis = 1.0
     , mfrow=c(1,4))



#****************************************************************************
# PLOT I - 0-10 CM
#****************************************************************************

x_min <- min(d10$Low_Perc, na.rm = T)
x_max <- max(d10$High_Perc, na.rm = T)

d10$col <- as.character(d10$col)

plot(d10$ID ~ d10$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-35, 70)
     , xlab = '', ylab='', main=''
     , col = d10$col
     , pch = 20
     # , cex = 0.75
     , cex = ifelse (d10$TopGroup == "All data"|d10$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
     # , xaxs="i"
)

arrows(d10$Low_Perc, d10$ID, d10$High_Perc, d10$ID
       ,code=3,length=0.05,angle=90,d10$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-20, 65, 10), labels = seq(-20, 65, 10)
      , cex=1
      , las = 1)

axis (side = 2, at = d10$ID, labels = paste(d10$SubGroup, sep = "")
      , cex=1
      , las = 2)

text(ifelse(d10$High_Perc > 50, d10$Low_Perc-10, d10$High_Perc+7), d10$ID
     , paste(d10$obs,"/",d10$n_study, sep = ""), cex=0.85, adj=0)

# Separation lines
abline(h=c(52,48,44), col="black", lty=3, lwd=2)

mtext("0 - 10 cm", side=3, cex=0.75)



#****************************************************************************
# PLOT II - 10-20 CM
#****************************************************************************

x_min <- min(d20$Low_Perc, na.rm = T)
x_max <- max(d20$High_Perc, na.rm = T)

d20$col <- as.character(d20$col)

plot(d20$ID ~ d20$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-35, 70)
     , xlab = '', ylab='', main=''
     , col = d20$col
     , pch = 20
     # , cex = 0.75
     , cex = ifelse (d20$TopGroup == "All data"|d20$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
     # , xaxs="i"
)


arrows(d20$Low_Perc, d20$ID, d20$High_Perc, d20$ID
       ,code=3,length=0.05,angle=90,d20$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-20, 65, 10), labels = seq(-20, 65, 10)
      , cex=1
      , las = 1)



# sort(sum_data$High2_Perc)
text(ifelse(d20$High_Perc > 65, d20$Low_Perc-10, d20$High_Perc+7), d20$ID
     , paste(d20$obs,"/",d20$n_study, sep = ""), cex=0.85, adj=0)

# ?axis ()

# Separation lines
abline(h=c(38,34,30), col="black", lty=3, lwd=2)

mtext("10 - 20 cm", side=3, cex=0.75)

mtext(side = 1, text = paste("Soil carbon stocks (% change)", sep=" "), line = 1, cex=0.75, outer = T, adj = 0.5)


#****************************************************************************
# PLOT III - 20-30 CM
#****************************************************************************
x_min <- min(d30$Low_Perc, na.rm = T)
x_max <- max(d30$High_Perc, na.rm = T)

d30$col <- as.character(d30$col)

plot(d30$ID ~ d30$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-35, 70)
     , xlab = '', ylab='', main=''
     , col = d30$col
     , pch = 20
     # , cex = 0.75
     , cex = ifelse (d30$TopGroup == "All data"|d30$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
     # , xaxs="i"
)


arrows(d30$Low_Perc, d30$ID, d30$High_Perc, d30$ID
       ,code=3,length=0.05,angle=90,d30$col,lwd=2)

# Separation lines
abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-20, 65, 10), labels = seq(-20, 65, 10)
      , cex=1
      , las = 1)



# sort(sum_data$High2_Perc)
text(ifelse(d30$High_Perc > 62, d30$Low_Perc-10, d30$High_Perc+14), d30$ID
     , paste(d30$obs,"/",d30$n_study, sep = ""), cex=0.85, adj=0)

# ?axis ()

abline(h=c(24,20,16), col="black", lty=3, lwd=2)

mtext("20 - 30 cm", side=3, cex=0.75)



#****************************************************************************
# PLOT IV - 30+ CM
#****************************************************************************

x_min <- min(d30p$Low_Perc, na.rm = T)
x_max <- max(d30p$High_Perc, na.rm = T)

d30p$col <- as.character(d30p$col)

plot(d30p$ID ~ d30p$Mean_Perc, lwd=2
     , las=1
     , xaxt='n', yaxt='n'
     , xlim = c(-35, 70)
     , xlab = '', ylab='', main=''
     , col = d30p$col
     , pch = 20
     # , cex = 0.75
     , cex = ifelse (d30p$TopGroup == "All data"|d30p$TopGroup == "SurfaceSubsurface", 1.5, 1.5) 
     # , xaxs="i"
)


arrows(d30p$Low_Perc, d30p$ID, d30p$High_Perc, d30p$ID
       ,code=3,length=0.05,angle=90,d30p$col,lwd=2)

abline(v=0, col="red", lty=2, lwd=2)


axis (side = 1, at = seq(-20, 65, 10), labels = seq(-20, 65, 10)
      , cex=1
      , las = 1)



# sort(sum_data$High2_Perc)
text(ifelse(d30p$High_Perc > 62, d30p$Low_Perc-10, d30p$High_Perc+7), d30p$ID
     , paste(d30p$obs,"/",d30p$n_study, sep = ""), cex=0.85, adj=0)

# ?axis ()

# Separation lines
abline(h=c(10,6,2), col="black", lty=3, lwd=2)

mtext("30+ cm", side=3, cex=0.75)


dev.off()