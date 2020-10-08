rm(list=ls())

library(ggpubr)
library(tidyverse)
library(ggplot2)


dataset <- read_csv("datasets/irrt1vt2_estBD_june23.csv")

# Remove outliers
temp_dataset <- split(dataset, dataset$ref_num=="24")
dataset <- temp_dataset$'FALSE'

# Remove outliers
temp_dataset <- split(dataset, dataset$ref_num=="19")
dataset <- temp_dataset$'FALSE'

# Factor whole dataset
#dataset[sapply(dataset, is.character)] <- lapply(dataset[sapply(dataset, is.character)], as.factor)

# Store rows without N+ and N- pairings in a temporary variable
tempData1a <- split(dataset, dataset$N_pair=="no")
dataseta <- tempData1a$'TRUE'

# For rows that do have a N+ and N- paring, store only the N- rows in a temporary variable
tempData1b <- split(dataset, dataset$N_pair_state=="N+")
datasetb <- tempData1b$'FALSE'

# Combine the two temporary variables into a unified dataset without the N+ treatments where such pairings exist in the dataset
dataset <- rbind(dataseta, datasetb)

dataset$irrig_water_type[dataset$irrig_water_type == "sewage"] <- "waste"
dataset$irrig_water_type[dataset$irrig_water_type == "mixed"] <- "waste"

temp_dataset <- split(dataset, dataset$irrig_water_type=="waste")
sum_data <- temp_dataset$'FALSE'

d <- sum_data %>% group_split(depth_cat_2)
d10 <- as.data.frame(d[1])
d20 <- as.data.frame(d[2])
d30 <- as.data.frame(d[3])
d30p <- as.data.frame(d[4])

ymin <- min(sum_data$per_OC_st_change)
ymax <- max(sum_data$per_OC_st_change)
#xmin <- min(sum_data$years_since_irrig_t1)
#xmax <- max(sum_data$years_since_irrig_t1)

# Set TIFF output parameters
tiff("Figure S5 - time since t1.tiff", width = 12, height = 3, units = 'in', res = 300)

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

p1 <- ggplot(d10, aes(x=years_since_irrig_t1, y=per_OC_st_change)) + geom_point() + geom_smooth(method=lm) + ggtitle("0 - 10 cm") + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


#****************************************************************************
# PLOT II - 10-20 CM
#****************************************************************************
 
p2 <- ggplot(d20, aes(x=years_since_irrig_t1, y=per_OC_st_change))+ geom_point() + geom_smooth(method=lm) + ggtitle("10 - 20 cm") + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))
 

#****************************************************************************
# PLOT III - 20-30 CM
#****************************************************************************
p3 <- ggplot(d30, aes(x=years_since_irrig_t1, y=per_OC_st_change)) + geom_point() + geom_smooth(method=lm) + ggtitle("20 - 30 cm") + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


#****************************************************************************
# PLOT IV - 30+ CM
#****************************************************************************

p4 <- ggplot(d30p, aes(x=years_since_irrig_t1, y=per_OC_st_change)) + geom_point() + geom_smooth(method=lm) + ggtitle("30+ cm") + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


figure <- ggarrange(p1, p2, p3, p4,
          ncol = 4, nrow = 1)

annotate_figure(figure,
                bottom = text_grob("Years since irrigation began (time 1)"),
                left = text_grob("SOC stock change (%)", rot = 90))

dev.off()