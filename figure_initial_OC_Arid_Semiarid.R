rm(list=ls())

library(ggpubr)
library(tidyverse)
library(ggplot2)


sum_data <- read_csv("datasets/irrt1vt2_estBD_may22.csv")

temp_dataset <- split(sum_data, sum_data$ref_num=="24")
sum_data <- temp_dataset$'FALSE'

tempData1a <- split(sum_data, sum_data$N_pair=="no")
dataseta <- tempData1a$'TRUE'

# For rows that do have a N+ and N- paring, store only the N- rows in a temporary variable
tempData1b <- split(sum_data, sum_data$N_pair_state=="N+")
datasetb <- tempData1b$'FALSE'

# Combine the two temporary variables into a unified dataset without the N+ treatments where such pairings exist in the dataset
sum_data <- rbind(dataseta, datasetb)

# temp_dataset <- split(sum_data, sum_data$depth_cat_2=="30+cm")
# sum_data <- temp_dataset$'FALSE'

temp_dataset <- split(sum_data, sum_data$aridity_category=="Arid")
arid_data <- temp_dataset$'TRUE'

temp_dataset <- split(sum_data, sum_data$aridity_category=="Semi-Arid")
semi_data <- temp_dataset$'TRUE'

da <- arid_data %>% group_split(depth_cat_2)
arid_d10 <- as.data.frame(da[1])
arid_d20 <- as.data.frame(da[2])
arid_d30 <- as.data.frame(da[3])
arid_d30p <- as.data.frame(da[4])

ds <- semi_data %>% group_split(depth_cat_2)
semi_d10 <- as.data.frame(ds[1])
semi_d20 <- as.data.frame(ds[2])
semi_d30 <- as.data.frame(ds[3])
semi_d30p <- as.data.frame(ds[4])

ymin <- min(sum_data$per_OC_st_change)
ymax <- max(sum_data$per_OC_st_change)
#xmin <- min(sum_data$years_since_irrig_t1)
#xmax <- max(sum_data$years_since_irrig_t1)

# Set TIFF output parameters
tiff("initial_OC_aridity_depth.tiff", width = 12, height = 5.5, units = 'in', res = 300)

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

p1 <- ggplot(arid_d10, aes(x=organic_C_st_t1, y=per_OC_st_change)) + geom_point() + geom_smooth(method=lm) + ggtitle("0 - 10 cm") + labs(y="Arid") + theme(axis.title.x=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


#****************************************************************************
# PLOT II - 10-20 CM
#****************************************************************************

p2 <- ggplot(arid_d20, aes(x=organic_C_st_t1, y=per_OC_st_change))+ geom_point() + geom_smooth(method=lm) + ggtitle("10 - 20 cm") + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


#****************************************************************************
# PLOT III - 20-30 CM
#****************************************************************************
p3 <- ggplot(arid_d30, aes(x=organic_C_st_t1, y=per_OC_st_change)) + geom_point() + geom_smooth(method=lm) + ggtitle("20 - 30 cm") + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


#****************************************************************************
# PLOT IV - 30+ CM
#****************************************************************************

p4 <- ggplot(arid_d30p, aes(x=organic_C_st_t1, y=per_OC_st_change)) + geom_point() + geom_smooth(method=lm) + ggtitle("30+ cm") + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


#****************************************************************************
# PLOT I - 0-10 CM
#****************************************************************************

p5 <- ggplot(semi_d10, aes(x=organic_C_st_t1, y=per_OC_st_change)) + geom_point() + geom_smooth(method=lm) + ggtitle("0 - 10 cm") + labs(y="Semi-Arid") + theme(axis.title.x=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


#****************************************************************************
# PLOT II - 10-20 CM
#****************************************************************************

p6 <- ggplot(semi_d20, aes(x=organic_C_st_t1, y=per_OC_st_change))+ geom_point() + geom_smooth(method=lm) + ggtitle("10 - 20 cm") + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


#****************************************************************************
# PLOT III - 20-30 CM
#****************************************************************************
p7 <- ggplot(semi_d30, aes(x=organic_C_st_t1, y=per_OC_st_change)) + geom_point() + geom_smooth(method=lm) + ggtitle("20 - 30 cm") + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


#****************************************************************************
# PLOT IV - 30+ CM
#****************************************************************************

p8 <- ggplot(semi_d30p, aes(x=organic_C_st_t1, y=per_OC_st_change)) + geom_point() + geom_smooth(method=lm) + ggtitle("30+ cm") + theme(axis.title.x=element_blank(), axis.title.y=element_blank()) + coord_cartesian(ylim = c(ymin, ymax))


figure <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8,
                    ncol = 4, nrow = 2)

annotate_figure(figure,
                bottom = text_grob("Initial SOC Stock (Mg/ha)"),
                left = text_grob("SOC stock change (%)", rot = 90))

dev.off()