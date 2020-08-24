rm(list=ls())

library(ggpubr)
library(tidyverse)
library(ggplot2)
library(WeMix)
library(ggthemes)

sum_data <- read_csv("datasets/irrt1vt2_estBD_june23.csv")

temp_dataset <- split(sum_data, sum_data$ref_num=="24")
sum_data <- temp_dataset$'FALSE'

temp_dataset <- split(sum_data, sum_data$depth_cat_2=="20-30cm")
sum_data <- temp_dataset$'TRUE'

tempData1a <- split(sum_data, sum_data$N_pair=="no")
dataseta <- tempData1a$'TRUE'

# For rows that do have a N+ and N- paring, store only the N- rows in a temporary variable
tempData1b <- split(sum_data, sum_data$N_pair_state=="N+")
datasetb <- tempData1b$'FALSE'

# Combine the two temporary variables into a unified dataset without the N+ treatments where such pairings exist in the dataset
sum_data <- rbind(dataseta, datasetb)

sum_data$aridity_category <- factor(sum_data$aridity_category, levels = c("Arid", "Semi-Arid", "Dry sub-humid", "Humid"))

ymin <- -100 #min(sum_data$per_OC_st_change)
ymax <- 100 #max(sum_data$per_OC_st_change)

tiff("initial_OC_per_change_20-30.tiff", width = 5, height = 3, units = 'in', res = 300)

par( mar=c(2, 0.2, 0.2, 0.2)
     , mai=c(0.15, 0.1, 0.1, 0.1)  # by inches, inner margin
     , omi = c(0.5, 1.75, 0.1, 0.1)  # by inches, outer margin 
     , mgp = c(0, 0.3, 0) # set distance of axis
     , tcl = 0.4
     # , cex.axis = 1.0
     , mfrow=c(1,4))

# create multiple linear model
lm_fit <- lm(per_OC_st_change ~ organic_C_st_t1, data=sum_data)

# save predictions of the model in the new data frame 
# together with variable you want to plot against
predicted_df <- data.frame(OC_pred = predict(lm_fit, sum_data), OCt1=sum_data$organic_C_st_t1)

x_label <- expression(paste("Initial SOC stock (Mg ha"^"-1", ")"))

p <- ggplot(sum_data, aes(x=organic_C_st_t1, y=per_OC_st_change, color=aridity_category)) + scale_color_manual(values = c("#f089de", "#e0c148", "#84e362", "#5cafb5")) + geom_point() + geom_line(color='black',data = predicted_df, aes(x=OCt1, y=OC_pred)) + labs(y="SOC Change (%)", x=x_label) + coord_cartesian(ylim = c(ymin, ymax))

p + theme_bw() + theme(legend.position = "top", legend.title = element_blank(),
                       axis.text.x = element_text(size = 6),
                       axis.text.y = element_text(size = 6),  
                       axis.title.x = element_text(size = 8),
                       axis.title.y = element_text(size = 8)) #+ annotate("text", x = 50, y = 45, label = "y = 23.96 - 0.6234x", size=2)


dev.off()