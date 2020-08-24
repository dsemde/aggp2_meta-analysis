rm(list=ls())

library(tidyverse)
library(metafor)

datacsv <- read_csv("datasets/irrt1vt2_estBD_june23.csv")
dataset <- data.frame(datacsv)

tiff("publication bias.tiff", width = 9, height = 4, units = 'in', res = 300)

par( mar=c(0.2, 0.2, 0.2, 0.2)
     , mai=c(0.1, 1.0, 0.1, 0.1)  # by inches, inner margin
     , omi = c(0.5, 0.5, 0.1, 0.1)  # by inches, outer margin 
     , mgp = c(0, 0.3, 0) # set distance of axis
     , tcl = 0.4
     , cex.axis = 0.75
     , mfrow=c(1,2))

# Remove outliers
temp_dataset <- split(dataset, dataset$ref_num=="24")
dataset <- temp_dataset$'FALSE'

# Store rows without N+ and N- pairings in a temporary variable
tempData1a <- split(dataset, dataset$N_pair=="no")
dataseta <- tempData1a$'TRUE'

# For rows that do have a N+ and N- paring, store only the N- rows in a temporary variable
tempData1b <- split(dataset, dataset$N_pair_state=="N+")
datasetb <- tempData1b$'FALSE'

# Combine the two temporary variables into a unified dataset without the N+ treatments where such pairings exist in the dataset
dataset <- rbind(dataseta, datasetb)

# Separate out the 0 - 10 cm depth incrememt
tempData2 <- split(dataset, dataset$depth_category=="0-10cm")
depth_10 <- tempData2$'TRUE'



my_data_C <- escalc(n1i = organic_C_n_t1, n2i = organic_C_n_t2, m1i = organic_C_t1, m2i = organic_C_t2, 
                    sd1i = organic_C_t1_sd_calc, sd2i = organic_C_t2_sd_calc, data = dataset, measure = "MD", 
                    append = TRUE)

ma_model <- rma(yi, vi, data = my_data_C)
trim_model <- trimfill(ma_model)
#summary(ma_model)
#summary(trim_model)



funnel(ma_model, ylab=" ", xlab=" ")
funnel(trim_model, ylab=" ", xlab=" ")

mtext("Standard Error", side=2, line = 23, cex=1.0, adj=0.5)
mtext("Sprinkler Irrigation", side=1, line = 1.5, cex=1.0, adj=-1.0)

dev.off()
