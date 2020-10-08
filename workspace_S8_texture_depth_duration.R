library(tidyverse)
library(psych)
library(Weighted.Desc.Stat)
library(plyr)

check_se <- function(se) {
  if(is.nan(se)){
    return(0)
  } else {
    return(se)
  }
}

check_NaN <- function(v) {
  if(is.nan(v)){
    return(NA)
  } else {
    return(v)
  }
}

check_split <- function(df, depth){
  if(!empty(df)){
    temp <- split(df, df$depth_cat_2==depth)
    return(temp$'TRUE')
  } else {
    empty_df = df[FALSE,]  
    return(empty_df)
  }
}

output <- c("ID", "TopGroup", "SubGroup", "treatment", "Mean_Perc", "se", "Low_Perc", "High_Perc", "col", "obs", "n_study")

# Load dataset
datacsv <- read_csv("datasets/irrt1vt2_estBD_june23.csv")
dataset <- data.frame(datacsv)

# Remove outliers
temp_dataset <- split(dataset, dataset$ref_num=="24")
dataset <- temp_dataset$'FALSE'

temp_dataset <- split(dataset, dataset$ref_num=="19")
dataset <- temp_dataset$'FALSE'

# Factor whole dataset
dataset[sapply(dataset, is.character)] <- lapply(dataset[sapply(dataset, is.character)], as.factor)

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
dataset <- temp_dataset$'FALSE'

#*********************************************************************************************************************************
#*******************************     COARSE     **********************************************************************************
#*********************************************************************************************************************************

# Separate out Arid Sites
datasettemp <- split(dataset, dataset$soil_texture=="coarse")
dataset_coarse <- datasettemp$'TRUE'

# Split dataset by Duration
ds <- split(dataset_coarse, dataset_coarse$study_duration_cat=="short term")
short <- ds$'TRUE'

dm <- split(dataset_coarse, dataset_coarse$study_duration_cat=="medium term")
medium <- dm$'TRUE'

dl <- split(dataset_coarse, dataset_coarse$study_duration_cat=="long term")
long <- dl$'TRUE'


#***************************************************************
#  OC Stock - SHORT
#***************************************************************

short10 <- check_split(short, "0-10cm")
short20 <- check_split(short, "10-20cm")
short30 <- check_split(short, "20-30cm")
short30p <- check_split(short, "30+cm")

# 0-10cm
mean <- check_NaN(w.mean(short10$per_OC_st_change,short10$organic_C_n_t1))
se <- check_se(w.sd(short10$per_OC_st_change,short10$organic_C_n_t1)/sqrt(sum(short10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short10$organic_C_n_t1)
n_study <- length(short10$organic_C_n_t1)
output <- c("14", "Coarse", "0-10 cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)

# 10-20cm
mean <- check_NaN(w.mean(short20$per_OC_st_change,short20$organic_C_n_t1))
se <- check_se(w.sd(short20$per_OC_st_change,short20$organic_C_n_t1)/sqrt(sum(short20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short20$organic_C_n_t1)
n_study <- length(short20$organic_C_n_t1)
add_row <- c("13", "Coarse", "10-20 cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 20-30cm
mean <- check_NaN(w.mean(short30$per_OC_st_change,short30$organic_C_n_t1))
se <- check_se(w.sd(short30$per_OC_st_change,short30$organic_C_n_t1)/sqrt(sum(short30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short30$organic_C_n_t1)
n_study <- length(short30$organic_C_n_t1)
add_row <- c("12", "Coarse", "20-30 cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 30+cm
mean <- check_NaN(w.mean(short30p$per_OC_st_change,short30p$organic_C_n_t1))
se <- check_se(w.sd(short30p$per_OC_st_change,short30p$organic_C_n_t1)/sqrt(sum(short30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short30p$organic_C_n_t1)
n_study <- length(short30p$organic_C_n_t1)
add_row <- c("11", "Coarse", "30+ cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# # All Short
# mean <- check_NaN(w.mean(short$per_OC_st_change,short$organic_C_n_t1))
# se <- check_se(w.sd(short$per_OC_st_change,short$organic_C_n_t1)/sqrt(sum(short$organic_C_n_t1)))
# low <- mean-se
# high <- mean+se
# obs <- sum(short$organic_C_n_t1)
# n_study <- length(short$organic_C_n_t1)
# add_row <- c("13", "Coarse", "Short Duration", "short", mean, se, low, high, "black", obs, n_study)
# output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - MEDIUM
#***************************************************************


medium10 <- check_split(medium, "0-10cm")
medium20 <- check_split(medium, "10-20cm")
medium30 <- check_split(medium, "20-30cm")
medium30p <- check_split(medium, "30+cm")

# 0-10cm
mean <- check_NaN(w.mean(medium10$per_OC_st_change,medium10$organic_C_n_t1))
se <- check_se(w.sd(medium10$per_OC_st_change,medium10$organic_C_n_t1)/sqrt(sum(medium10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium10$organic_C_n_t1)
n_study <- length(medium10$organic_C_n_t1)
add_row <- c("9", "Coarse", "0-10 cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 10-20cm
mean <- check_NaN(w.mean(medium20$per_OC_st_change,medium20$organic_C_n_t1))
se <- check_se(w.sd(medium20$per_OC_st_change,medium20$organic_C_n_t1)/sqrt(sum(medium20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium20$organic_C_n_t1)
n_study <- length(medium20$organic_C_n_t1)
add_row <- c("8", "Coarse", "10-20 cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 20-30cm
mean <- check_NaN(w.mean(medium30$per_OC_st_change,medium30$organic_C_n_t1))
se <- check_se(w.sd(medium30$per_OC_st_change,medium30$organic_C_n_t1)/sqrt(sum(medium30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium30$organic_C_n_t1)
n_study <- length(medium30$organic_C_n_t1)
add_row <- c("7", "Coarse", "20-30 cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 30+cm
mean <- check_NaN(w.mean(medium30p$per_OC_st_change,medium30p$organic_C_n_t1))
se <- check_se(w.sd(medium30p$per_OC_st_change,medium30p$organic_C_n_t1)/sqrt(sum(medium30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium30p$organic_C_n_t1)
n_study <- length(medium30p$organic_C_n_t1)
add_row <- c("6", "Coarse", "30+ cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# # All Medium
# mean <- check_NaN(w.mean(medium$per_OC_st_change,medium$organic_C_n_t1))
# se <- check_se(w.sd(medium$per_OC_st_change,medium$organic_C_n_t1)/sqrt(sum(medium$organic_C_n_t1)))
# low <- mean-se
# high <- mean+se
# obs <- sum(medium$organic_C_n_t1)
# n_study <- length(medium$organic_C_n_t1)
# add_row <- c("7", "Coarse", "Medium Duration", "medium", mean, se, low, high, "black", obs, n_study)
# output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - LONG
#***************************************************************

long10 <- check_split(long, "0-10cm")
long20 <- check_split(long, "10-20cm")
long30 <- check_split(long, "20-30cm")
long30p <- check_split(long, "30+cm")

# 0-10cm
mean <- check_NaN(w.mean(long10$per_OC_st_change,long10$organic_C_n_t1))
se <- check_se(w.sd(long10$per_OC_st_change,long10$organic_C_n_t1)/sqrt(sum(long10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long10$organic_C_n_t1)
n_study <- length(long10$organic_C_n_t1)
add_row <- c("4", "Coarse", "0-10 cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 10-20cm
mean <- check_NaN(w.mean(long20$per_OC_st_change,long20$organic_C_n_t1))
se <- check_se(w.sd(long20$per_OC_st_change,long20$organic_C_n_t1)/sqrt(sum(long20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long20$organic_C_n_t1)
n_study <- length(long20$organic_C_n_t1)
add_row <- c("3", "Coarse", "10-20 cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 20-30cm
mean <- check_NaN(w.mean(long30$per_OC_st_change,long30$organic_C_n_t1))
se <- check_se(w.sd(long30$per_OC_st_change,long30$organic_C_n_t1)/sqrt(sum(long30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long30$organic_C_n_t1)
n_study <- length(long30$organic_C_n_t1)
add_row <- c("2", "Coarse", "20-30 cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 30+cm
mean <- check_NaN(w.mean(long30p$per_OC_st_change,long30p$organic_C_n_t1))
se <- check_se(w.sd(long30p$per_OC_st_change,long30p$organic_C_n_t1)/sqrt(sum(long30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long30p$organic_C_n_t1)
n_study <- length(long30p$organic_C_n_t1)
add_row <- c("1", "Coarse", "30+ cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# # All Long
# mean <- check_NaN(w.mean(long$per_OC_st_change,long$organic_C_n_t1))
# se <- check_se(w.sd(long$per_OC_st_change,long$organic_C_n_t1)/sqrt(sum(long$organic_C_n_t1)))
# low <- mean-se
# high <- mean+se
# obs <- sum(long$organic_C_n_t1)
# n_study <- length(long$organic_C_n_t1)
# add_row <- c("1", "Coarse", "Long Duration", "long", mean, se, low, high, "black", obs, n_study)
# output <- rbind(output, add_row)



#*********************************************************************************************************************************
#*******************************     MEDIUM    ***********************************************************************************
#*********************************************************************************************************************************

# Separate out Semi-Arid Sites
datasettemp <- split(dataset, dataset$soil_texture=="medium")
dataset_medium <- datasettemp$'TRUE'

# Split dataset by Duration
ds <- split(dataset_medium, dataset_medium$study_duration_cat=="short term")
short <- ds$'TRUE'
dm <- split(dataset_medium, dataset_medium$study_duration_cat=="medium term")
medium <- dm$'TRUE'
dl <- split(dataset_medium, dataset_medium$study_duration_cat=="long term")
long <- dl$'TRUE'

#***************************************************************
#  OC Stock - SHORT
#***************************************************************

short10 <- check_split(short, "0-10cm")
short20 <- check_split(short, "10-20cm")
short30 <- check_split(short, "20-30cm")
short30p <- check_split(short, "30+cm")

# 0-10cm
mean <- check_NaN(w.mean(short10$per_OC_st_change,short10$organic_C_n_t1))
se <- check_se(w.sd(short10$per_OC_st_change,short10$organic_C_n_t1)/sqrt(sum(short10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short10$organic_C_n_t1)
n_study <- length(short10$organic_C_n_t1)
add_row <- c("14", "Medium", "0-10 cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 10-20cm
mean <- check_NaN(w.mean(short20$per_OC_st_change,short20$organic_C_n_t1))
se <- check_se(w.sd(short20$per_OC_st_change,short20$organic_C_n_t1)/sqrt(sum(short20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short20$organic_C_n_t1)
n_study <- length(short20$organic_C_n_t1)
add_row <- c("13", "Medium", "10-20 cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 20-30cm
mean <- check_NaN(w.mean(short30$per_OC_st_change,short30$organic_C_n_t1))
se <- check_se(w.sd(short30$per_OC_st_change,short30$organic_C_n_t1)/sqrt(sum(short30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short30$organic_C_n_t1)
n_study <- length(short30$organic_C_n_t1)
add_row <- c("12", "Medium", "20-30 cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 30+cm
mean <- check_NaN(w.mean(short30p$per_OC_st_change,short30p$organic_C_n_t1))
se <- check_se(w.sd(short30p$per_OC_st_change,short30p$organic_C_n_t1)/sqrt(sum(short30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short30p$organic_C_n_t1)
n_study <- length(short30p$organic_C_n_t1)
add_row <- c("11", "Medium", "30+ cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# # All Short
# mean <- check_NaN(w.mean(short$per_OC_st_change,short$organic_C_n_t1))
# se <- check_se(w.sd(short$per_OC_st_change,short$organic_C_n_t1)/sqrt(sum(short$organic_C_n_t1)))
# low <- mean-se
# high <- mean+se
# obs <- sum(short$organic_C_n_t1)
# n_study <- length(short$organic_C_n_t1)
# add_row <- c("13", "Medium", "Short Duration", "short", mean, se, low, high, "black", obs, n_study)
# output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - MEDIUM
#***************************************************************

medium10 <- check_split(medium, "0-10cm")
medium20 <- check_split(medium, "10-20cm")
medium30 <- check_split(medium, "20-30cm")
medium30p <- check_split(medium, "30+cm")

# 0-10cm
mean <- check_NaN(w.mean(medium10$per_OC_st_change,medium10$organic_C_n_t1))
se <- check_se(w.sd(medium10$per_OC_st_change,medium10$organic_C_n_t1)/sqrt(sum(medium10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium10$organic_C_n_t1)
n_study <- length(medium10$organic_C_n_t1)
add_row <- c("9", "Medium", "0-10 cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 10-20cm
mean <- check_NaN(w.mean(medium20$per_OC_st_change,medium20$organic_C_n_t1))
se <- check_se(w.sd(medium20$per_OC_st_change,medium20$organic_C_n_t1)/sqrt(sum(medium20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium20$organic_C_n_t1)
n_study <- length(medium20$organic_C_n_t1)
add_row <- c("8", "Medium", "10-20 cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 20-30cm
mean <- check_NaN(w.mean(medium30$per_OC_st_change,medium30$organic_C_n_t1))
se <- check_se(w.sd(medium30$per_OC_st_change,medium30$organic_C_n_t1)/sqrt(sum(medium30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium30$organic_C_n_t1)
n_study <- length(medium30$organic_C_n_t1)
add_row <- c("7", "Medium", "20-30 cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 30+cm
mean <- check_NaN(w.mean(medium30p$per_OC_st_change,medium30p$organic_C_n_t1))
se <- check_se(w.sd(medium30p$per_OC_st_change,medium30p$organic_C_n_t1)/sqrt(sum(medium30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium30p$organic_C_n_t1)
n_study <- length(medium30p$organic_C_n_t1)
add_row <- c("6", "Medium", "30+ cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# # All Medium
# mean <- check_NaN(w.mean(medium$per_OC_st_change,medium$organic_C_n_t1))
# se <- check_se(w.sd(medium$per_OC_st_change,medium$organic_C_n_t1)/sqrt(sum(medium$organic_C_n_t1)))
# low <- mean-se
# high <- mean+se
# obs <- sum(medium$organic_C_n_t1)
# n_study <- length(medium$organic_C_n_t1)
# add_row <- c("7", "Medium", "Medium Duration", "medium", mean, se, low, high, "black", obs, n_study)
# output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - LONG
#***************************************************************

long10 <- check_split(long, "0-10cm")
long20 <- check_split(long, "10-20cm")
long30 <- check_split(long, "20-30cm")
long30p <- check_split(long, "30+cm")

# 0-10cm
mean <- check_NaN(w.mean(long10$per_OC_st_change,long10$organic_C_n_t1))
se <- check_se(w.sd(long10$per_OC_st_change,long10$organic_C_n_t1)/sqrt(sum(long10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long10$organic_C_n_t1)
n_study <- length(long10$organic_C_n_t1)
add_row <- c("4", "Medium", "0-10 cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 10-20cm
mean <- check_NaN(w.mean(long20$per_OC_st_change,long20$organic_C_n_t1))
se <- check_se(w.sd(long20$per_OC_st_change,long20$organic_C_n_t1)/sqrt(sum(long20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long20$organic_C_n_t1)
n_study <- length(long20$organic_C_n_t1)
add_row <- c("3", "Medium", "10-20 cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 20-30cm
mean <- check_NaN(w.mean(long30$per_OC_st_change,long30$organic_C_n_t1))
se <- check_se(w.sd(long30$per_OC_st_change,long30$organic_C_n_t1)/sqrt(sum(long30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long30$organic_C_n_t1)
n_study <- length(long30$organic_C_n_t1)
add_row <- c("2", "Medium", "20-30 cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 30+cm
mean <- check_NaN(w.mean(long30p$per_OC_st_change,long30p$organic_C_n_t1))
se <- check_se(w.sd(long30p$per_OC_st_change,long30p$organic_C_n_t1)/sqrt(sum(long30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long30p$organic_C_n_t1)
n_study <- length(long30p$organic_C_n_t1)
add_row <- c("1", "Medium", "30+ cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# # All Long
# mean <- check_NaN(w.mean(long$per_OC_st_change,long$organic_C_n_t1))
# se <- check_se(w.sd(long$per_OC_st_change,long$organic_C_n_t1)/sqrt(sum(long$organic_C_n_t1)))
# low <- mean-se
# high <- mean+se
# obs <- sum(long$organic_C_n_t1)
# n_study <- length(long$organic_C_n_t1)
# add_row <- c("1", "Medium", "Long Duration", "long", mean, se, low, high, "black", obs, n_study)
# output <- rbind(output, add_row)

#*********************************************************************************************************************************
#*******************************     FINE     ************************************************************************************
#*********************************************************************************************************************************

# Separate out Semi-Arid Sites
datasettemp <- split(dataset, dataset$soil_texture=="fine")
dataset_fine <- datasettemp$'TRUE'

# Split dataset by Duration
ds <- split(dataset_fine, dataset_fine$study_duration_cat=="short term")
short <- ds$'TRUE'
dm <- split(dataset_fine, dataset_fine$study_duration_cat=="medium term")
medium <- dm$'TRUE'
dl <- split(dataset_fine, dataset_fine$study_duration_cat=="long term")
long <- dl$'TRUE'

#***************************************************************
#  OC Stock - SHORT
#***************************************************************

short10 <- check_split(short, "0-10cm")
short20 <- check_split(short, "10-20cm")
short30 <- check_split(short, "20-30cm")
short30p <- check_split(short, "30+cm")

# 0-10cm
mean <- check_NaN(w.mean(short10$per_OC_st_change,short10$organic_C_n_t1))
se <- check_se(w.sd(short10$per_OC_st_change,short10$organic_C_n_t1)/sqrt(sum(short10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short10$organic_C_n_t1)
n_study <- length(short10$organic_C_n_t1)
add_row <- c("14", "Fine", "0-10 cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 10-20cm
mean <- check_NaN(w.mean(short20$per_OC_st_change,short20$organic_C_n_t1))
se <- check_se(w.sd(short20$per_OC_st_change,short20$organic_C_n_t1)/sqrt(sum(short20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short20$organic_C_n_t1)
n_study <- length(short20$organic_C_n_t1)
add_row <- c("13", "Fine", "10-20 cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 20-30cm
mean <- check_NaN(w.mean(short30$per_OC_st_change,short30$organic_C_n_t1))
se <- check_se(w.sd(short30$per_OC_st_change,short30$organic_C_n_t1)/sqrt(sum(short30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short30$organic_C_n_t1)
n_study <- length(short30$organic_C_n_t1)
add_row <- c("12", "Fine", "20-30 cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 30+cm
mean <- check_NaN(w.mean(short30p$per_OC_st_change,short30p$organic_C_n_t1))
se <- check_se(w.sd(short30p$per_OC_st_change,short30p$organic_C_n_t1)/sqrt(sum(short30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(short30p$organic_C_n_t1)
n_study <- length(short30p$organic_C_n_t1)
add_row <- c("11", "Fine", "30+ cm", "short", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# # All Short
# mean <- check_NaN(w.mean(short$per_OC_st_change,short$organic_C_n_t1))
# se <- check_se(w.sd(short$per_OC_st_change,short$organic_C_n_t1)/sqrt(sum(short$organic_C_n_t1)))
# low <- mean-se
# high <- mean+se
# obs <- sum(short$organic_C_n_t1)
# n_study <- length(short$organic_C_n_t1)
# add_row <- c("13", "Fine", "Short Duration", "short", mean, se, low, high, "black", obs, n_study)
# output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - MEDIUM
#***************************************************************

medium10 <- check_split(medium, "0-10cm")
medium20 <- check_split(medium, "10-20cm")
medium30 <- check_split(medium, "20-30cm")
medium30p <- check_split(medium, "30+cm")

# 0-10cm
mean <- check_NaN(w.mean(medium10$per_OC_st_change,medium10$organic_C_n_t1))
se <- check_se(w.sd(medium10$per_OC_st_change,medium10$organic_C_n_t1)/sqrt(sum(medium10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium10$organic_C_n_t1)
n_study <- length(medium10$organic_C_n_t1)
add_row <- c("9", "Fine", "0-10 cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 10-20cm
mean <- check_NaN(w.mean(medium20$per_OC_st_change,medium20$organic_C_n_t1))
se <- check_se(w.sd(medium20$per_OC_st_change,medium20$organic_C_n_t1)/sqrt(sum(medium20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium20$organic_C_n_t1)
n_study <- length(medium20$organic_C_n_t1)
add_row <- c("8", "Fine", "10-20 cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 20-30cm
mean <- check_NaN(w.mean(medium30$per_OC_st_change,medium30$organic_C_n_t1))
se <- check_se(w.sd(medium30$per_OC_st_change,medium30$organic_C_n_t1)/sqrt(sum(medium30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium30$organic_C_n_t1)
n_study <- length(medium30$organic_C_n_t1)
add_row <- c("7", "Fine", "20-30 cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 30+cm
mean <- check_NaN(w.mean(medium30p$per_OC_st_change,medium30p$organic_C_n_t1))
se <- check_se(w.sd(medium30p$per_OC_st_change,medium30p$organic_C_n_t1)/sqrt(sum(medium30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium30p$organic_C_n_t1)
n_study <- length(medium30p$organic_C_n_t1)
add_row <- c("6", "Fine", "30+ cm", "medium", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# # All Medium
# mean <- check_NaN(w.mean(medium$per_OC_st_change,medium$organic_C_n_t1))
# se <- check_se(w.sd(medium$per_OC_st_change,medium$organic_C_n_t1)/sqrt(sum(medium$organic_C_n_t1)))
# low <- mean-se
# high <- mean+se
# obs <- sum(medium$organic_C_n_t1)
# n_study <- length(medium$organic_C_n_t1)
# add_row <- c("7", "Fine", "Medium Duration", "medium", mean, se, low, high, "black", obs, n_study)
# output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - LONG
#***************************************************************

long10 <- check_split(long, "0-10cm")
long20 <- check_split(long, "10-20cm")
long30 <- check_split(long, "20-30cm")
long30p <- check_split(long, "30+cm")

# 0-10cm
mean <- check_NaN(w.mean(long10$per_OC_st_change,long10$organic_C_n_t1))
se <- check_se(w.sd(long10$per_OC_st_change,long10$organic_C_n_t1)/sqrt(sum(long10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long10$organic_C_n_t1)
n_study <- length(long10$organic_C_n_t1)
add_row <- c("4", "Fine", "0-10 cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 10-20cm
mean <- check_NaN(w.mean(long20$per_OC_st_change,long20$organic_C_n_t1))
se <- check_se(w.sd(long20$per_OC_st_change,long20$organic_C_n_t1)/sqrt(sum(long20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long20$organic_C_n_t1)
n_study <- length(long20$organic_C_n_t1)
add_row <- c("3", "Fine", "10-20 cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 20-30cm
mean <- check_NaN(w.mean(long30$per_OC_st_change,long30$organic_C_n_t1))
se <- check_se(w.sd(long30$per_OC_st_change,long30$organic_C_n_t1)/sqrt(sum(long30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long30$organic_C_n_t1)
n_study <- length(long30$organic_C_n_t1)
add_row <- c("2", "Fine", "20-30 cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# 30+cm
mean <- check_NaN(w.mean(long30p$per_OC_st_change,long30p$organic_C_n_t1))
se <- check_se(w.sd(long30p$per_OC_st_change,long30p$organic_C_n_t1)/sqrt(sum(long30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(long30p$organic_C_n_t1)
n_study <- length(long30p$organic_C_n_t1)
add_row <- c("1", "Fine", "30+ cm", "long", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# # All Long
# mean <- check_NaN(w.mean(long$per_OC_st_change,long$organic_C_n_t1))
# se <- check_se(w.sd(long$per_OC_st_change,long$organic_C_n_t1)/sqrt(sum(long$organic_C_n_t1)))
# low <- mean-se
# high <- mean+se
# obs <- sum(long$organic_C_n_t1)
# n_study <- length(long$organic_C_n_t1)
# add_row <- c("1", "Fine", "Long Duration", "long", mean, se, low, high, "black", obs, n_study)
# output <- rbind(output, add_row)


# Output CSV file
colnames(output) <- c("ID", "TopGroup", "SubGroup", "treatment", "Mean_Perc", "se", "Low_Perc", "High_Perc", "col", "obs", "n_study")

output <- data.frame(output)
write_csv(output, "datasets/texture_depth_duration.csv")

