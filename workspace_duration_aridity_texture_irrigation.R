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

check_split_aridity <- function(df, cat){
  if(!empty(df)){
    temp <- split(df, df$aridity_category==cat)
    return(temp$'TRUE')
  } else {
    empty_df = df[FALSE,]  
    return(empty_df)
  }
}

check_split_texture <- function(df, cat){
  if(!empty(df)){
    temp <- split(df, df$soil_texture==cat)
    return(temp$'TRUE')
  } else {
    empty_df = df[FALSE,]  
    return(empty_df)
  }
}

check_split_irrigation <- function(df, cat){
  if(!empty(df)){
    temp <- split(df, df$irrig_methodcat==cat)
    return(temp$'TRUE')
  } else {
    empty_df = df[FALSE,]  
    return(empty_df)
  }
}

# Load dataset
datacsv <- read_csv("datasets/irrt1vt2_estBD_may22.csv")
dataset <- data.frame(datacsv)

# Remove outliers
temp_dataset <- split(dataset, dataset$ref_num=="24")
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

#*********************************************************************************************************************************
#*******************************     short term     *******************************************************************************
#*********************************************************************************************************************************

# Separate out Arid Sites
datasettemp <- split(dataset, dataset$study_duration_cat=="short term")
dataset_short <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - ARIDITY
#***************************************************************

arid <- check_split_aridity(dataset_short, "Arid")
semi <- check_split_aridity(dataset_short, "Semi-Arid")
subhumid <- check_split_aridity(dataset_short, "Dry sub-humid")
humid <- check_split_aridity(dataset_short, "Humid")

# Arid
mean <- check_NaN(w.mean(arid$per_OC_st_change,arid$organic_C_n_t1))
se <- check_se(w.sd(arid$per_OC_st_change,arid$organic_C_n_t1)/sqrt(sum(arid$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(arid$organic_C_n_t1)
n_study <- length(arid$organic_C_n_t1)
output <- c("14", "Aridity", "Arid", "short term", mean, se, low, high, "orange", obs, n_study)

# Semi-arid
mean <- check_NaN(w.mean(semi$per_OC_st_change,semi$organic_C_n_t1))
se <- check_se(w.sd(semi$per_OC_st_change,semi$organic_C_n_t1)/sqrt(sum(semi$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(semi$organic_C_n_t1)
n_study <- length(semi$organic_C_n_t1)
add_row <- c("13", "Aridity", "Semi-arid", "short term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Dry sub-humid
mean <- check_NaN(w.mean(subhumid$per_OC_st_change,subhumid$organic_C_n_t1))
se <- check_se(w.sd(subhumid$per_OC_st_change,subhumid$organic_C_n_t1)/sqrt(sum(subhumid$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(subhumid$organic_C_n_t1)
n_study <- length(subhumid$organic_C_n_t1)
add_row <- c("12", "Aridity", "Dry sub-humid", "short term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Humid
mean <- check_NaN(w.mean(humid$per_OC_st_change,humid$organic_C_n_t1))
se <- check_se(w.sd(humid$per_OC_st_change,humid$organic_C_n_t1)/sqrt(sum(humid$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(humid$organic_C_n_t1)
n_study <- length(humid$organic_C_n_t1)
add_row <- c("11", "Aridity", "Humid", "short term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - TEXTURE
#***************************************************************

coarse <- check_split_texture(dataset_short, "coarse")
medium <- check_split_texture(dataset_short, "medium")
fine <- check_split_texture(dataset_short, "fine")

# Coarse
mean <- check_NaN(w.mean(coarse$per_OC_st_change,coarse$organic_C_n_t1))
se <- check_se(w.sd(coarse$per_OC_st_change,coarse$organic_C_n_t1)/sqrt(sum(coarse$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(coarse$organic_C_n_t1)
n_study <- length(coarse$organic_C_n_t1)
add_row <- c("9", "Texture", "Coarse", "short term", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# Medium
mean <- check_NaN(w.mean(medium$per_OC_st_change,medium$organic_C_n_t1))
se <- check_se(w.sd(medium$per_OC_st_change,medium$organic_C_n_t1)/sqrt(sum(medium$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium$organic_C_n_t1)
n_study <- length(medium$organic_C_n_t1)
add_row <- c("8", "Texture", "Medium", "short term", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# Fine
mean <- check_NaN(w.mean(fine$per_OC_st_change,fine$organic_C_n_t1))
se <- check_se(w.sd(fine$per_OC_st_change,fine$organic_C_n_t1)/sqrt(sum(fine$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(fine$organic_C_n_t1)
n_study <- length(fine$organic_C_n_t1)
add_row <- c("7", "Texture", "Fine", "short term", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

#***************************************************************
#  OC Stock - IRRIGATION
#***************************************************************

drip <- check_split_irrigation(dataset_short, "drippers")
sprinkler <- check_split_irrigation(dataset_short, "sprinkler")
flood <- check_split_irrigation(dataset_short, "floodfurr")

# Drip
mean <- check_NaN(w.mean(drip$per_OC_st_change,drip$organic_C_n_t1))
se <- check_se(w.sd(drip$per_OC_st_change,drip$organic_C_n_t1)/sqrt(sum(drip$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(drip$organic_C_n_t1)
n_study <- length(drip$organic_C_n_t1)
add_row <- c("5", "Irrigation", "Drip", "short term", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)

# Sprinler
mean <- check_NaN(w.mean(sprinkler$per_OC_st_change,sprinkler$organic_C_n_t1))
se <- check_se(w.sd(sprinkler$per_OC_st_change,sprinkler$organic_C_n_t1)/sqrt(sum(sprinkler$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(sprinkler$organic_C_n_t1)
n_study <- length(sprinkler$organic_C_n_t1)
add_row <- c("4", "Irrigation", "Sprinkler", "short term", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)

# Flood/Furrow
mean <- check_NaN(w.mean(flood$per_OC_st_change,flood$organic_C_n_t1))
se <- check_se(w.sd(flood$per_OC_st_change,flood$organic_C_n_t1)/sqrt(sum(flood$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(flood$organic_C_n_t1)
n_study <- length(flood$organic_C_n_t1)
add_row <- c("3", "Irrigation", "Flood/Furrow", "short term", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - INCREMENT MEAN
#***************************************************************

# All short term
mean <- check_NaN(w.mean(dataset_short$per_OC_st_change,dataset_short$organic_C_n_t1))
se <- check_se(w.sd(dataset_short$per_OC_st_change,dataset_short$organic_C_n_t1)/sqrt(sum(dataset_short$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_short$organic_C_n_t1)
n_study <- length(dataset_short$organic_C_n_t1)
add_row <- c("1", "", "Duration Mean", "short term", mean, se, low, high, "black", obs, n_study)
output <- rbind(output, add_row)


#*********************************************************************************************************************************
#*******************************     medium term     *******************************************************************************
#*********************************************************************************************************************************

# Separate out Arid Sites
datasettemp <- split(dataset, dataset$study_duration_cat=="medium term")
dataset_med <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - ARIDITY
#***************************************************************

arid <- check_split_aridity(dataset_med, "Arid")
semi <- check_split_aridity(dataset_med, "Semi-Arid")
subhumid <- check_split_aridity(dataset_med, "Dry sub-humid")
humid <- check_split_aridity(dataset_med, "Humid")

# Arid
mean <- check_NaN(w.mean(arid$per_OC_st_change,arid$organic_C_n_t1))
se <- check_se(w.sd(arid$per_OC_st_change,arid$organic_C_n_t1)/sqrt(sum(arid$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(arid$organic_C_n_t1)
n_study <- length(arid$organic_C_n_t1)
add_row <- c("14", "Aridity", "Arid", "medium term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Semi-arid
mean <- check_NaN(w.mean(semi$per_OC_st_change,semi$organic_C_n_t1))
se <- check_se(w.sd(semi$per_OC_st_change,semi$organic_C_n_t1)/sqrt(sum(semi$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(semi$organic_C_n_t1)
n_study <- length(semi$organic_C_n_t1)
add_row <- c("13", "Aridity", "Semi-arid", "medium term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Dry sub-humid
mean <- check_NaN(w.mean(subhumid$per_OC_st_change,subhumid$organic_C_n_t1))
se <- check_se(w.sd(subhumid$per_OC_st_change,subhumid$organic_C_n_t1)/sqrt(sum(subhumid$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(subhumid$organic_C_n_t1)
n_study <- length(subhumid$organic_C_n_t1)
add_row <- c("12", "Aridity", "Dry sub-humid", "medium term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Humid
mean <- check_NaN(w.mean(humid$per_OC_st_change,humid$organic_C_n_t1))
se <- check_se(w.sd(humid$per_OC_st_change,humid$organic_C_n_t1)/sqrt(sum(humid$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(humid$organic_C_n_t1)
n_study <- length(humid$organic_C_n_t1)
add_row <- c("11", "Aridity", "Humid", "medium term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - TEXTURE
#***************************************************************

coarse <- check_split_texture(dataset_med, "coarse")
medium <- check_split_texture(dataset_med, "medium")
fine <- check_split_texture(dataset_med, "fine")

# Coarse
mean <- check_NaN(w.mean(coarse$per_OC_st_change,coarse$organic_C_n_t1))
se <- check_se(w.sd(coarse$per_OC_st_change,coarse$organic_C_n_t1)/sqrt(sum(coarse$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(coarse$organic_C_n_t1)
n_study <- length(coarse$organic_C_n_t1)
add_row <- c("9", "Texture", "Coarse", "medium term", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# Medium
mean <- check_NaN(w.mean(medium$per_OC_st_change,medium$organic_C_n_t1))
se <- check_se(w.sd(medium$per_OC_st_change,medium$organic_C_n_t1)/sqrt(sum(medium$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium$organic_C_n_t1)
n_study <- length(medium$organic_C_n_t1)
add_row <- c("8", "Texture", "Medium", "medium term", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# Fine
mean <- check_NaN(w.mean(fine$per_OC_st_change,fine$organic_C_n_t1))
se <- check_se(w.sd(fine$per_OC_st_change,fine$organic_C_n_t1)/sqrt(sum(fine$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(fine$organic_C_n_t1)
n_study <- length(fine$organic_C_n_t1)
add_row <- c("7", "Texture", "Fine", "medium term", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

#***************************************************************
#  OC Stock - IRRIGATION
#***************************************************************

drip <- check_split_irrigation(dataset_med, "drippers")
sprinkler <- check_split_irrigation(dataset_med, "sprinkler")
flood <- check_split_irrigation(dataset_med, "floodfurr")

# Drip
mean <- check_NaN(w.mean(drip$per_OC_st_change,drip$organic_C_n_t1))
se <- check_se(w.sd(drip$per_OC_st_change,drip$organic_C_n_t1)/sqrt(sum(drip$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(drip$organic_C_n_t1)
n_study <- length(drip$organic_C_n_t1)
add_row <- c("5", "Irrigation", "Drip", "medium term", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)

# Sprinler
mean <- check_NaN(w.mean(sprinkler$per_OC_st_change,sprinkler$organic_C_n_t1))
se <- check_se(w.sd(sprinkler$per_OC_st_change,sprinkler$organic_C_n_t1)/sqrt(sum(sprinkler$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(sprinkler$organic_C_n_t1)
n_study <- length(sprinkler$organic_C_n_t1)
add_row <- c("4", "Irrigation", "Sprinkler", "medium term", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)

# Flood/Furrow
mean <- check_NaN(w.mean(flood$per_OC_st_change,flood$organic_C_n_t1))
se <- check_se(w.sd(flood$per_OC_st_change,flood$organic_C_n_t1)/sqrt(sum(flood$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(flood$organic_C_n_t1)
n_study <- length(flood$organic_C_n_t1)
add_row <- c("3", "Irrigation", "Flood/Furrow", "medium term", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - INCREMENT MEAN
#***************************************************************

# All medium term
mean <- check_NaN(w.mean(dataset_med$per_OC_st_change,dataset_med$organic_C_n_t1))
se <- check_se(w.sd(dataset_med$per_OC_st_change,dataset_med$organic_C_n_t1)/sqrt(sum(dataset_med$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_med$organic_C_n_t1)
n_study <- length(dataset_med$organic_C_n_t1)
add_row <- c("1", "", "Duration Mean", "medium term", mean, se, low, high, "black", obs, n_study)
output <- rbind(output, add_row)


#*********************************************************************************************************************************
#*******************************     long term     *******************************************************************************
#*********************************************************************************************************************************

# Separate out Arid Sites
datasettemp <- split(dataset, dataset$study_duration_cat=="long term")
dataset_long <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - ARIDITY
#***************************************************************

arid <- check_split_aridity(dataset_long, "Arid")
semi <- check_split_aridity(dataset_long, "Semi-Arid")
subhumid <- check_split_aridity(dataset_long, "Dry sub-humid")
humid <- check_split_aridity(dataset_long, "Humid")

# Arid
mean <- check_NaN(w.mean(arid$per_OC_st_change,arid$organic_C_n_t1))
se <- check_se(w.sd(arid$per_OC_st_change,arid$organic_C_n_t1)/sqrt(sum(arid$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(arid$organic_C_n_t1)
n_study <- length(arid$organic_C_n_t1)
add_row <- c("14", "Aridity", "Arid", "long term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Semi-arid
mean <- check_NaN(w.mean(semi$per_OC_st_change,semi$organic_C_n_t1))
se <- check_se(w.sd(semi$per_OC_st_change,semi$organic_C_n_t1)/sqrt(sum(semi$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(semi$organic_C_n_t1)
n_study <- length(semi$organic_C_n_t1)
add_row <- c("13", "Aridity", "Semi-arid", "long term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Dry sub-humid
mean <- check_NaN(w.mean(subhumid$per_OC_st_change,subhumid$organic_C_n_t1))
se <- check_se(w.sd(subhumid$per_OC_st_change,subhumid$organic_C_n_t1)/sqrt(sum(subhumid$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(subhumid$organic_C_n_t1)
n_study <- length(subhumid$organic_C_n_t1)
add_row <- c("12", "Aridity", "Dry sub-humid", "long term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Humid
mean <- check_NaN(w.mean(humid$per_OC_st_change,humid$organic_C_n_t1))
se <- check_se(w.sd(humid$per_OC_st_change,humid$organic_C_n_t1)/sqrt(sum(humid$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(humid$organic_C_n_t1)
n_study <- length(humid$organic_C_n_t1)
add_row <- c("11", "Aridity", "Humid", "long term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - TEXTURE
#***************************************************************

coarse <- check_split_texture(dataset_long, "coarse")
medium <- check_split_texture(dataset_long, "medium")
fine <- check_split_texture(dataset_long, "fine")

# Coarse
mean <- check_NaN(w.mean(coarse$per_OC_st_change,coarse$organic_C_n_t1))
se <- check_se(w.sd(coarse$per_OC_st_change,coarse$organic_C_n_t1)/sqrt(sum(coarse$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(coarse$organic_C_n_t1)
n_study <- length(coarse$organic_C_n_t1)
add_row <- c("9", "Texture", "Coarse", "long term", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# Medium
mean <- check_NaN(w.mean(medium$per_OC_st_change,medium$organic_C_n_t1))
se <- check_se(w.sd(medium$per_OC_st_change,medium$organic_C_n_t1)/sqrt(sum(medium$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(medium$organic_C_n_t1)
n_study <- length(medium$organic_C_n_t1)
add_row <- c("8", "Texture", "Medium", "long term", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# Fine
mean <- check_NaN(w.mean(fine$per_OC_st_change,fine$organic_C_n_t1))
se <- check_se(w.sd(fine$per_OC_st_change,fine$organic_C_n_t1)/sqrt(sum(fine$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(fine$organic_C_n_t1)
n_study <- length(fine$organic_C_n_t1)
add_row <- c("7", "Texture", "Fine", "long term", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

#***************************************************************
#  OC Stock - IRRIGATION
#***************************************************************

drip <- check_split_irrigation(dataset_long, "drippers")
sprinkler <- check_split_irrigation(dataset_long, "sprinkler")
flood <- check_split_irrigation(dataset_long, "floodfurr")

# Drip
mean <- check_NaN(w.mean(drip$per_OC_st_change,drip$organic_C_n_t1))
se <- check_se(w.sd(drip$per_OC_st_change,drip$organic_C_n_t1)/sqrt(sum(drip$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(drip$organic_C_n_t1)
n_study <- length(drip$organic_C_n_t1)
add_row <- c("5", "Irrigation", "Drip", "long term", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)

# Sprinler
mean <- check_NaN(w.mean(sprinkler$per_OC_st_change,sprinkler$organic_C_n_t1))
se <- check_se(w.sd(sprinkler$per_OC_st_change,sprinkler$organic_C_n_t1)/sqrt(sum(sprinkler$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(sprinkler$organic_C_n_t1)
n_study <- length(sprinkler$organic_C_n_t1)
add_row <- c("4", "Irrigation", "Sprinkler", "long term", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)

# Flood/Furrow
mean <- check_NaN(w.mean(flood$per_OC_st_change,flood$organic_C_n_t1))
se <- check_se(w.sd(flood$per_OC_st_change,flood$organic_C_n_t1)/sqrt(sum(flood$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(flood$organic_C_n_t1)
n_study <- length(flood$organic_C_n_t1)
add_row <- c("3", "Irrigation", "Flood/Furrow", "long term", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - INCREMENT MEAN
#***************************************************************

# All long term
mean <- check_NaN(w.mean(dataset_long$per_OC_st_change,dataset_long$organic_C_n_t1))
se <- check_se(w.sd(dataset_long$per_OC_st_change,dataset_long$organic_C_n_t1)/sqrt(sum(dataset_long$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_long$organic_C_n_t1)
n_study <- length(dataset_long$organic_C_n_t1)
add_row <- c("1", "", "Duration Mean", "long term", mean, se, low, high, "black", obs, n_study)
output <- rbind(output, add_row)

# Output CSV file
colnames(output) <- c("ID", "TopGroup", "SubGroup", "treatment", "Mean_Perc", "se", "Low_Perc", "High_Perc", "col", "obs", "n_study")

output <- data.frame(output)
write_csv(output, "datasets/duration_aridity_texture_irrigation.csv")

