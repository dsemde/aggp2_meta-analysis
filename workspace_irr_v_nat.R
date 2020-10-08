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
datacsv <- read_csv("datasets/irrvnat_estBD.csv")
dataset <- data.frame(datacsv)

# Remove outliers
temp_dataset <- split(dataset, dataset$ref_num=="7")
dataset <- temp_dataset$'FALSE'

# Remove outliers
temp_dataset <- split(dataset, dataset$ref_num=="9")
dataset <- temp_dataset$'FALSE'


#*********************************************************************************************************************************
#*******************************     0 - 10 CM     *******************************************************************************
#*********************************************************************************************************************************

# Separate out Arid Sites
datasettemp <- split(dataset, dataset$depth_category_2=="0-10cm")
dataset_10 <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - ARIDITY
#***************************************************************

arid <- check_split_aridity(dataset_10, "Arid")
semi <- check_split_aridity(dataset_10, "Semi-Arid")
subhumid <- check_split_aridity(dataset_10, "Dry sub-humid")
humid <- check_split_aridity(dataset_10, "Humid")

# Arid
mean <- check_NaN(w.mean(arid$per_OC_st_change,arid$organic_C_n_nat))
se <- check_se(w.sd(arid$per_OC_st_change,arid$organic_C_n_nat)/sqrt(sum(arid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(arid$organic_C_n_nat)
n_study <- length(arid$organic_C_n_nat)
output <- c("23", "Aridity", "Arid", "0 - 10 cm", mean, se, low, high, "orange", obs, n_study)
#output <- c("14", "Aridity", "Arid", "0 - 10 cm", mean, "orange", n_study)

# Semi-arid
mean <- check_NaN(w.mean(semi$per_OC_st_change,semi$organic_C_n_nat))
se <- check_se(w.sd(semi$per_OC_st_change,semi$organic_C_n_nat)/sqrt(sum(semi$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(semi$organic_C_n_nat)
n_study <- length(semi$organic_C_n_nat)
add_row <- c("22", "Aridity", "Semi-arid", "0 - 10 cm", mean, se, low, high, "orange", obs, n_study)
#add_row <- c("14", "Aridity", "Semi-Arid", "0 - 10 cm", mean, "orange", n_study)
output <- rbind(output, add_row)

# Dry sub-humid
mean <- check_NaN(w.mean(subhumid$per_OC_st_change,subhumid$organic_C_n_nat))
se <- check_se(w.sd(subhumid$per_OC_st_change,subhumid$organic_C_n_nat)/sqrt(sum(subhumid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(subhumid$organic_C_n_nat)
n_study <- length(subhumid$organic_C_n_nat)
add_row <- c("21", "Aridity", "Dry sub-humid", "0 - 10 cm", mean, se, low, high, "orange", obs, n_study)
#add_row <- c("14", "Aridity", "Dry sub-humid", "0 - 10 cm", mean, "orange", n_study)
output <- rbind(output, add_row)

# Humid
mean <- check_NaN(w.mean(humid$per_OC_st_change,humid$organic_C_n_nat))
se <- check_se(w.sd(humid$per_OC_st_change,humid$organic_C_n_nat)/sqrt(sum(humid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(humid$organic_C_n_nat)
n_study <- length(humid$organic_C_n_nat)
add_row <- c("20", "Aridity", "Humid", "0 - 10 cm", mean, se, low, high, "orange", obs, n_study)
 #add_row <- c("14", "Aridity", "Humid", "0 - 10 cm", mean, "orange", n_study)
output <- rbind(output, add_row)
# 
# 
# #***************************************************************
# #  OC Stock - TEXTURE
# #***************************************************************
# 
# coarse <- check_split_texture(dataset_10, "coarse")
# medium <- check_split_texture(dataset_10, "medium")
# fine <- check_split_texture(dataset_10, "fine")
# 
# # Coarse
# mean <- check_NaN(w.mean(coarse$per_OC_st_change,coarse$organic_C_n_nat))
# se <- check_se(w.sd(coarse$per_OC_st_change,coarse$organic_C_n_nat)/sqrt(sum(coarse$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(coarse$organic_C_n_nat)
# n_study <- length(coarse$organic_C_n_nat)
# add_row <- c("9", "Texture", "Coarse", "0 - 10 cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Medium
# mean <- check_NaN(w.mean(medium$per_OC_st_change,medium$organic_C_n_nat))
# se <- check_se(w.sd(medium$per_OC_st_change,medium$organic_C_n_nat)/sqrt(sum(medium$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(medium$organic_C_n_nat)
# n_study <- length(medium$organic_C_n_nat)
# add_row <- c("8", "Texture", "Medium", "0 - 10 cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Fine
# mean <- check_NaN(w.mean(fine$per_OC_st_change,fine$organic_C_n_nat))
# se <- check_se(w.sd(fine$per_OC_st_change,fine$organic_C_n_nat)/sqrt(sum(fine$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(fine$organic_C_n_nat)
# n_study <- length(fine$organic_C_n_nat)
# add_row <- c("7", "Texture", "Fine", "0 - 10 cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# #***************************************************************
# #  OC Stock - IRRIGATION
# #***************************************************************
# 
# drip <- check_split_irrigation(dataset_10, "drippers")
# sprinkler <- check_split_irrigation(dataset_10, "sprinkler")
# flood <- check_split_irrigation(dataset_10, "floodfurr")
# 
# # Drip
# mean <- check_NaN(w.mean(drip$per_OC_st_change,drip$organic_C_n_nat))
# se <- check_se(w.sd(drip$per_OC_st_change,drip$organic_C_n_nat)/sqrt(sum(drip$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(drip$organic_C_n_nat)
# n_study <- length(drip$organic_C_n_nat)
# add_row <- c("5", "Irrigation", "Drip", "0 - 10 cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Sprinler
# mean <- check_NaN(w.mean(sprinkler$per_OC_st_change,sprinkler$organic_C_n_nat))
# se <- check_se(w.sd(sprinkler$per_OC_st_change,sprinkler$organic_C_n_nat)/sqrt(sum(sprinkler$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(sprinkler$organic_C_n_nat)
# n_study <- length(sprinkler$organic_C_n_nat)
# add_row <- c("4", "Irrigation", "Sprinkler", "0 - 10 cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Flood/Furrow
# mean <- check_NaN(w.mean(flood$per_OC_st_change,flood$organic_C_n_nat))
# se <- check_se(w.sd(flood$per_OC_st_change,flood$organic_C_n_nat)/sqrt(sum(flood$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(flood$organic_C_n_nat)
# n_study <- length(flood$organic_C_n_nat)
# add_row <- c("3", "Irrigation", "Flood/Furrow", "0 - 10 cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - INCREMENT MEAN
#***************************************************************

# All 0 - 10 cm
mean <- check_NaN(w.mean(dataset_10$per_OC_st_change,dataset_10$organic_C_n_nat))
se <- check_se(w.sd(dataset_10$per_OC_st_change,dataset_10$organic_C_n_nat)/sqrt(sum(dataset_10$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_10$organic_C_n_nat)
n_study <- length(dataset_10$organic_C_n_nat)
add_row <- c("19", "", "Increment Mean", "0 - 10 cm", mean, se, low, high, "black", obs, n_study)
output <- rbind(output, add_row)


#*********************************************************************************************************************************
#*******************************     10 - 20 CM     *******************************************************************************
#*********************************************************************************************************************************

# Separate out Arid Sites
datasettemp <- split(dataset, dataset$depth_category_2=="10-20cm")
dataset_20 <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - ARIDITY
#***************************************************************

arid <- check_split_aridity(dataset_20, "Arid")
semi <- check_split_aridity(dataset_20, "Semi-Arid")
subhumid <- check_split_aridity(dataset_20, "Dry sub-humid")
humid <- check_split_aridity(dataset_20, "Humid")

# Arid
mean <- check_NaN(w.mean(arid$per_OC_st_change,arid$organic_C_n_nat))
se <- check_se(w.sd(arid$per_OC_st_change,arid$organic_C_n_nat)/sqrt(sum(arid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(arid$organic_C_n_nat)
n_study <- length(arid$organic_C_n_nat)
add_row <- c("17", "Aridity", "Arid", "10 - 20 cm", mean, se, low, high, "orange", obs, n_study)
#add_row <- c("14", "Aridity", "Arid", "10 - 20 cm", mean, "orange", n_study)
output <- rbind(output, add_row)

# Semi-arid
mean <- check_NaN(w.mean(semi$per_OC_st_change,semi$organic_C_n_nat))
se <- check_se(w.sd(semi$per_OC_st_change,semi$organic_C_n_nat)/sqrt(sum(semi$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(semi$organic_C_n_nat)
n_study <- length(semi$organic_C_n_nat)
add_row <- c("16", "Aridity", "Semi-arid", "10 - 20 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Dry sub-humid
mean <- check_NaN(w.mean(subhumid$per_OC_st_change,subhumid$organic_C_n_nat))
se <- check_se(w.sd(subhumid$per_OC_st_change,subhumid$organic_C_n_nat)/sqrt(sum(subhumid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(subhumid$organic_C_n_nat)
n_study <- length(subhumid$organic_C_n_nat)
add_row <- c("15", "Aridity", "Dry sub-humid", "10 - 20 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Humid
mean <- check_NaN(w.mean(humid$per_OC_st_change,humid$organic_C_n_nat))
se <- check_se(w.sd(humid$per_OC_st_change,humid$organic_C_n_nat)/sqrt(sum(humid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(humid$organic_C_n_nat)
n_study <- length(humid$organic_C_n_nat)
add_row <- c("14", "Aridity", "Humid", "10 - 20 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)
# 
# 
# #***************************************************************
# #  OC Stock - TEXTURE
# #***************************************************************
# 
# coarse <- check_split_texture(dataset_20, "coarse")
# medium <- check_split_texture(dataset_20, "medium")
# fine <- check_split_texture(dataset_20, "fine")
# 
# # Coarse
# mean <- check_NaN(w.mean(coarse$per_OC_st_change,coarse$organic_C_n_nat))
# se <- check_se(w.sd(coarse$per_OC_st_change,coarse$organic_C_n_nat)/sqrt(sum(coarse$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(coarse$organic_C_n_nat)
# n_study <- length(coarse$organic_C_n_nat)
# add_row <- c("9", "Texture", "Coarse", "10 - 20 cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Medium
# mean <- check_NaN(w.mean(medium$per_OC_st_change,medium$organic_C_n_nat))
# se <- check_se(w.sd(medium$per_OC_st_change,medium$organic_C_n_nat)/sqrt(sum(medium$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(medium$organic_C_n_nat)
# n_study <- length(medium$organic_C_n_nat)
# add_row <- c("8", "Texture", "Medium", "10 - 20 cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Fine
# mean <- check_NaN(w.mean(fine$per_OC_st_change,fine$organic_C_n_nat))
# se <- check_se(w.sd(fine$per_OC_st_change,fine$organic_C_n_nat)/sqrt(sum(fine$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(fine$organic_C_n_nat)
# n_study <- length(fine$organic_C_n_nat)
# add_row <- c("7", "Texture", "Fine", "10 - 20 cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# #***************************************************************
# #  OC Stock - IRRIGATION
# #***************************************************************
# 
# drip <- check_split_irrigation(dataset_20, "drippers")
# sprinkler <- check_split_irrigation(dataset_20, "sprinkler")
# flood <- check_split_irrigation(dataset_20, "floodfurr")
# 
# # Drip
# mean <- check_NaN(w.mean(drip$per_OC_st_change,drip$organic_C_n_nat))
# se <- check_se(w.sd(drip$per_OC_st_change,drip$organic_C_n_nat)/sqrt(sum(drip$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(drip$organic_C_n_nat)
# n_study <- length(drip$organic_C_n_nat)
# add_row <- c("5", "Irrigation", "Drip", "10 - 20 cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Sprinler
# mean <- check_NaN(w.mean(sprinkler$per_OC_st_change,sprinkler$organic_C_n_nat))
# se <- check_se(w.sd(sprinkler$per_OC_st_change,sprinkler$organic_C_n_nat)/sqrt(sum(sprinkler$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(sprinkler$organic_C_n_nat)
# n_study <- length(sprinkler$organic_C_n_nat)
# add_row <- c("4", "Irrigation", "Sprinkler", "10 - 20 cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Flood/Furrow
# mean <- check_NaN(w.mean(flood$per_OC_st_change,flood$organic_C_n_nat))
# se <- check_se(w.sd(flood$per_OC_st_change,flood$organic_C_n_nat)/sqrt(sum(flood$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(flood$organic_C_n_nat)
# n_study <- length(flood$organic_C_n_nat)
# add_row <- c("3", "Irrigation", "Flood/Furrow", "10 - 20 cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)
# 

#***************************************************************
#  OC Stock - INCREMENT MEAN
#***************************************************************

# All 10 - 20 cm
mean <- check_NaN(w.mean(dataset_20$per_OC_st_change,dataset_20$organic_C_n_nat))
se <- check_se(w.sd(dataset_20$per_OC_st_change,dataset_20$organic_C_n_nat)/sqrt(sum(dataset_20$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_20$organic_C_n_nat)
n_study <- length(dataset_20$organic_C_n_nat)
add_row <- c("13", "", "Increment Mean", "10 - 20 cm", mean, se, low, high, "black", obs, n_study)
output <- rbind(output, add_row)


#*********************************************************************************************************************************
#*******************************     20 - 30 CM     *******************************************************************************
#*********************************************************************************************************************************

# Separate out Arid Sites
datasettemp <- split(dataset, dataset$depth_category_2=="20-30cm")
dataset_30 <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - ARIDITY
#***************************************************************

arid <- check_split_aridity(dataset_30, "Arid")
semi <- check_split_aridity(dataset_30, "Semi-Arid")
subhumid <- check_split_aridity(dataset_30, "Dry sub-humid")
humid <- check_split_aridity(dataset_30, "Humid")

# Arid
mean <- check_NaN(w.mean(arid$per_OC_st_change,arid$organic_C_n_nat))
se <- check_se(w.sd(arid$per_OC_st_change,arid$organic_C_n_nat)/sqrt(sum(arid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(arid$organic_C_n_nat)
n_study <- length(arid$organic_C_n_nat)
add_row <- c("11", "Aridity", "Arid", "20 - 30 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Semi-arid
mean <- check_NaN(w.mean(semi$per_OC_st_change,semi$organic_C_n_nat))
se <- check_se(w.sd(semi$per_OC_st_change,semi$organic_C_n_nat)/sqrt(sum(semi$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(semi$organic_C_n_nat)
n_study <- length(semi$organic_C_n_nat)
add_row <- c("10", "Aridity", "Semi-arid", "20 - 30 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Dry sub-humid
mean <- check_NaN(w.mean(subhumid$per_OC_st_change,subhumid$organic_C_n_nat))
se <- check_se(w.sd(subhumid$per_OC_st_change,subhumid$organic_C_n_nat)/sqrt(sum(subhumid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(subhumid$organic_C_n_nat)
n_study <- length(subhumid$organic_C_n_nat)
add_row <- c("9", "Aridity", "Dry sub-humid", "20 - 30 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Humid
mean <- check_NaN(w.mean(humid$per_OC_st_change,humid$organic_C_n_nat))
se <- check_se(w.sd(humid$per_OC_st_change,humid$organic_C_n_nat)/sqrt(sum(humid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(humid$organic_C_n_nat)
n_study <- length(humid$organic_C_n_nat)
add_row <- c("8", "Aridity", "Humid", "20 - 30 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)
# 
# 
# #***************************************************************
# #  OC Stock - TEXTURE
# #***************************************************************
# 
# coarse <- check_split_texture(dataset_30, "coarse")
# medium <- check_split_texture(dataset_30, "medium")
# fine <- check_split_texture(dataset_30, "fine")
# 
# # Coarse
# mean <- check_NaN(w.mean(coarse$per_OC_st_change,coarse$organic_C_n_nat))
# se <- check_se(w.sd(coarse$per_OC_st_change,coarse$organic_C_n_nat)/sqrt(sum(coarse$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(coarse$organic_C_n_nat)
# n_study <- length(coarse$organic_C_n_nat)
# add_row <- c("9", "Texture", "Coarse", "20 - 30 cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Medium
# mean <- check_NaN(w.mean(medium$per_OC_st_change,medium$organic_C_n_nat))
# se <- check_se(w.sd(medium$per_OC_st_change,medium$organic_C_n_nat)/sqrt(sum(medium$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(medium$organic_C_n_nat)
# n_study <- length(medium$organic_C_n_nat)
# add_row <- c("8", "Texture", "Medium", "20 - 30 cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Fine
# mean <- check_NaN(w.mean(fine$per_OC_st_change,fine$organic_C_n_nat))
# se <- check_se(w.sd(fine$per_OC_st_change,fine$organic_C_n_nat)/sqrt(sum(fine$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(fine$organic_C_n_nat)
# n_study <- length(fine$organic_C_n_nat)
# add_row <- c("7", "Texture", "Fine", "20 - 30 cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# #***************************************************************
# #  OC Stock - IRRIGATION
# #***************************************************************
# 
# drip <- check_split_irrigation(dataset_30, "drippers")
# sprinkler <- check_split_irrigation(dataset_30, "sprinkler")
# flood <- check_split_irrigation(dataset_30, "floodfurr")
# 
# # Drip
# mean <- check_NaN(w.mean(drip$per_OC_st_change,drip$organic_C_n_nat))
# se <- check_se(w.sd(drip$per_OC_st_change,drip$organic_C_n_nat)/sqrt(sum(drip$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(drip$organic_C_n_nat)
# n_study <- length(drip$organic_C_n_nat)
# add_row <- c("5", "Irrigation", "Drip", "20 - 30 cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Sprinler
# mean <- check_NaN(w.mean(sprinkler$per_OC_st_change,sprinkler$organic_C_n_nat))
# se <- check_se(w.sd(sprinkler$per_OC_st_change,sprinkler$organic_C_n_nat)/sqrt(sum(sprinkler$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(sprinkler$organic_C_n_nat)
# n_study <- length(sprinkler$organic_C_n_nat)
# add_row <- c("4", "Irrigation", "Sprinkler", "20 - 30 cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Flood/Furrow
# mean <- check_NaN(w.mean(flood$per_OC_st_change,flood$organic_C_n_nat))
# se <- check_se(w.sd(flood$per_OC_st_change,flood$organic_C_n_nat)/sqrt(sum(flood$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(flood$organic_C_n_nat)
# n_study <- length(flood$organic_C_n_nat)
# add_row <- c("3", "Irrigation", "Flood/Furrow", "20 - 30 cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - INCREMENT MEAN
#***************************************************************

# All 20 - 30 cm
mean <- check_NaN(w.mean(dataset_30$per_OC_st_change,dataset_30$organic_C_n_nat))
se <- check_se(w.sd(dataset_30$per_OC_st_change,dataset_30$organic_C_n_nat)/sqrt(sum(dataset_30$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_30$organic_C_n_nat)
n_study <- length(dataset_30$organic_C_n_nat)
add_row <- c("7", "", "Increment Mean", "20 - 30 cm", mean, se, low, high, "black", obs, n_study)
output <- rbind(output, add_row)



#*********************************************************************************************************************************
#*******************************     30+ CM     *******************************************************************************
#*********************************************************************************************************************************

# Separate out Arid Sites
datasettemp <- split(dataset, dataset$depth_category_2=="30+cm")
dataset_30p <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - ARIDITY
#***************************************************************

arid <- check_split_aridity(dataset_30p, "Arid")
semi <- check_split_aridity(dataset_30p, "Semi-Arid")
subhumid <- check_split_aridity(dataset_30p, "Dry sub-humid")
humid <- check_split_aridity(dataset_30p, "Humid")

# Arid
mean <- check_NaN(w.mean(arid$per_OC_st_change,arid$organic_C_n_nat))
se <- check_se(w.sd(arid$per_OC_st_change,arid$organic_C_n_nat)/sqrt(sum(arid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(arid$organic_C_n_nat)
n_study <- length(arid$organic_C_n_nat)
add_row <- c("5", "Aridity", "Arid", "30+ cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Semi-arid
mean <- check_NaN(w.mean(semi$per_OC_st_change,semi$organic_C_n_nat))
se <- check_se(w.sd(semi$per_OC_st_change,semi$organic_C_n_nat)/sqrt(sum(semi$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(semi$organic_C_n_nat)
n_study <- length(semi$organic_C_n_nat)
add_row <- c("4", "Aridity", "Semi-arid", "30+ cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Dry sub-humid
mean <- check_NaN(w.mean(subhumid$per_OC_st_change,subhumid$organic_C_n_nat))
se <- check_se(w.sd(subhumid$per_OC_st_change,subhumid$organic_C_n_nat)/sqrt(sum(subhumid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(subhumid$organic_C_n_nat)
n_study <- length(subhumid$organic_C_n_nat)
add_row <- c("3", "Aridity", "Dry sub-humid", "30+ cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Humid
mean <- check_NaN(w.mean(humid$per_OC_st_change,humid$organic_C_n_nat))
se <- check_se(w.sd(humid$per_OC_st_change,humid$organic_C_n_nat)/sqrt(sum(humid$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(humid$organic_C_n_nat)
n_study <- length(humid$organic_C_n_nat)
add_row <- c("2", "Aridity", "Humid", "30+ cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# 
# #***************************************************************
# #  OC Stock - TEXTURE
# #***************************************************************
# 
# coarse <- check_split_texture(dataset_30p, "coarse")
# medium <- check_split_texture(dataset_30p, "medium")
# fine <- check_split_texture(dataset_30p, "fine")
# 
# # Coarse
# mean <- check_NaN(w.mean(coarse$per_OC_st_change,coarse$organic_C_n_nat))
# se <- check_se(w.sd(coarse$per_OC_st_change,coarse$organic_C_n_nat)/sqrt(sum(coarse$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(coarse$organic_C_n_nat)
# n_study <- length(coarse$organic_C_n_nat)
# add_row <- c("9", "Texture", "Coarse", "30+ cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Medium
# mean <- check_NaN(w.mean(medium$per_OC_st_change,medium$organic_C_n_nat))
# se <- check_se(w.sd(medium$per_OC_st_change,medium$organic_C_n_nat)/sqrt(sum(medium$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(medium$organic_C_n_nat)
# n_study <- length(medium$organic_C_n_nat)
# add_row <- c("8", "Texture", "Medium", "30+ cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Fine
# mean <- check_NaN(w.mean(fine$per_OC_st_change,fine$organic_C_n_nat))
# se <- check_se(w.sd(fine$per_OC_st_change,fine$organic_C_n_nat)/sqrt(sum(fine$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(fine$organic_C_n_nat)
# n_study <- length(fine$organic_C_n_nat)
# add_row <- c("7", "Texture", "Fine", "30+ cm", mean, se, low, high, "#bf63bf", obs, n_study)
# output <- rbind(output, add_row)
# 
# #***************************************************************
# #  OC Stock - IRRIGATION
# #***************************************************************
# 
# drip <- check_split_irrigation(dataset_30p, "drippers")
# sprinkler <- check_split_irrigation(dataset_30p, "sprinkler")
# flood <- check_split_irrigation(dataset_30p, "floodfurr")
# 
# # Drip
# mean <- check_NaN(w.mean(drip$per_OC_st_change,drip$organic_C_n_nat))
# se <- check_se(w.sd(drip$per_OC_st_change,drip$organic_C_n_nat)/sqrt(sum(drip$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(drip$organic_C_n_nat)
# n_study <- length(drip$organic_C_n_nat)
# add_row <- c("5", "Irrigation", "Drip", "30+ cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Sprinler
# mean <- check_NaN(w.mean(sprinkler$per_OC_st_change,sprinkler$organic_C_n_nat))
# se <- check_se(w.sd(sprinkler$per_OC_st_change,sprinkler$organic_C_n_nat)/sqrt(sum(sprinkler$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(sprinkler$organic_C_n_nat)
# n_study <- length(sprinkler$organic_C_n_nat)
# add_row <- c("4", "Irrigation", "Sprinkler", "30+ cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)
# 
# # Flood/Furrow
# mean <- check_NaN(w.mean(flood$per_OC_st_change,flood$organic_C_n_nat))
# se <- check_se(w.sd(flood$per_OC_st_change,flood$organic_C_n_nat)/sqrt(sum(flood$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(flood$organic_C_n_nat)
# n_study <- length(flood$organic_C_n_nat)
# add_row <- c("3", "Irrigation", "Flood/Furrow", "30+ cm", mean, se, low, high, "cadetblue", obs, n_study)
# output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - INCREMENT MEAN
#***************************************************************

# All 20 - 30 cm
mean <- check_NaN(w.mean(dataset_30p$per_OC_st_change,dataset_30p$organic_C_n_nat))
se <- check_se(w.sd(dataset_30p$per_OC_st_change,dataset_30p$organic_C_n_nat)/sqrt(sum(dataset_30p$organic_C_n_nat)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_30p$organic_C_n_nat)
n_study <- length(dataset_30p$organic_C_n_nat)
add_row <- c("1", "", "Increment Mean", "30+ cm", mean, se, low, high, "black", obs, n_study)
output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - OVERALL MEAN
#***************************************************************

# mean <- check_NaN(w.mean(dataset$per_OC_st_change,dataset$organic_C_n_nat))
# se <- check_se(w.sd(dataset$per_OC_st_change,dataset$organic_C_n_nat)/sqrt(sum(dataset$organic_C_n_nat)))
# low <- mean-se
# high <- mean+se
# obs <- sum(dataset$organic_C_n_nat)
# n_study <- length(dataset$organic_C_n_nat)
# add_row <- c("0", "", "Overall Mean", "All", mean, se, low, high, "black", obs, n_study)
# output <- rbind(output, add_row)

# Output CSV file
colnames(output) <- c("ID", "TopGroup", "SubGroup", "treatment", "Mean_Perc", "se", "Low_Perc", "High_Perc", "col", "obs", "n_study")

output <- data.frame(output)
write_csv(output, "datasets/irr_v_nat_aridity_texture_irrigation_depth.csv")

