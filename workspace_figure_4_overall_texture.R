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

check_split_depth <- function(df, cat){
  if(!empty(df)){
    temp <- split(df, df$depth_cat_2==cat)
    return(temp$'TRUE')
  } else {
    empty_df = df[FALSE,]  
    return(empty_df)
  }
}

# Load dataset
datacsv <- read_csv("datasets/irrt1vt2_estBD_june23.csv")
dataset <- data.frame(datacsv)

# Remove outliers
temp_dataset <- split(dataset, dataset$ref_num=="24")
dataset <- temp_dataset$'FALSE'

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
dataset <- temp_dataset$'FALSE'

#*********************************************************************************************************************************
#*******************************     Texture     *********************************************************************************
#*********************************************************************************************************************************

datasettemp <- split(dataset, dataset$soil_texture=="coarse")
dataset_coarse <- datasettemp$'TRUE'
datasettemp <- split(dataset, dataset$soil_texture=="medium")
dataset_medium <- datasettemp$'TRUE'
datasettemp <- split(dataset, dataset$soil_texture=="fine")
dataset_fine <- datasettemp$'TRUE'

# Coarse
mean <- check_NaN(w.mean(dataset_coarse$per_OC_st_change,dataset_coarse$organic_C_n_t1))
se <- check_se(w.sd(dataset_coarse$per_OC_st_change,dataset_coarse$organic_C_n_t1)/sqrt(sum(dataset_coarse$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_coarse$organic_C_n_t1)
n_study <- length(dataset_coarse$organic_C_n_t1)
output <- c("7", "texture", "Coarse", "Overall", mean, se, low, high, "#bf63bf", obs, n_study)

# Medium
mean <- check_NaN(w.mean(dataset_medium$per_OC_st_change,dataset_medium$organic_C_n_t1))
se <- check_se(w.sd(dataset_medium$per_OC_st_change,dataset_medium$organic_C_n_t1)/sqrt(sum(dataset_medium$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_medium$organic_C_n_t1)
n_study <- length(dataset_medium$organic_C_n_t1)
add_row <- c("6", "texture", "Medium", "Overall", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)

# Fine
mean <- check_NaN(w.mean(dataset_fine$per_OC_st_change,dataset_fine$organic_C_n_t1))
se <- check_se(w.sd(dataset_fine$per_OC_st_change,dataset_fine$organic_C_n_t1)/sqrt(sum(dataset_fine$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_fine$organic_C_n_t1)
n_study <- length(dataset_fine$organic_C_n_t1)
add_row <- c("5", "texture", "Fine", "Overall", mean, se, low, high, "#bf63bf", obs, n_study)
output <- rbind(output, add_row)



# Output CSV file
colnames(output) <- c("ID", "TopGroup", "SubGroup", "treatment", "Mean_Perc", "se", "Low_Perc", "High_Perc", "col", "obs", "n_study")

output <- data.frame(output)
write_csv(output, "datasets/overall_texture.csv")