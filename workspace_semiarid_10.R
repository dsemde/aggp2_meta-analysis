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
#*******************************     Semi-Arid 0 - 10 cm     *********************************************************************
#*********************************************************************************************************************************

# Separate out short term
datasettemp <- split(dataset, dataset$aridity_category=="Semi-Arid")
dataset_semi <- datasettemp$'TRUE'

datasettemp <- split(dataset_semi, dataset_semi$depth_cat_2=="0-10cm")
dataset_semi10 <- datasettemp$'TRUE'

datasettemp <- split(dataset_semi10, dataset_semi10$study_duration_cat=="short term")
dataset_short <- datasettemp$'TRUE'
datasettemp <- split(dataset_semi10, dataset_semi10$study_duration_cat=="medium term")
dataset_medium <- datasettemp$'TRUE'
datasettemp <- split(dataset_semi10, dataset_semi10$study_duration_cat=="long term")
dataset_long <- datasettemp$'TRUE'

# Dhort
mean <- check_NaN(w.mean(dataset_short$per_OC_st_change,dataset_short$organic_C_n_t1))
se <- check_se(w.sd(dataset_short$per_OC_st_change,dataset_short$organic_C_n_t1)/sqrt(sum(dataset_short$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_short$organic_C_n_t1)
n_study <- length(dataset_short$organic_C_n_t1)
output <- c("7", "aridity", "Short", "Semi-Arid", mean, se, low, high, "orange", obs, n_study)

# Medium
mean <- check_NaN(w.mean(dataset_medium$per_OC_st_change,dataset_medium$organic_C_n_t1))
se <- check_se(w.sd(dataset_medium$per_OC_st_change,dataset_medium$organic_C_n_t1)/sqrt(sum(dataset_medium$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_medium$organic_C_n_t1)
n_study <- length(dataset_medium$organic_C_n_t1)
add_row <- c("6", "aridity", "Medium", "Semi-Arid", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Long
mean <- check_NaN(w.mean(dataset_long$per_OC_st_change,dataset_long$organic_C_n_t1))
se <- check_se(w.sd(dataset_long$per_OC_st_change,dataset_long$organic_C_n_t1)/sqrt(sum(dataset_long$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_long$organic_C_n_t1)
n_study <- length(dataset_long$organic_C_n_t1)
add_row <- c("5", "aridity", "Long", "Semi-Arid", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)



#*********************************************************************************************************************************
#*******************************     Sprinkler     *******************************************************************************
#*********************************************************************************************************************************

# Separate out short term
datasettemp <- split(dataset, dataset$irrig_methodcat=="sprinkler")
dataset_semi <- datasettemp$'TRUE'

datasettemp <- split(dataset_semi, dataset_semi$depth_cat_2=="0-10cm")
dataset_semi10 <- datasettemp$'TRUE'

datasettemp <- split(dataset_semi10, dataset_semi10$study_duration_cat=="short term")
dataset_short <- datasettemp$'TRUE'
datasettemp <- split(dataset_semi10, dataset_semi10$study_duration_cat=="medium term")
dataset_medium <- datasettemp$'TRUE'
datasettemp <- split(dataset_semi10, dataset_semi10$study_duration_cat=="long term")
dataset_long <- datasettemp$'TRUE'

# Dhort
mean <- check_NaN(w.mean(dataset_short$per_OC_st_change,dataset_short$organic_C_n_t1))
se <- check_se(w.sd(dataset_short$per_OC_st_change,dataset_short$organic_C_n_t1)/sqrt(sum(dataset_short$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_short$organic_C_n_t1)
n_study <- length(dataset_short$organic_C_n_t1)
add_row<- c("3", "aridity", "Short", "Sprinkler", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)

# Medium
mean <- check_NaN(w.mean(dataset_medium$per_OC_st_change,dataset_medium$organic_C_n_t1))
se <- check_se(w.sd(dataset_medium$per_OC_st_change,dataset_medium$organic_C_n_t1)/sqrt(sum(dataset_medium$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_medium$organic_C_n_t1)
n_study <- length(dataset_medium$organic_C_n_t1)
add_row <- c("2", "aridity", "Medium", "Sprinkler", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)

# Long
mean <- check_NaN(w.mean(dataset_long$per_OC_st_change,dataset_long$organic_C_n_t1))
se <- check_se(w.sd(dataset_long$per_OC_st_change,dataset_long$organic_C_n_t1)/sqrt(sum(dataset_long$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_long$organic_C_n_t1)
n_study <- length(dataset_long$organic_C_n_t1)
add_row <- c("1", "aridity", "Long", "Sprinkler", mean, se, low, high, "cadetblue", obs, n_study)
output <- rbind(output, add_row)





# Output CSV file
colnames(output) <- c("ID", "TopGroup", "SubGroup", "treatment", "Mean_Perc", "se", "Low_Perc", "High_Perc", "col", "obs", "n_study")

output <- data.frame(output)
write_csv(output, "datasets/semiarid_0_10_duration.csv")

