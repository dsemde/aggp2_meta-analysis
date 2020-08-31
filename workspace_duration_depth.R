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
datacsv <- read_csv("datasets/irrt1vt2_estBD_may22.csv")
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

#*********************************************************************************************************************************
#*******************************     short term     *******************************************************************************
#*********************************************************************************************************************************

# Separate out short term
datasettemp <- split(dataset, dataset$study_duration_cat=="short term")
dataset_short <- datasettemp$'TRUE'


d10 <- check_split_depth(dataset_short, "0-10cm")
d20 <- check_split_depth(dataset_short, "10-20cm")
d30 <- check_split_depth(dataset_short, "20-30cm")
d30p <- check_split_depth(dataset_short, "30+cm")

# 0 - 10 cm
mean <- check_NaN(w.mean(d10$per_OC_st_change,d10$organic_C_n_t1))
se <- check_se(w.sd(d10$per_OC_st_change,d10$organic_C_n_t1)/sqrt(sum(d10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d10$organic_C_n_t1)
n_study <- length(d10$organic_C_n_t1)
output <- c("17", "aridity", "0 - 10 cm", "short term", mean, se, low, high, "orange", obs, n_study)

# 10 - 20 cm
mean <- check_NaN(w.mean(d20$per_OC_st_change,d20$organic_C_n_t1))
se <- check_se(w.sd(d20$per_OC_st_change,d20$organic_C_n_t1)/sqrt(sum(d20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d20$organic_C_n_t1)
n_study <- length(d20$organic_C_n_t1)
add_row <- c("16", "aridity", "10 - 20 cm", "short term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# 20 - 30 cm
mean <- check_NaN(w.mean(d30$per_OC_st_change,d30$organic_C_n_t1))
se <- check_se(w.sd(d30$per_OC_st_change,d30$organic_C_n_t1)/sqrt(sum(d30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d30$organic_C_n_t1)
n_study <- length(d30$organic_C_n_t1)
add_row <- c("15", "aridity", "20 - 30 cm", "short term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# 30+ cm
mean <- check_NaN(w.mean(d30p$per_OC_st_change,d30p$organic_C_n_t1))
se <- check_se(w.sd(d30p$per_OC_st_change,d30p$organic_C_n_t1)/sqrt(sum(d30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d30p$organic_C_n_t1)
n_study <- length(d30p$organic_C_n_t1)
add_row <- c("14", "aridity", " 30+ cm", "short term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

#***************************************************************
#  OC Stock -duration mean
#***************************************************************

# All short term
mean <- check_NaN(w.mean(dataset_short$per_OC_st_change,dataset_short$organic_C_n_t1))
se <- check_se(w.sd(dataset_short$per_OC_st_change,dataset_short$organic_C_n_t1)/sqrt(sum(dataset_short$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_short$organic_C_n_t1)
n_study <- length(dataset_short$organic_C_n_t1)
add_row <- c("13", "", "Duration Mean", "short term", mean, se, low, high, "black", obs, n_study)
output <- rbind(output, add_row)



#*********************************************************************************************************************************
#*******************************     medium term     *******************************************************************************
#*********************************************************************************************************************************

# Separate out medium term
datasettemp <- split(dataset, dataset$study_duration_cat=="medium term")
dataset_med <- datasettemp$'TRUE'

d10 <- check_split_depth(dataset_med, "0-10cm")
d20 <- check_split_depth(dataset_med, "10-20cm")
d30 <- check_split_depth(dataset_med, "20-30cm")
d30p <- check_split_depth(dataset_med, "30+cm")

# 0 - 10 cm
mean <- check_NaN(w.mean(d10$per_OC_st_change,d10$organic_C_n_t1))
se <- check_se(w.sd(d10$per_OC_st_change,d10$organic_C_n_t1)/sqrt(sum(d10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d10$organic_C_n_t1)
n_study <- length(d10$organic_C_n_t1)
add_row <- c("11", "aridity", "0 - 10 cm", "medium term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# 10 - 20 cm
mean <- check_NaN(w.mean(d20$per_OC_st_change,d20$organic_C_n_t1))
se <- check_se(w.sd(d20$per_OC_st_change,d20$organic_C_n_t1)/sqrt(sum(d20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d20$organic_C_n_t1)
n_study <- length(d20$organic_C_n_t1)
add_row <- c("10", "aridity", "10 - 20 cm", "medium term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# 20 - 30 cm
mean <- check_NaN(w.mean(d30$per_OC_st_change,d30$organic_C_n_t1))
se <- check_se(w.sd(d30$per_OC_st_change,d30$organic_C_n_t1)/sqrt(sum(d30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d30$organic_C_n_t1)
n_study <- length(d30$organic_C_n_t1)
add_row <- c("9", "aridity", "20 - 30 cm", "medium term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# 30+ cm
mean <- check_NaN(w.mean(d30p$per_OC_st_change,d30p$organic_C_n_t1))
se <- check_se(w.sd(d30p$per_OC_st_change,d30p$organic_C_n_t1)/sqrt(sum(d30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d30p$organic_C_n_t1)
n_study <- length(d30p$organic_C_n_t1)
add_row <- c("8", "aridity", "30+ cm", "medium term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - duration mean
#***************************************************************

# All medium term
mean <- check_NaN(w.mean(dataset_med$per_OC_st_change,dataset_med$organic_C_n_t1))
se <- check_se(w.sd(dataset_med$per_OC_st_change,dataset_med$organic_C_n_t1)/sqrt(sum(dataset_med$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(dataset_med$organic_C_n_t1)
n_study <- length(dataset_med$organic_C_n_t1)
add_row <- c("7", "", "Duration Mean", "medium term", mean, se, low, high, "black", obs, n_study)
output <- rbind(output, add_row)


#*********************************************************************************************************************************
#*******************************     long term     *******************************************************************************
#*********************************************************************************************************************************

# Separate out long term
datasettemp <- split(dataset, dataset$study_duration_cat=="long term")
dataset_long <- datasettemp$'TRUE'

d10 <- check_split_depth(dataset_long, "0-10cm")
d20 <- check_split_depth(dataset_long, "10-20cm")
d30 <- check_split_depth(dataset_long, "20-30cm")
d30p <- check_split_depth(dataset_long, "30+cm")

# 0 - 10cm
mean <- check_NaN(w.mean(d10$per_OC_st_change,d10$organic_C_n_t1))
se <- check_se(w.sd(d10$per_OC_st_change,d10$organic_C_n_t1)/sqrt(sum(d10$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d10$organic_C_n_t1)
n_study <- length(d10$organic_C_n_t1)
add_row <- c("5", "aridity", "0 - 10 cm", "long term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# 10 - 20 cm
mean <- check_NaN(w.mean(d20$per_OC_st_change,d20$organic_C_n_t1))
se <- check_se(w.sd(d20$per_OC_st_change,d20$organic_C_n_t1)/sqrt(sum(d20$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d20$organic_C_n_t1)
n_study <- length(d20$organic_C_n_t1)
add_row <- c("4", "aridity", "10 - 20 cm", "long term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# 20 - 30 cm
mean <- check_NaN(w.mean(d30$per_OC_st_change,d30$organic_C_n_t1))
se <- check_se(w.sd(d30$per_OC_st_change,d30$organic_C_n_t1)/sqrt(sum(d30$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d30$organic_C_n_t1)
n_study <- length(d30$organic_C_n_t1)
add_row <- c("3", "aridity", "20 - 30 cm", "long term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# 30+ cm
mean <- check_NaN(w.mean(d30p$per_OC_st_change,d30p$organic_C_n_t1))
se <- check_se(w.sd(d30p$per_OC_st_change,d30p$organic_C_n_t1)/sqrt(sum(d30p$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(d30p$organic_C_n_t1)
n_study <- length(d30p$organic_C_n_t1)
add_row <- c("2", "aridity", "30+ cm", "long term", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)


#***************************************************************
#  OC Stock - duration mean
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
write_csv(output, "datasets/duration_depth.csv")

