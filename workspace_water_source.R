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

check_split_water <- function(df, cat){
  if(!empty(df)){
    temp <- split(df, df$irrig_water_type==cat)
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

#*********************************************************************************************************************************
#*******************************     0 - 10 CM     *******************************************************************************
#*********************************************************************************************************************************

# Separate out fresh Sites
datasettemp <- split(dataset, dataset$depth_cat_2=="0-10cm")
dataset_10 <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - freshITY
#***************************************************************

fresh <- check_split_water(dataset_10, "fresh")
waste <- check_split_water(dataset_10, "waste")
sewage <- check_split_water(dataset_10, "sewage")
mixed <- check_split_water(dataset_10, "mixed")

# Fresh
mean <- check_NaN(w.mean(fresh$per_OC_st_change,fresh$organic_C_n_t1))
se <- check_se(w.sd(fresh$per_OC_st_change,fresh$organic_C_n_t1)/sqrt(sum(fresh$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(fresh$organic_C_n_t1)
n_study <- length(fresh$organic_C_n_t1)
output <- c("19", "freshity", "fresh", "0 - 10 cm", mean, se, low, high, "orange", obs, n_study)

# Waste
mean <- check_NaN(w.mean(waste$per_OC_st_change,waste$organic_C_n_t1))
se <- check_se(w.sd(waste$per_OC_st_change,waste$organic_C_n_t1)/sqrt(sum(waste$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(waste$organic_C_n_t1)
n_study <- length(waste$organic_C_n_t1)
add_row <- c("18", "freshity", "waste", "0 - 10 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Sewage
mean <- check_NaN(w.mean(sewage$per_OC_st_change,sewage$organic_C_n_t1))
se <- check_se(w.sd(sewage$per_OC_st_change,sewage$organic_C_n_t1)/sqrt(sum(sewage$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(sewage$organic_C_n_t1)
n_study <- length(sewage$organic_C_n_t1)
add_row <- c("17", "freshity", "sewage", "0 - 10 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Mixed
mean <- check_NaN(w.mean(mixed$per_OC_st_change,mixed$organic_C_n_t1))
se <- check_se(w.sd(mixed$per_OC_st_change,mixed$organic_C_n_t1)/sqrt(sum(mixed$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(mixed$organic_C_n_t1)
n_study <- length(mixed$organic_C_n_t1)
add_row <- c("16", "freshity", "mixed", "0 - 10 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)


#*********************************************************************************************************************************
#*******************************     10 - 20 CM     *******************************************************************************
#*********************************************************************************************************************************

# Separate out fresh Sites
datasettemp <- split(dataset, dataset$depth_cat_2=="10-20cm")
dataset_20 <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - freshITY
#***************************************************************

fresh <- check_split_water(dataset_20, "fresh")
waste <- check_split_water(dataset_20, "waste")
sewage <- check_split_water(dataset_20, "sewage")
mixed <- check_split_water(dataset_20, "mixed")

# fresh
mean <- check_NaN(w.mean(fresh$per_OC_st_change,fresh$organic_C_n_t1))
se <- check_se(w.sd(fresh$per_OC_st_change,fresh$organic_C_n_t1)/sqrt(sum(fresh$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(fresh$organic_C_n_t1)
n_study <- length(fresh$organic_C_n_t1)
add_row <- c("14", "freshity", "fresh", "10 - 20 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# waste-fresh
mean <- check_NaN(w.mean(waste$per_OC_st_change,waste$organic_C_n_t1))
se <- check_se(w.sd(waste$per_OC_st_change,waste$organic_C_n_t1)/sqrt(sum(waste$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(waste$organic_C_n_t1)
n_study <- length(waste$organic_C_n_t1)
add_row <- c("13", "freshity", "waste", "10 - 20 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Dry sub-mixed
mean <- check_NaN(w.mean(sewage$per_OC_st_change,sewage$organic_C_n_t1))
se <- check_se(w.sd(sewage$per_OC_st_change,sewage$organic_C_n_t1)/sqrt(sum(sewage$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(sewage$organic_C_n_t1)
n_study <- length(sewage$organic_C_n_t1)
add_row <- c("12", "freshity", "sewage", "10 - 20 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# mixed
mean <- check_NaN(w.mean(mixed$per_OC_st_change,mixed$organic_C_n_t1))
se <- check_se(w.sd(mixed$per_OC_st_change,mixed$organic_C_n_t1)/sqrt(sum(mixed$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(mixed$organic_C_n_t1)
n_study <- length(mixed$organic_C_n_t1)
add_row <- c("11", "freshity", "mixed", "10 - 20 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)



#*********************************************************************************************************************************
#*******************************     20 - 30 CM     *******************************************************************************
#*********************************************************************************************************************************

# Separate out fresh Sites
datasettemp <- split(dataset, dataset$depth_cat_2=="20-30cm")
dataset_30 <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - freshITY
#***************************************************************

fresh <- check_split_water(dataset_30, "fresh")
waste <- check_split_water(dataset_30, "waste")
sewage <- check_split_water(dataset_30, "sewage")
mixed <- check_split_water(dataset_30, "mixed")

# fresh
mean <- check_NaN(w.mean(fresh$per_OC_st_change,fresh$organic_C_n_t1))
se <- check_se(w.sd(fresh$per_OC_st_change,fresh$organic_C_n_t1)/sqrt(sum(fresh$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(fresh$organic_C_n_t1)
n_study <- length(fresh$organic_C_n_t1)
add_row <- c("9", "freshity", "fresh", "20 - 30 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# waste-fresh
mean <- check_NaN(w.mean(waste$per_OC_st_change,waste$organic_C_n_t1))
se <- check_se(w.sd(waste$per_OC_st_change,waste$organic_C_n_t1)/sqrt(sum(waste$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(waste$organic_C_n_t1)
n_study <- length(waste$organic_C_n_t1)
add_row <- c("8", "freshity", "waste", "20 - 30 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Dry sub-mixed
mean <- check_NaN(w.mean(sewage$per_OC_st_change,sewage$organic_C_n_t1))
se <- check_se(w.sd(sewage$per_OC_st_change,sewage$organic_C_n_t1)/sqrt(sum(sewage$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(sewage$organic_C_n_t1)
n_study <- length(sewage$organic_C_n_t1)
add_row <- c("7", "freshity", "sewage", "20 - 30 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# mixed
mean <- check_NaN(w.mean(mixed$per_OC_st_change,mixed$organic_C_n_t1))
se <- check_se(w.sd(mixed$per_OC_st_change,mixed$organic_C_n_t1)/sqrt(sum(mixed$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(mixed$organic_C_n_t1)
n_study <- length(mixed$organic_C_n_t1)
add_row <- c("6", "freshity", "mixed", "20 - 30 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)




#*********************************************************************************************************************************
#*******************************     30+ CM     *******************************************************************************
#*********************************************************************************************************************************

# Separate out fresh Sites
datasettemp <- split(dataset, dataset$depth_cat_2=="30+cm")
dataset_30p <- datasettemp$'TRUE'


#***************************************************************
#  OC Stock - freshITY
#***************************************************************

fresh <- check_split_water(dataset_30p, "fresh")
waste <- check_split_water(dataset_30p, "waste")
sewage <- check_split_water(dataset_30p, "sewage")
mixed <- check_split_water(dataset_30p, "mixed")

# fresh
mean <- check_NaN(w.mean(fresh$per_OC_st_change,fresh$organic_C_n_t1))
se <- check_se(w.sd(fresh$per_OC_st_change,fresh$organic_C_n_t1)/sqrt(sum(fresh$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(fresh$organic_C_n_t1)
n_study <- length(fresh$organic_C_n_t1)
add_row <- c("4", "freshity", "fresh", "30+ cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# waste-fresh
mean <- check_NaN(w.mean(waste$per_OC_st_change,waste$organic_C_n_t1))
se <- check_se(w.sd(waste$per_OC_st_change,waste$organic_C_n_t1)/sqrt(sum(waste$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(waste$organic_C_n_t1)
n_study <- length(waste$organic_C_n_t1)
add_row <- c("3", "freshity", "waste", "30+ cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# Dry sub-mixed
mean <- check_NaN(w.mean(sewage$per_OC_st_change,sewage$organic_C_n_t1))
se <- check_se(w.sd(sewage$per_OC_st_change,sewage$organic_C_n_t1)/sqrt(sum(sewage$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(sewage$organic_C_n_t1)
n_study <- length(sewage$organic_C_n_t1)
add_row <- c("2", "freshity", "sewage", "30+ cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# mixed
mean <- check_NaN(w.mean(mixed$per_OC_st_change,mixed$organic_C_n_t1))
se <- check_se(w.sd(mixed$per_OC_st_change,mixed$organic_C_n_t1)/sqrt(sum(mixed$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(mixed$organic_C_n_t1)
n_study <- length(mixed$organic_C_n_t1)
add_row <- c("1", "freshity", "mixed", "30+ cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)


# Output CSV file
colnames(output) <- c("ID", "TopGroup", "SubGroup", "treatment", "Mean_Perc", "se", "Low_Perc", "High_Perc", "col", "obs", "n_study")

output <- data.frame(output)
write_csv(output, "datasets/water_source_depth.csv")

