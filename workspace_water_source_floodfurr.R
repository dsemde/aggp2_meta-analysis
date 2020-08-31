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

# Remove outliers
temp_dataset <- split(dataset, dataset$irrig_methodcat=="floodfurr")
datasettest <- temp_dataset$'TRUE'

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


# fresh
mean <- check_NaN(w.mean(fresh$per_OC_st_change,fresh$organic_C_n_t1))
se <- check_se(w.sd(fresh$per_OC_st_change,fresh$organic_C_n_t1)/sqrt(sum(fresh$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(fresh$organic_C_n_t1)
n_study <- length(fresh$organic_C_n_t1)
add_row <- c("8", "freshity", "fresh", "10 - 20 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# waste
mean <- check_NaN(w.mean(waste$per_OC_st_change,waste$organic_C_n_t1))
se <- check_se(w.sd(waste$per_OC_st_change,waste$organic_C_n_t1)/sqrt(sum(waste$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(waste$organic_C_n_t1)
n_study <- length(waste$organic_C_n_t1)
add_row <- c("7", "freshity", "waste", "10 - 20 cm", mean, se, low, high, "orange", obs, n_study)
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


# fresh
mean <- check_NaN(w.mean(fresh$per_OC_st_change,fresh$organic_C_n_t1))
se <- check_se(w.sd(fresh$per_OC_st_change,fresh$organic_C_n_t1)/sqrt(sum(fresh$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(fresh$organic_C_n_t1)
n_study <- length(fresh$organic_C_n_t1)
add_row <- c("5", "freshity", "fresh", "20 - 30 cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# waste
mean <- check_NaN(w.mean(waste$per_OC_st_change,waste$organic_C_n_t1))
se <- check_se(w.sd(waste$per_OC_st_change,waste$organic_C_n_t1)/sqrt(sum(waste$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(waste$organic_C_n_t1)
n_study <- length(waste$organic_C_n_t1)
add_row <- c("4", "freshity", "waste", "20 - 30 cm", mean, se, low, high, "orange", obs, n_study)
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

# fresh
mean <- check_NaN(w.mean(fresh$per_OC_st_change,fresh$organic_C_n_t1))
se <- check_se(w.sd(fresh$per_OC_st_change,fresh$organic_C_n_t1)/sqrt(sum(fresh$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(fresh$organic_C_n_t1)
n_study <- length(fresh$organic_C_n_t1)
add_row <- c("2", "freshity", "fresh", "30+ cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)

# waste
mean <- check_NaN(w.mean(waste$per_OC_st_change,waste$organic_C_n_t1))
se <- check_se(w.sd(waste$per_OC_st_change,waste$organic_C_n_t1)/sqrt(sum(waste$organic_C_n_t1)))
low <- mean-se
high <- mean+se
obs <- sum(waste$organic_C_n_t1)
n_study <- length(waste$organic_C_n_t1)
add_row <- c("1", "freshity", "waste", "30+ cm", mean, se, low, high, "orange", obs, n_study)
output <- rbind(output, add_row)



# Output CSV file
colnames(output) <- c("ID", "TopGroup", "SubGroup", "treatment", "Mean_Perc", "se", "Low_Perc", "High_Perc", "col", "obs", "n_study")

output <- data.frame(output)
write_csv(output, "datasets/water_source_depth_floodfurr.csv")

