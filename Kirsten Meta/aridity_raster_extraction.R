rm(list=ls())

# Load libraries
library(raster)
library(tidyverse)


# Function to unload conflictine 'extract' function calls
detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

# Function to assign climate class based on aridity index value
get_climate_class <- function(df){
  clim_output <- "climate_category"
  for (row in 1:nrow(df)){
    if(df[row, "standard_aridity_index"]<0.03){
      climate_category <- "Hyper Arid"
    }else if(df[row, "standard_aridity_index"]>=0.03 && df[row, "standard_aridity_index"]<0.2){
      climate_category <- "Arid"
    }else if(df[row, "standard_aridity_index"]>=0.2 && df[row, "standard_aridity_index"]<0.5){
      climate_category <- "Semi-Arid"
    }else if(df[row, "standard_aridity_index"]>=0.5 && df[row, "standard_aridity_index"]<0.65){
      climate_category <- "Dry sub-humid"
    }else if(df[row, "standard_aridity_index"]>=0.65){
      climate_category <- "Humid"
    }else{
      climate_category <- "Inappropriate AI value"
    }
    clim_output <- rbind(clim_output, climate_category)
  }
  clim_output <- clim_output[-1,]
  return(clim_output)
}

# Load rasters
et0 <- raster("Kirsten Meta/rasters/et0yr.tif")
precip <- raster("Kirsten Meta/rasters/wc2.1_30s_bio_12.tif")
temp <- raster("Kirsten Meta/rasters/wc2.1_30s_bio_1.tif")
elevation <- raster("Kirsten Meta/rasters/wc2.1_30s_elev.tif")

# Load dataset
location <- read_csv("Kirsten Meta/datasets/meta_location_modified.csv", col_names = TRUE)

# Separate out longitude and latitude from dataset
reference <- data.frame(reference=location[,1])
coords <- data.frame(lon=location[,9], lat=location[,7])

# Unload conflicting libraries
detach_package(tidyverse)
detach_package(broom)
detach_package(tidyr)

# Extract evapotranspiration values
et0_values <- extract(et0, coords)
et0_values <- data.frame(et0_values)

# Extract precipitation values
precip_values <- extract(precip, coords)
precip_values <- data.frame(precip_values)

# Calculate Standard Aridity Index Values
ai <- precip_values/et0_values
colnames(ai)[1] <- "standard_aridity_index"


# Calculate Inverted Aridity Index Values
ai_inv <- et0_values/precip_values
colnames(ai_inv)[1] <- "inverted_aridity_index"


# Extract temperature values
temp_values <- extract(temp, coords)
temp_values <- data.frame(temp_values)

# Extract elevation values
elev_values <- extract(elevation, coords)
elev_values <- data.frame(elev_values)


# Reload tidyverse for csv output
library(tidyverse)

output <- cbind(reference, location[,2],coords, et0_values, precip_values, ai, ai_inv)
clim_class <- data.frame(climate_category=get_climate_class(output))
output <- cbind(output, clim_class, temp_values, elev_values)

write_csv(output, "Kirsten Meta/calc_aridity_index.csv")
