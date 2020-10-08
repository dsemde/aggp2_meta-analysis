
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

dataset_geo <- read_csv("datasets/geo_area.csv")
#dataset[sapply(dataset, is.character)] <- lapply(dataset[sapply(dataset, is.character)], as.factor)

# Set TIFF output parameters
tiff("geo area.tiff", width = 6, height = 4, units = 'in', res = 300)

par( mar=c(2, 0.2, 0.2, 0.2)
     , mai=c(0.1, 0.1, 0.1, 0.1)  # by inches, inner margin
     , omi = c(0.1, 0.1, 0.1, 0.1)  # by inches, outer margin
     , tcl = 0.4)



# Pie Chart from data frame with Appended Sample Sizes
mytable <- table(dataset_geo$geo_area)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, col = brewer.pal(n = 7, name = "Pastel2"))

dev.off()
