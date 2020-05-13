library(tidyverse)
library(ggplot2)
library(RColorBrewer)

dataset <- read_csv("datasets/irrt1vt2_estBD_apr20.csv")
as.factor(dataset$year)


tiff("study by year.tiff", width = 7.5, height = 4, units = 'in', res = 300)



ggplot(dataset, aes(year)) + geom_bar() + theme_bw() + xlab("Publication Year") + ylab("Count")
# Pie Chart from data frame with Appended Sample Sizes
#mytable <- table(dataset_geo$geo_area)
#lbls <- paste(names(mytable), "\n", mytable, sep="")
#pie(mytable, labels = lbls, col = brewer.pal(n = 7, name = "Pastel2"))

dev.off()
