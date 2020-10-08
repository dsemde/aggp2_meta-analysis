library(tidyverse)
library(ggplot2)
library(RColorBrewer)

dataset <- read_csv("datasets/geo_area.csv")
as.factor(dataset$year)


#tiff("Figure 1 - study by year.tiff", width = 7.5, height = 4, units = 'in', res = 300)

#par(mar=c(5.1, 2.1, 2.1, 10))

plot <- ggplot(dataset, aes(year)) + geom_bar() + theme_bw() + xlab("Publication Year") + ylab("Count") + theme(plot.margin=unit(c(0.1,0.2,0.1,0.1),"in"))
# Pie Chart from data frame with Appended Sample Sizes
#mytable <- table(dataset_geo$geo_area)
#lbls <- paste(names(mytable), "\n", mytable, sep="")
#pie(mytable, labels = lbls, col = brewer.pal(n = 7, name = "Pastel2"))

ggsave(filename = "Figure 1 - study by year.tiff", plot = plot, width = 7.5, height = 4)

#dev.off()
