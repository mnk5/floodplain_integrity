#####################
# Floodplain Integrity Assessment
# Index Calculation for functions
# M. Karpack, Spring 2019

# Take assembled stressor data and translate to 0 to 1 metrics
# for individual floodplain functions

library(ggplot2)
library(RColorBrewer)


# set path to Git folder
basepath <- "C:/Users/mnk5/Documents/floodplain_integrity"

# Load csv file of stressor data from "CorrelationAnalysis.R" script output
data.path <- paste(basepath, "/Outputs/Combined_Data.csv", sep="")
all.data <- read.csv(data.path)
data.names <- colnames(all.data)

# Convert HUC-12 from numeric to character
all.data$HUC12 <- as.character(all.data$HUC12)

# boxplots to look at range of data

for (i in 4:ncol(all.data)){
  hist(all.data[,i], main = data.names[i])
  boxplot(all.data[,i], main = data.names[i])
  text(y=fivenum(all.data[,i]), labels = round(fivenum(all.data[,i]), digits = 2), x = 0.75)
  
}


