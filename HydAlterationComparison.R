# -*- coding: utf-8 -*-

library(ggplot2)



## load in table with weighted hydrologic alteration values

attributefile <- "C:/Users/mnk5/Documents/GIS/DATA/Hyd_Alteration/ByHUC12/Combined_weighted_HydAlt.csv"

df <- read.csv(attributefile)

## Get data organized by metric and weighting scheme

names <- c("pnMH20", "pnFH1", "pnFH6", "pnFH7", "pnDH1", "pnDH2","pnDH3","pnDH4","pnDH5","pnDH15" )

HydAlt <- list()
for (n in 7:16){
  data <- df[,c(n, n+13, n+25, n+37)]
  colnames(data) <- c("Max","Mean","LengthWeight","OrderWeight")
  # set -999 values from GIS to NA 
  data[data == -999] <- NA
  
  name <- names[n-6]
  HydAlt[[name]] <- data
  
}


## Create boxlpots of 4 methods for each of 10 Hyd Alt metrics
boxplots <- list()
for (i in 1:length(HydAlt)){
  boxplots[[i]] <-  boxplot(HydAlt[[i]], 
      xlab ='Aggregating Method', 
      ylab = 'Hydologic Alteration',
      main = names[i])
 }
