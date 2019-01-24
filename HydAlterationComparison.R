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

#############################################
## look at hyd alteration metrics by order

streamfile <- "C:/Users/mnk5/Documents/GIS/DATA/Hyd_Alteration/ByHUC12/attributetable.txt"

df2 <- read.csv(streamfile)
df2 <- df2[, c("pnMH20", "pnFH1", "pnFH6", "pnFH7", "pnDH1", "pnDH2","pnDH3","pnDH4","pnDH5","pnDH15",'StrmOrder')]

# Create boxlpots for each of 10 Hyd Alt metrics for stream segments by Stream Order
boxplots_SO <- list()
for (i in 1:10){
  metric <- names[i]
  boxplots_SO[[i]] <-  boxplot(df2[,i] ~ StrmOrder, data = df2, 
                            xlab ='Stream Order', 
                            ylab = 'Hydologic Alteration',
                            main = sprintf('%s by Stream Segment', metric))
}

#############################################
# Repeat for mean Hyd Alt by Stream order for FP

meandata <- df[,c(20:29, 54)]
# set -999 values from GIS to NA 
meandata[meandata == -999] <- NA

## Create boxlpots for each of 10 Hyd Alt metrics for FP segments by Stream Order
boxplots_FP_SO <- list()
for (i in 1:10){
  metric <- names[i]
  boxplots_FP_SO[[i]] <-  boxplot(meandata[,i] ~ StrmOrder, data = meandata, 
                               xlab ='Stream Order', 
                               ylab = 'Hydologic Alteration',
                               main = sprintf('%s by FP Segment', metric))
}
