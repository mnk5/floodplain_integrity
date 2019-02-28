################
# Floodplain Integrity Assessment 
# Hydrologic Alteration Metric Comparison
# M. Karpack, Spring 2019

# Comparison of Hydrologic Alteration metrics provided by R. McManamay
# at the stream level and aggregated to the HUC-12 unit using several different methods

library(ggplot2)
library(lsmeans)


## load in table with weighted hydrologic alteration values

attributefile <- "C:/Users/mnk5/Documents/GIS/DATA/Hyd_Alteration/ByHUC12/Combined_weighted_HydAlt.csv"

df <- read.csv(attributefile)


## Get data organized by metric and weighting scheme

names <- c("pnMH20", "pnFH1", "pnFH6", "pnFH7", "pnDH1", "pnDH2","pnDH3","pnDH4","pnDH5","pnDH15" )

HydAlt <- list()
for (n in 6:15){
  data <- df[,c(n, n+12, n+23, n+34, n+48)]
  colnames(data) <- c("Max","Mean","LengthWeight","OrderWeight","MaxOrderMean")
  # set -999 values from GIS to NA 
  data[data == -999] <- NA
  
  name <- names[n-5]
  HydAlt[[name]] <- data
  
}


## Create boxlpots of 4 methods for each of 10 Hyd Alt metrics
par(mar=c(2.1,4.1,2.1,2.1),mfrow = c(5,2))
for (i in 1:length(HydAlt)){
  boxplot(HydAlt[[i]], 
      xlab ='Aggregating Method', 
      ylab = 'Hydologic Alteration',
      main = names[i])
}


## Test for statistically sig difference

method.lm <- lapply(HydAlt, function(x) lm(values ~ ind, data = stack(x)))
# results <-lapply(method.aov, function(x) summary(x))
method.pairwise <- lapply(method.lm, function(x) lsmeans(x, pairwise ~ ind))
method.contrasts <- lapply(method.pairwise, function(x) x$contrasts)

#############################################
## look at hyd alteration metrics by order

streamfile <- "C:/Users/mnk5/Documents/GIS/DATA/Hyd_Alteration/ByHUC12/attributetable.txt"

df2 <- read.csv(streamfile)
df2 <- df2[, c("pnMH20", "pnFH1", "pnFH6", "pnFH7", "pnDH1", "pnDH2","pnDH3","pnDH4","pnDH5","pnDH15",'StrmOrder')]

# Create boxlpots for each of 10 Hyd Alt metrics for stream segments by Stream Order
par(mar=c(2.1,4.1,2.1,2.1),mfrow = c(5,2))
for (i in 1:10){
  metric <- names[i]
  boxplot(df2[,i] ~ StrmOrder, data = df2, 
                            xlab ='Stream Order', 
                            ylab = 'Hydologic Alteration',
                            main = sprintf('%s by Stream Segment', metric))
}

#############################################
# Repeat for mean Hyd Alt by Stream order for FP

meandata <- df[,c(18:27, 50)]
# set -999 values from GIS to NA 
meandata[meandata == -999] <- NA

## Create boxlpots for each of 10 Hyd Alt metrics for FP segments by Stream Order
par(mar=c(2.1,4.1,2.1,2.1),mfrow = c(5,2))
for (i in 1:10){
  metric <- names[i]
  boxplot(meandata[,i] ~ StrmOrder, data = meandata, 
                               xlab ='Stream Order', 
                               ylab = 'Hydologic Alteration',
                               main = sprintf('Mean %s by FP Segment', metric))
}

#############################################
# Repeat for max Hyd Alt by Stream order for FP

maxdata <- df[,c(6:15, 50)]
# set -999 values from GIS to NA 
maxdata[maxdata == -999] <- NA

## Create boxlpots for each of 10 Hyd Alt metrics for FP segments by Stream Order

par(mar=c(2.1,4.1,2.1,2.1),mfrow = c(5,2))
for (i in 1:10){
  
  metric <- names[i]
  boxplot(maxdata[,i] ~ StrmOrder, data = maxdata, 
                                  xlab ='Stream Order', 
                                  ylab = 'Hydologic Alteration',
                                  main = sprintf('Max %s by FP Segment', metric))
  
}


#############################################
# Repeat for mean of max order streams only Hyd Alt by Stream order for FP

maxSOdata <- df[,c(54:63, 50)]
# set -999 values from GIS to NA 
maxSOdata[maxSOdata == -999] <- NA

## Create boxlpots for each of 10 Hyd Alt metrics for FP segments by Stream Order

par(mar=c(2.1,4.1,2.1,2.1),mfrow = c(5,2))
for (i in 1:10){
  
  metric <- names[i]
  boxplot(maxSOdata[,i] ~ StrmOrder, data = maxSOdata, 
          xlab ='Stream Order', 
          ylab = 'Hydologic Alteration',
          main = sprintf('Mean of Max Order Streams %s by FP Segment', metric))
  
}
