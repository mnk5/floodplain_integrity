#####################
# Floodplain Integrity Assessment
# Index Calculation for functions
# M. Karpack, Spring 2019

# Take assembled stressor data and translate to 0 to 1 metrics
# for individual floodplain functions

library(ggplot2)
library(RColorBrewer)
library(corrplot)
library(emmeans)
library(psych)


# set path to Git folder
basepath <- "C:/Users/mnk5/Documents/floodplain_integrity"
out.path <- paste(basepath, "/Outputs/", sep="") # for saving outputss

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

#####################
# Get only final stressors and HUC-12 Identifier into df

keep.columns <- c("Agriculture", "Buildings", "Ditches", "Developed", "ForestLoss",
                  "Impervious", "LeveedArea", "NonNativeVeg", "Roads_Rail","Wells", "MH20")
stressors <- all.data[, keep.columns]

# adjust stressors that are not 0 to 1 
stressors$Impervious <- stressors$Impervious/100 # convert percent to decimal

# Scale count and line denisty by max value observed
stressors$Ditches <- stressors$Ditches/max(stressors$Ditches)
stressors$Roads_Rail <- stressors$Roads_Rail/max(stressors$Roads_Rail)
stressors$Wells <- stressors$Wells/max(stressors$Wells)


# Compare all measures
boxplot(stressors, use.cols = TRUE, ylab = 'Stressor Density')


# Scale buildings to max building density in CO
stressors$Buildings <- stressors$Buildings/max(stressors$Buildings)


# Make neagtive 
stressors.neg <- 1-stressors
# boxplot(stressors.neg, use.cols = T)

#####################
# Calculate functions as average of stressors

# choose datasets by function

# Flood reduction
FR.stressors <- c("Buildings", "Roads_Rail", "ForestLoss", "Developed", "LeveedArea")
# Groundwater regulation
GW.stressors <- c("Impervious", "Ditches", "Agriculture", "ForestLoss", "Wells")
# Sediment Flux
SF.stressors <- c("MH20", "Agriculture", "Roads_Rail", "ForestLoss")
# Organics and Solute regulation
OS.stressors <- c("MH20", "ForestLoss", "Impervious") # Roads_Rail not included b/c of high correlation
# Habitat provisioning 
HP.stressors <- c("Roads_Rail", "MH20", "NonNativeVeg", "Developed", "Agriculture", "ForestLoss")

data.byfunction <- list(FR.stressors, GW.stressors, SF.stressors, OS.stressors, HP.stressors)


# Compute Index as average of stressors for each function
Function.Index <- data.frame(matrix(NA, nrow = nrow(stressors.neg), ncol = length(data.byfunction)))

for (i in 1:length(data.byfunction)) {
  Function.Index[,i] <- rowMeans(stressors.neg[,data.byfunction[[i]]])
}

colnames(Function.Index) <- c("Floods", "Groundwater", "Sediment", 
                              "Organics/Solutes", "Habitat")


# plot boxplot of index by function
function.plot <- ggplot(stack(Function.Index), aes(x = ind, y = values)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0,1)) +
  xlab("Floodplain Function") +
  ylab("Integrity Index") +
  ggtitle("Index of Floodplain Integrity by Function")
function.plot

# plot correlation of Indices

function.cor <- cor(Function.Index, use = "pairwise.complete.obs")

out.graph <- paste(out.path, "IFI_Correlation.jpg", sep="")
jpeg(out.graph, width = 2000, height = 2000, units = "px")
corrplot(function.cor, type = "upper", method = "circle", tl.col="black", tl.srt=45,
         tl.cex= 4.5, diag=FALSE, addCoef.col = "#bbbcc1", number.cex = 4, cl.pos ="n")
dev.off()



# Compute overall Index of floodplain Integrity
IFI <- data.frame(IFI.geomean = apply(Function.Index, 1, prod)^(1/5))
IFI.product <- data.frame(IFI.prod = apply(Function.Index, 1, prod))
IFI.comb <- data.frame(IFI,IFI.product)

IFI.plot <- ggplot(stack(IFI.comb), aes(x = ind, y = values)) + 
  scale_y_continuous(limits = c(0,1)) +
  geom_boxplot() +
  xlab("") +
  ylab("Index of Floodplain Integrity") 

IFI.plot

###################################
# GEO MEAN

# Compute index using geometric mean
Function.Index.Geo <- data.frame(matrix(NA, nrow = nrow(stressors.neg), ncol = length(data.byfunction)))

for (i in 1:length(data.byfunction)) {
  Function.Index.Geo[,i] <- apply(stressors.neg[,data.byfunction[[i]]], 1, function(x) geometric.mean(x))
}

colnames(Function.Index.Geo) <- c("Floods", "Groundwater", "Sediment", 
                              "Organics/Solutes", "Habitat")



# plot boxplot of index by function with geomean
function.plot.geo <- ggplot(stack(Function.Index.Geo), aes(x = ind, y = values)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0,1)) +
  xlab("Floodplain Function") +
  ylab("Integrity Index") +
  ggtitle("Index of Floodplain Integrity by Function, Geometric Mean")
function.plot.geo

# Compute overall Index of floodplain Integrity with Geo Mean
IFI.geo <- data.frame(IFI.geomean = apply(Function.Index.Geo, 1, prod)^(1/5))
IFI.product.geo <- data.frame(IFI.prod = apply(Function.Index.Geo, 1, prod))
IFI.comb.geo <- data.frame(IFI.geo,IFI.product.geo)

IFI.plot.geo <- ggplot(stack(IFI.comb.geo), aes(x = ind, y = values)) + 
  scale_y_continuous(limits = c(0,1)) +
  geom_boxplot() +
  xlab("") +
  ylab("Index of Floodplain Integrity, Function Geometric Mean") 

IFI.plot.geo

###########################
# Cap stressors at 75th percentile

stressors.scaled <- stressors # initialize vector
percent.capped <- list()

# loop over stressors
for (i in 1:ncol(stressors)) {
  # find 90th percentile
  limit <- quantile(stressors[,i], probs = 0.90)
  
  # for non-zero 90th percentiles, compute as relative to 90th percentile
  if (limit != 0) {
    stressors.scaled[,i] <- stressors.scaled[,i]/limit
    
    # Count percentage of data being capped to one
    percent.capped[[i]] <- sum(stressors.scaled[,i]>1)/nrow(stressors.scaled)
    
    # set values over 90th percentile to 1
    stressors.scaled[,i][stressors.scaled[,i]>1] <- 1
    
  } else {
    # if 90th percentile is 0, scale relative to max value
    stressors.scaled[,i] <- stressors.scaled[,i]/max(stressors.scaled[,i])
    
    percent.capped[[i]] <- 0
  }
    
}

## Boxplot scaled stressors
boxplot(stressors.scaled, use.cols = TRUE, ylab = 'Scaled Stressor Density')



# Compute  Index as average of stressors scaled for each function
Function.Index.Scaled <- data.frame(matrix(NA, nrow = nrow(stressors.scaled), ncol = length(data.byfunction)))

for (i in 1:length(data.byfunction)) {
  Function.Index.Scaled[,i] <- 1 - rowMeans(stressors.scaled[,data.byfunction[[i]]])
}

colnames(Function.Index.Scaled) <- c("Floods", "Groundwater", "Sediment", 
                              "Organics/Solutes", "Habitat")


# plot boxplot of index by function
function.plot.scaled <- ggplot(stack(Function.Index.Scaled), aes(x = ind, y = values)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0,1)) +
  xlab("Floodplain Function") +
  ylab("Integrity Index") +
  ggtitle("Index of Floodplain Integrity by Function, Scaled Stressors")
function.plot.scaled

# plot correlation of Indices

function.scaled.cor <- cor(Function.Index.Scaled, use = "pairwise.complete.obs")

out.graph <- paste(out.path, "IFI_Scaled_Correlation.jpg", sep="")
jpeg(out.graph, width = 2000, height = 2000, units = "px")
corrplot(function.scaled.cor, type = "upper", method = "circle", tl.col="black", tl.srt=45,
         tl.cex= 4.5, diag=FALSE, addCoef.col = "#bbbcc1", number.cex = 4, cl.pos ="n")
dev.off()



# Compute overall Index of floodplain Integrity
IFI.scaled <- data.frame(IFI.geomean = apply(Function.Index.Scaled, 1, function(x) geometric.mean(x)))
IFI.product.scaled <- data.frame(IFI.prod = apply(Function.Index.Scaled, 1, prod))
IFI.comb.scaled <- data.frame(IFI.scaled,IFI.product.scaled)

IFI.scaled.plot <- ggplot(stack(IFI.comb.scaled), aes(x = ind, y = values)) + 
  scale_y_continuous(limits = c(0,1)) +
  geom_boxplot() +
  xlab("") +
  ylab("Index of Floodplain Integrity, scaled stressors") 

IFI.scaled.plot

# Export to csv
HUC12 <- all.data$HUC12

scaled.stressors <- data.frame(HUC12, stressors.scaled)
Stressors.outfile <- paste(out.path, "Scaled_Stressors.csv", sep="")
write.csv(scaled.stressors, file = Stressors.outfile)

combined.data <- data.frame(HUC12,Function.Index.Scaled,IFI.comb.scaled)
IFI.outfile <- paste(out.path, "IFI_Scaled.csv", sep="")
write.csv(combined.data, file = IFI.outfile)

