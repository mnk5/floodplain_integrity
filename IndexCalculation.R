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

#####################
# Get only final stressors and HUC-12 Identifier into df
HUC12 <- all.data$HUC12
keep.columns <- c("Agriculture", "Buildings", "Ditches", "Developed", "ForestLoss",
                  "Impervious", "LeveedArea", "NonNativeVeg", "Roads_Rail","Wells", "MH20")
stressors <- all.data[, keep.columns]

# adjust stressors that are not 0 to 1 
stressors$Impervious <- stressors$Impervious/100 # convert percent to decimal

# Scale count and line denisty by max value observed
stressors$Ditches <- stressors$Ditches/max(stressors$Ditches)
stressors$Roads_Rail <- stressors$Roads_Rail/max(stressors$Roads_Rail)
stressors$Wells <- stressors$Wells/max(stressors$Wells)

# Scale buildings to max building density in CO
stressors$Buildings <- stressors$Buildings/max(stressors$Buildings)

# Compare all measures
boxplot(stressors, use.cols = TRUE)


# Make neagtive 
stressors.neg <- 1-stressors
boxplot(stressors.neg, use.cols = T)

#####################
# Calculate functions as average of stressors

# choose datasets by function

# Flood reduction
FR.stressors <- c("Buildings", "Roads_Rail", "ForestLoss", "Developed", "Wells", "LeveedArea")
# Groundwater regulation
GW.stressors <- c("Impervious", "Ditches", "Agriculture", "ForestLoss")
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

colnames(Function.Index) <- c("Floods", "Groudwater", "Sediment", 
                              "Organic/Solutes", "Habitat")


# plot boxplot of index by function
function.plot <- ggplot(stack(Function.Index), aes(x = ind, y = values)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0,1)) +
  xlab("Floodplain Function") +
  ylab("Integrity Index") +
  ggtitle("Index of Floodplain Integrity by Function")
function.plot

# Compute overall Index of floodplain Integrity


# Compute index using percentiles






