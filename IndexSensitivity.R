#####################
# Floodplain Integrity Assessment
# Index Sensitivity Analysis
# M. Karpack, Summer 2019

# Test non-linear functions for stressors in calculation

library(ggplot2)
library(RColorBrewer)
library(corrplot)
library(emmeans)
library(psych)
library(gridExtra)
library(ggpubr)


# set path to Git folder
basepath <- "C:/Users/mnk5/Documents/floodplain_integrity"
out.path <- paste(basepath, "/Outputs/", sep="") # for saving outputss

# Load csv file of scaler stressor data from "IndexCalculation.R" script output
data.path <- paste(basepath, "/Outputs/Scaled_Stressors.csv", sep="")
all.data <- read.csv(data.path)
data.names <- colnames(all.data)

# Convert HUC-12 from numeric to character
all.data$HUC12 <- as.character(all.data$HUC12)


#####################
# Calculate functional IFI

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

#############################

# Compute Index for each function with several metrics
IFI.linear <- data.frame(matrix(NA, nrow = nrow(all.data), ncol = length(data.byfunction)))
colnames(IFI.linear) <- c("Floods", "Groundwater", "Sediment", 
                              "Organics/Solutes", "Habitat")
IFI.pos <- IFI.linear
IFI.neg <- IFI.linear

for (i in 1:length(data.byfunction)) {
  stressors <- all.data[,data.byfunction[[i]]]
  IFI.linear[,i] <- 1 - rowMeans(stressors)
  
  IFI.pos[,i] <- rowMeans((stressors - 1)^2)
  IFI.neg[,i] <- rowMeans((-stressors^2 +1))
  
}

# 
# plot(stressors$Agriculture, IFI.neg$Habitat)
plot(stressors$MH20, IFI.pos$Habitat)
# plot(stressors$MH20, IFI.linear$Habitat)

# plot boxplots of index by function
linear.plot <- ggplot(stack(IFI.linear), aes(x = ind, y = values)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0,1)) +
  xlab("") +
  ylab("Integrity Index") +
  ggtitle("Function IFI, linear")


pos.plot <- ggplot(stack(IFI.pos), aes(x = ind, y = values)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0,1)) +
  xlab("") +
  ylab("Integrity Index") +
  ggtitle("Funtion IFI, positive quadratic")


neg.plot <- ggplot(stack(IFI.neg), aes(x = ind, y = values)) + 
  geom_boxplot() +
  scale_y_continuous(limits = c(0,1)) +
  xlab("Floodplain Function") +
  ylab("Integrity Index") +
  ggtitle("Function IFI, negative quadratic")

grid.arrange(linear.plot, pos.plot, neg.plot)



# Compute overall Index of floodplain Integrity
IFI.linear$Overall <- apply(IFI.linear, 1, prod)^(1/5)
IFI.pos$Overall <- apply(IFI.pos, 1, prod)^(1/5)
IFI.neg$Overall <- apply(IFI.neg, 1, prod)^(1/5)

IFI.Overall <- data.frame("Linear"  = IFI.linear$Overall,
                          "Positive Quadratic" = IFI.pos$Overall,
                          "Negative Quadratic" = IFI.neg$Overall)



IFI.plot <- ggplot(na.omit(stack(IFI.Overall)), aes(x = ind, y = values, group = ind)) + 
  scale_y_continuous(limits = c(0,1)) +
  geom_boxplot() +
  xlab("") +
  ylab("Index of Floodplain Integrity") +
  theme_linedraw() +
  theme(text = element_text(size=16), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), legend.position = "none") 

IFI.plot

IFI.hist <- ggplot(na.omit(stack(IFI.Overall)), aes(x = values, group = ind)) + 
  scale_y_continuous() +
  facet_wrap(~ind) +
  geom_histogram(bins=20) +
  xlab("Index of Floodplain Integrity") +
  ylab("Count of Floodplain Units") +
  theme_bw() +
  theme(text = element_text(size=16), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), legend.position = "none") 

IFI.hist


# Test for Significant difference

function.lm <- lm(values ~ ind, data = stack(IFI.Overall))
function.pairwise <- lsmeans(function.lm, pairwise ~ ind)
method.contrasts <- function.pairwise$contrasts
method.contrasts
 # results - all differences are significant





# Export to csv
HUC12 <- all.data$HUC12

# combined.data <- data.frame(HUC12,Function.Index.Scaled,IFI.comb.scaled)
# IFI.outfile <- paste(out.path, "IFI_Scaled_.csv", sep="")
# write.csv(combined.data, file = IFI.outfile)

