#####################
# Floodplain Integrity Assessment
# Index Analysis
# M. Karpack, Spring 2019

# Analysis IFI data by stream order, floodplain area, ecoregion and city/not city

library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(gridExtra)
library(emmeans)
library(scales)


# set path to Git folder
basepath <- "C:/Users/mnk5/Documents/floodplain_integrity"
out.path <- paste(basepath, "/Outputs/", sep="") # for saving outputss

# Load csv file of stressor data with Ecoregion and city as exported from GIS
data.path <- paste(basepath, "/RawData/IFI_Ecoregion_cities.csv", sep="")
all.data <- read.csv(data.path)
col.names <- colnames(all.data)

# Histograms of IFI results

# find columns to plot
functions <- c("Floods", "Groundwater", "Sediment", "Organics_Solutes", "Habitat")
func.IFI <- all.data[, functions]
#colnames(func.IFI)[which(names(func.IFI) == "IFI_geomean")] <- "Overall IFI"

p <- ggplot(gather(func.IFI), aes(value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~key, scales = 'free_y') +
  xlab("Index of Floodplain Integrity") +
  ylab("Count") +
  theme(text = element_text(size=20))
p

b <- ggplot(all.data, aes(x=IFI_geomean)) +
  geom_histogram(bins = 20) +
  xlab("Overall Index of Floodplain Integrity" ) +
  ylab("Count") +
  theme(text = element_text(size=20))
b

#######################################
# Plot by floodplain area
a1 <- ggplot(all.data, aes(x = FP_Areakm2, y = Floods)) + 
    geom_point() +
    scale_x_continuous() +
    scale_y_continuous() +
    xlab("") +
    ylab("Floods IFI")

a2 <- ggplot(all.data, aes(x = FP_Areakm2, y = Groundwater)) + 
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous() +
  xlab("") +
  ylab("Groundwater IFI")

a3 <- ggplot(all.data, aes(x = FP_Areakm2, y = Sediment)) + 
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous() +
  xlab(bquote("Floodplain unit area," ~km^2)) +
  ylab("Sediment IFI")

a4 <- ggplot(all.data, aes(x = FP_Areakm2, y = Organics_Solutes)) + 
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous() +
  xlab(bquote("Floodplain unit area," ~km^2)) +
  ylab("Organics and Solutes IFI")

a5 <- ggplot(all.data, aes(x = FP_Areakm2, y = Habitat)) + 
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous() +
  xlab(bquote("Floodplain unit area," ~km^2)) +
  ylab("Habitat IFI")

grid.arrange(a1, a2, a5, a4, a3, nrow = 2)

# Linear relationship between IFI as function of Area
area.lm <- lm(all.data$IFI_geomean ~ log10(all.data$FP_Areakm2))
R2 <- summary(area.lm)$r.squared

a6 <- ggplot(all.data, aes(x = FP_Areakm2, y = IFI_geomean)) + 
  geom_point() +
  scale_x_log10(labels = trans_format('log10',math_format(10^.x))) +
  scale_y_continuous() +
  xlab(bquote("\nFloodplain unit area," ~km^2)) +
  ylab("Overall IFI\n") +
  theme(text = element_text(size=20))
a6



####################
# IFI by stream order
all.data[all.data == -999] <- NA
count.data <- as.data.frame((table(all.data$StrmOrder)))
names(count.data)[1] = 'StrmOrder'
count.data$Freq <- paste(" N =", as.character(count.data$Freq), sep = " ")



SO <- ggplot(all.data, aes(StrmOrder, IFI_geomean, group = StrmOrder)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "\nStream Order", breaks = seq(1:8)) + 
  ylab("Overall IFI\n") +
  theme(text = element_text(size=20), panel.grid.major.x = element_blank()) +
  geom_text(data = count.data, aes(StrmOrder, y = 1.0, label = Freq), nudge_y = 0.05, size = 5)
SO

###################
# IFI by ecoregion

all.data$ECO_name <- as.factor(all.data$ECO_name)

# get counts for label
count.data <- as.data.frame((table(all.data$ECO_name)))
names(count.data)[1] = 'ECO_name'
count.data$Freq <- paste(" N =", as.character(count.data$Freq), sep = " ")

# Compute ANOVA
eco.aov <- aov(IFI_geomean ~ ECO_name, data = all.data)
summary(eco.aov)
# result: they are significantly different

eco.lm <- lm(IFI_geomean ~ ECO_name, data = all.data)
eco.pairwise <- lsmeans(eco.lm, pairwise ~ ECO_name)
method.contrasts <- eco.pairwise$contrasts
method.contrasts
# Results - three statistically significant groups

# add column for coloring in ggplot
all.data$sig.group <- NA
all.data$sig.group <- ifelse(all.data$ECO_name %in% c('ANP', 'COP', 'SRO'), 'A',
                             ifelse(all.data$ECO_name %in% c('HPL', 'WYB'), 'B', 'C'))

ECO <- ggplot(all.data, aes(ECO_name, IFI_geomean)) +
  geom_boxplot(aes(ECO_name, IFI_geomean, fill = sig.group)) +
  scale_fill_manual(values = c("#217463","#32ae95", "#9ce3d4")) +
  scale_x_discrete(name = "\nEPA Ecoregion, Level 3") + 
  ylab("Overall IFI\n") +
  theme(text = element_text(size=20), panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_text(data = count.data, aes(ECO_name, y = 1.0, label = Freq), nudge_y = 0.05, size = 5)
ECO

###################
# IFI by city or not

all.data$In_City <- as.factor(all.data$In_City)

# get counts for label
count.data <- as.data.frame((table(all.data$In_City)))
names(count.data)[1] = 'In_City'
count.data$Freq <- paste(" N =", as.character(count.data$Freq), sep = " ")

# T test for difference
t.test(IFI_geomean ~ In_City, data = all.data)
# result: means are not equal

City.plot <- ggplot(all.data, aes(In_City, IFI_geomean)) +
  geom_boxplot(aes(In_City, IFI_geomean)) +
  scale_x_discrete(name = "", labels = c("Not in city", "In city")) + 
  ylab("Overall IFI\n") +
  theme(text = element_text(size=20), panel.grid.major.x = element_blank(),
        legend.position = "none") +
  geom_text(data = count.data, aes(In_City, y = 1.0, label = Freq), nudge_y = 0.05, size = 5)
City.plot
