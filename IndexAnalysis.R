#####################
# Floodplain Integrity Assessment
# Index Analysis
# M. Karpack, Spring 2019

# Analysis IFI data by stream order, floodplain area, ecoregion and city/not city,
# Comparison to IFI and identification of most impacted function. 

library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(gridExtra)
library(emmeans)
library(scales)
library(dplyr)
library(data.table)


# set path to Git folder
basepath <- "C:/Users/mnk5/Documents/floodplain_integrity"
out.path <- paste(basepath, "/Outputs/", sep="") # for saving outputss

# Load csv file of stressor data with Ecoregion and city as exported from GIS
data.path <- paste(basepath, "/RawData/IFI_Ecoregion_cities.csv", sep="")
all.data <- read.csv(data.path)
colnames(all.data)[which(names(all.data) == "IFI_geomea")] <- "IFI_geomean"

col.names <- colnames(all.data)

# Histograms of IFI results

# find columns to plot
functions <- c("Floods", "Groundwate", "Sediment", "Organics_S", "Habitat")
func.IFI <- all.data[, functions]
colnames(func.IFI) <- c("Floods", "Groundwater", "Sediment", "Organics_Solutes", "Habitat")

colnames(func.IFI)[which(names(func.IFI) == "IFI_geomean")] <- "Overall IFI"

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
# Plot bar graphs of function IFI by area

# Arrange data into groups of 0.05 bins
breaks <- seq(0.00, 1, 0.05)
breaks[1] <- -Inf

by.area<- data.frame(Area_km2 = all.data$FP_Areakm2)

by.area$Floods <- cut(func.IFI$Floods, breaks)
by.area$Groundwater <- cut(func.IFI$Groundwater, breaks)
by.area$Sediment <- cut(func.IFI$Sediment, breaks)
by.area$Organics_Solutes <- cut(func.IFI$Organics_Solutes, breaks)
by.area$Habitat <- cut(func.IFI$Habitat, breaks)
by.area$Overall_IFI <- cut(all.data$IFI_geomean, breaks)

Flood.sum <- by.area %>%
  group_by(Floods) %>%
  summarise(area = sum(Area_km2))

GW.sum <- by.area %>%
  group_by(Groundwater) %>%
  summarise(area = sum(Area_km2))

Sed.sum <- by.area %>%
  group_by(Sediment) %>%
  summarise(area = sum(Area_km2))

OS.sum <- by.area %>%
  group_by(Organics_Solutes) %>%
  summarise(area = sum(Area_km2))

Habitat.sum <- by.area %>%
  group_by(Habitat) %>%
  summarise(area = sum(Area_km2))

Overall.sum <- by.area %>%
  group_by(Overall_IFI) %>%
  summarise(area = sum(Area_km2))


# THIS IS ALL MANUAL AND WILL NEED TO CHANGE IF DATA CHANGES
area.sum <- data.frame(breaks = OS.sum$Organics_Solutes)
area.sum$Floods <- NA
area.sum[7:20,2] <- Flood.sum$area
area.sum$Groundwater <- NA
area.sum[2:20,3] <- GW.sum$area
area.sum$Sediment <- NA
area.sum[2:20,4] <- Sed.sum$area
area.sum$Organics_Solutes <- OS.sum$area
area.sum$Habitat <- NA
area.sum[4:20,6] <- Habitat.sum$area
area.sum$Overall <- Overall.sum$area

area.sum$breaks <- as.numeric(area.sum$breaks)

# Graph with facet wrap "histograms"

area.df <- melt(area.sum, id = 1, measure = 2:7)
levels(area.df$variable) = c("Flood Reduction", "Groundwater Storage", "Sediment Regulation",
                          "Organics/Solutes Regulation", "Habitat Provision", "Overall IFI")

area.barplot <- ggplot(data = area.df, aes(x = breaks, y = value)) +
  geom_bar(stat = "identity", width = 1,  position = position_nudge(x = -0.5)) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2), 
                     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7","0.8", "0.9", "1.0")) +
  facet_wrap(~ variable, ncol = 2) +
  labs(x = "IFI Value", y = bquote("Total floodplain area, " ~km^2)) +
  theme_minimal(base_size = 12)
area.barplot


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



############################
# IFI by stream order
all.data[all.data == -999] <- NA
count.data <- as.data.frame((table(all.data$StrmOrder)))
names(count.data)[1] = 'StrmOrder'
count.data$Freq <- paste(" N =", as.character(count.data$Freq), sep = " ")



SO <- ggplot(all.data, aes(StrmOrder, IFI_geomean, group = StrmOrder)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Stream Order", breaks = seq(1:8)) + 
  ylab("Overall IFI\n") +
  theme_linedraw() +
  theme(text = element_text(size=24), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), plot.background = element_rect(fill = "#e9ecee")) +
  geom_text(data = count.data, aes(StrmOrder, y = 1.0, label = Freq), nudge_y = 0.05, size = 5)
SO

############################
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

#############################
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
  scale_x_discrete(name = "", labels = c("Rural", "City")) + 
  ylab("Overall IFI\n") +
  theme_linedraw() +
  theme(text = element_text(size=24), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), legend.position = "none",
        plot.background = element_rect(fill = "#e9ecee")) +
  geom_text(data = count.data, aes(In_City, y = 1.0, label = Freq), nudge_y = 0.05, size = 5)
City.plot


##############################
# IFI vs ICI comparison

# read export from GIS
ICI <- read.csv(paste(basepath, "/RawData/ICI_byHUC12.csv", sep=""))

IFI <- func.IFI
IFI$Overall <- all.data$IFI_geomea
IFI$HUC12 <- all.data$HUC12

# Join ICI and IFI based on HUC12
ICI.comp <- merge(IFI, ICI, by.x = "HUC12", by.y = "IFI_HUC12", all.x = TRUE)
names(ICI.comp)[names(ICI.comp)== 'MEAN_ICI_I'] <- "ICI"

# Scatter plot of ICI vs IFI
ICI.plot <- ggplot(ICI.comp, aes(x = ICI, y = Overall)) + geom_point() +
  xlim(0,1) + ylim(0,1) +
  # scale_x_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  # scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  coord_equal() +
  xlab("Index of Catchment Integrity") +
  ylab("Overall Index of Floodplain Integrity") +
  theme_bw()
ICI.plot

# fit linear model
ICI.lm <- lm(data = ICI.comp, Overall ~ ICI)
R2.ICI <- summary(ICI.lm)$r.squared

# Look at distribution of ICI values (very few over 0.8)
# hist(ICI.comp$ICI, xlim = c(0,1))
