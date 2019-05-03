#####################
# Floodplain Integrity Assessment
# Index Analysis
# M. Karpack, Spring 2019

# Analysis IFI data by stream order, floodplain area, ecoregion and city/not city,
# Comparison to IFI and identification of most impacted function. 

library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(grid)
library(gridExtra)
library(emmeans)
library(scales)
library(dplyr)
library(data.table)
library(stringr)
library(psych)
library(ggpubr)


# set path to Git folder
basepath <- "C:/Users/mnk5/Documents/floodplain_integrity"
out.path <- paste(basepath, "/Outputs/", sep="") # for saving outputss

# Load csv file of stressor data with Ecoregion and city as exported from GIS
data.path <- paste(basepath, "/RawData/IFI_Ecoregion_cities.csv", sep="")
all.data <- read.csv(data.path)
colnames(all.data)[which(names(all.data) == "IFI_geomea")] <- "IFI_geomean"

col.names <- colnames(all.data)

# find columns to plot
functions <- c("Floods", "Groundwate", "Sediment", "Organics_S", "Habitat")
func.IFI <- all.data[, functions]
colnames(func.IFI) <- c("Floods", "Groundwater", "Sediment", "Organics_Solutes", "Habitat")

# colnames(func.IFI)[which(names(func.IFI) == "IFI_geomean")] <- "Overall IFI"

######################################
# general statistics about overall IFI
IFI.stats <- describe(all.data$IFI_geomean)
IFI.stats

IFI.func.stats <- apply(func.IFI, 2, function(x) describe(x))
IFI.func.stats

######################################
# Histograms of IFI results

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
area.sum[4:20,2] <- Flood.sum$area
area.sum$Groundwater <- NA
area.sum[6:20,3] <- GW.sum$area
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

area.barplot <- ggplot(data = na.omit(area.df), aes(x = breaks, y = value)) +
  geom_bar(stat = "identity", width = 1,  position = position_nudge(x = -0.5), fill = "grey27") +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 4), 
                     labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0")) +
                     # labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7","0.8", "0.9", "1.0")) +
  facet_wrap(~ variable, ncol = 3) +
  labs(x = "IFI Value", y = bquote("Total floodplain area, " ~km^2)) +
  theme_bw(base_size = 16) +
  theme(strip.background =element_rect(fill="grey93"))
area.barplot


#######################################
# Plot by floodplain area
a1 <- ggplot(all.data, aes(x = FP_Areakm2, y = Floods)) + 
    geom_point() +
    scale_x_continuous() +
    scale_y_continuous() +
    xlab("") +
    ylab("Floods IFI")

a2 <- ggplot(all.data, aes(x = FP_Areakm2, y = Groundwate)) + 
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

a4 <- ggplot(all.data, aes(x = FP_Areakm2, y = Organics_S)) + 
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
  xlab(bquote("Floodplain unit area," ~km^2)) +
  ylab("Overall IFI") +
  theme_bw() +
  theme(text = element_text(size=20))
a6



############################
# IFI by stream order
all.data[all.data == -999] <- NA
count.data <- as.data.frame((table(all.data$StrmOrder)))
names(count.data)[1] = 'StrmOrder'
count.data$Freq <- paste(" N =", as.character(count.data$Freq), sep = " ")

SO_comparisons <- list( c("1","2"), c("2","3"), c("3", "4"), c("4","5"), c("5","6"), c("6", "7"))

SO <- ggplot(na.omit(all.data), aes(StrmOrder, IFI_geomean, group = StrmOrder)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Stream Order", breaks = seq(1:8)) + 
  ylab("Overall IFI") +
  theme_linedraw() +
  theme(text = element_text(size=16), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  geom_text(data = count.data, aes(StrmOrder, y = 1.0, label = Freq), nudge_y = 0.05, size = 4) +
  labs(tag = "c)") +
  stat_compare_means(comparisons = SO_comparisons, label = "p.signif", method = "t.test", 
                     label.y = seq(1.2, 1.4, 0.2/length(my_comparisons)))
  
SO

# Test for significant difference
all.data$StrmOrder <- as.factor(all.data$StrmOrder)
SO.lm <- lm(IFI_geomean ~ StrmOrder, data = all.data)
SO.pairwise <- lsmeans(SO.lm, pairwise ~ StrmOrder)
method.contrasts <- SO.pairwise$contrasts
method.contrasts
# Results - 1-3 not sig different, 4-6 all sig different fromnext larger, 6-7 not sig different, 8 is weird. 

############################
# IFI by ecoregion

all.data$ECO_name <- as.factor(all.data$ECO_name)

# get counts for label
count.data.ECO <- as.data.frame((table(all.data$ECO_name)))
names(count.data.ECO)[1] = 'ECO_name'
count.data.ECO$Freq <- paste(" N =", as.character(count.data.ECO$Freq), sep = " ")

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
  geom_text(data = count.data.ECO, aes(ECO_name, y = 1.0, label = Freq), nudge_y = 0.05, size = 5)
ECO

#############################
# IFI by city or not

all.data$In_City <- as.factor(all.data$In_City)

# get counts for label
count.data.city <- as.data.frame((table(all.data$In_City)))
names(count.data.city)[1] = 'In_City'
count.data.city$Freq <- paste(" N =", as.character(count.data.city$Freq), sep = " ")

# T test for difference
t.test(IFI_geomean ~ In_City, data = all.data)
# result: means are not equal

City.plot <- ggplot(all.data, aes(In_City, IFI_geomean)) +
  geom_boxplot(aes(In_City, IFI_geomean)) +
  scale_x_discrete(name = "", labels = c("Rural", "Urban")) + 
  ylab("Overall IFI") +
  theme_linedraw() +
  theme(text = element_text(size=16), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), legend.position = "none") +
  geom_text(data = count.data.city, aes(In_City, y = 1.0, label = Freq), nudge_y = 0.05, size = 4) +
  labs(tag = "b)")
City.plot

##############################
# IFI by Physiographic region

# Convert to factor and re-order to match geography
all.data$PhysioReg <- factor(all.data$PhysioReg,
                                levels = c("Intermontane Plateaus", "Rocky Mountain System", "Interior Plains"),
                                ordered = TRUE)

# get counts for label
count.data.phys <- as.data.frame((table(all.data$PhysioReg)))
names(count.data.phys)[1] = 'PhysioReg'
count.data.phys$Freq <- paste(" N =", as.character(count.data.phys$Freq), sep = " ")

# Test for differences 
phys.lm <- lm(IFI_geomean ~ PhysioReg, data = all.data)
phys.pairwise <- lsmeans(phys.lm, pairwise ~ PhysioReg)
method.contrasts <- phys.pairwise$contrasts
method.contrasts
# results: Interior plains different from both Rocky mtn and Plateaus

Phys_comparisons <- list(c("Intermontane Plateaus", "Rocky Mountain System"), 
                         c("Rocky Mountain System", "Interior Plains"),
                         c("Intermontane Plateaus", "Interior Plains"))

#Plot boxplots
PHYS <- ggplot(all.data, aes(PhysioReg, IFI_geomean)) +
  geom_boxplot(aes(PhysioReg, IFI_geomean)) +
  scale_x_discrete(name = NULL, labels = function(x) str_wrap(x, width = 20)) + 
  ylab("Overall IFI") +
  theme_linedraw() +
  theme(text = element_text(size=16), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), legend.position = "none") +
  geom_text(data = count.data.phys, aes(PhysioReg, y = 1.0, label = Freq), nudge_y = 0.05, size = 4) +
  labs(tag = "a)") +
  stat_compare_means(comparisons = Phys_comparisons, label = "p.signif", 
                     label.y = c(1.2, 1.3, 1.4))
PHYS

# Stream Order by physiographic region
phys.so <- ggplot(na.omit(all.data), aes(StrmOrder)) +
  geom_bar() +
  facet_wrap(~ PhysioReg) +
  xlab("Stream Order") +
  ylab("Number of Floodplain Units") +
  theme_bw() +
  theme(text = element_text(size = 16))
phys.so 

##############################
# Combine boxplots to make figure

# divide grid arrange by 5
Figure <- grid.arrange(PHYS, City.plot, SO,
                       layout_matrix = rbind(c(1,1,1,2,2), c(3,3,3,3,3)))

##############################
# IFI vs ICI comparison

# read export from GIS
ICI <- read.csv(paste(basepath, "/RawData/ICI_byHUC12.csv", sep=""))
# read file intersected with the floodplain
ICI.intersect <- read.csv(paste(basepath, "/RawData/ICI_byHUC12_FloodplainIntersect.csv", sep=""))

IFI <- func.IFI
IFI$Overall <- all.data$IFI_geomea
IFI$HUC12 <- all.data$HUC12

# Join ICI and IFI based on HUC12
ICI.comp <- merge(IFI, ICI, by.x = "HUC12", by.y = "IFI_HUC12", all.x = TRUE)
names(ICI.comp)[names(ICI.comp)== 'MEAN_ICI_I'] <- "ICI"

ICI.intersect.comp <- merge(IFI, ICI.intersect, by.x = "HUC12", by.y = "IFI_HUC12", all.x = TRUE)
names(ICI.intersect.comp)[names(ICI.intersect.comp)== 'MEAN_ICI_I'] <- "ICI"

# fit linear models
ICI.lm <- lm(data = ICI.comp, Overall ~ ICI)
R2.ICI <- summary(ICI.lm)$r.squared

ICI.intersect.lm <- lm(data = ICI.intersect.comp, Overall ~ ICI)
R2.ICI.intersect <- summary(ICI.intersect.lm)$r.squared

# Scatter plot of ICI vs IFI
ICI.plot <- ggplot(ICI.comp, aes(x = ICI, y = Overall)) + geom_point() +
  xlim(0,1) + ylim(0,1) +
  # scale_x_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  # scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  coord_equal() +
  xlab("Index of Catchment Integrity") +
  ylab("Overall Index of Floodplain Integrity") +
  ggtitle("All Catchments") +
  geom_text(x= 0.1, y=0.1, label = paste0("R^2 = ", round(R2.ICI,2))) +
  theme_bw()

# Scatter plot for catchments intersected with floodplains, ICI vs IFI
ICI.intersect.plot <- ggplot(ICI.intersect.comp, aes(x = ICI, y = Overall)) + 
  geom_point(size = 1) +
  xlim(0,1) + ylim(0,1) +
  # scale_x_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  # scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  coord_equal() +
  xlab("Index of Catchment Integrity") +
  ylab("Overall Index of Floodplain Integrity") +
  # ggtitle("Catchments Intersected with Floodplain") +
  # geom_text(x= 0.1, y=0.1, label = paste0("R^2 = ", round(R2.ICI.intersect,2))) +
  theme_bw() +
  theme(text = element_text(size=16)) +
  labs(tag = "a)")

grid.arrange(ICI.plot, ICI.intersect.plot, ncol = 2)


# Look at distribution of ICI values (very few over 0.8)
# hist(ICI.comp$ICI, xlim = c(0,1))

##########################################
# Compare IFI to wetland abundance

# read in wetland abundance by HUC-12 file
wetlands <- read.csv(paste(basepath, "/RawData/Wetlands_table.csv", sep=""))
wetlands <- wetlands[,c("HUC12", "Area_Density")]

IFI$StrmOrder <- all.data$StrmOrder
IFI$StrmOrder <- as.factor(IFI$StrmOrder)

# Join Wetland abundance and IFI based on HUC12
wetlands.comp <- merge(wetlands, IFI, by = "HUC12")

# fit linear model
wetlands.lm <- lm(data = wetlands.comp, Overall ~ Area_Density)
R2.wetlands <- summary(wetlands.lm)$r.squared

# lm by stream order

wetlands.lm.order <- by(wetlands.comp, wetlands.comp$StrmOrder, 
                        function(x) lm(data = x, Overall ~ Area_Density))
R2.wetlands.order <- lapply(wetlands.lm.order, function(x) summary(x)$r.squared)


# Scatter plot of wetlands vs IFI
wetlands.plot <- ggplot(wetlands.comp, aes(x = Area_Density, y = Overall)) + 
  geom_point(size = 1) +
  xlim(0,1) + ylim(0,1) +
  # facet_wrap(~ StrmOrder, ncol = 3) +
  # scale_x_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  # scale_y_continuous(breaks = seq(0,1,0.25), labels = seq(0,1,0.25)) +
  coord_equal() +
  xlab("Density of Wetlands") +
  ylab("Overall Index of Floodplain Integrity") +
  # geom_text(x= 0.9, y=0.1, label = paste0("R^2 = ", round(R2.wetlands,2))) +
  theme_bw() +
  theme(text = element_text(size=16)) +
  labs(tag = "b)")
wetlands.plot

grid.arrange(ICI.intersect.plot, wetlands.plot, ncol = 2)

################################
# Sensitivity analysis of Function IFI results

# Numeric value (1 to 5) to represent function with min value
func.sensitivity <- data.frame(HUC12 = as.character(all.data$HUC12))
func.sensitivity$min.func <- apply(func.IFI, 1, which.min)

# add function names
func.lookup <- data.frame(num = seq(1,5), names = c("Floods", "Groundwater", "Sediment",
                                                    "Organics/Solutes", "Habitat"))
func.sensitivity$min.func.name <- func.lookup$names[match(unlist(func.sensitivity$min.func), func.lookup$num)]

# Standard deviation of function IFI
func.sensitivity$std.dev <- apply(func.IFI, 1, sd)

# Modified coefficient of variation (sd of functions / overall IFI (geomean))
func.sensitivity$CV <- func.sensitivity$std.dev/all.data$IFI_geomean

# Plots to visualize sesitivity
func.plot <- ggplot(func.sensitivity, aes(min.func.name)) +
  geom_bar() +
  xlab("Function with Minimum IFI") +
  ylab("Number of Floodplain Units") +
  theme(text = element_text(size=20)) +
  theme_bw()
func.plot

sd.hist <- ggplot(func.sensitivity, aes(x = std.dev)) +
  geom_histogram(binwidth = 0.02) +
  xlab("Standard deviation of Function IFI") +
  ylab("Number of Floodplain Units") +
  theme(text = element_text(size=20)) +
  theme_bw()
sd.hist

# Investigate by stream order
func.sensitivity$StrmOrder <- all.data$StrmOrder

# plot Std dev and C.V. of function IFI by stream order
sd.SO <- ggplot(func.sensitivity, aes(StrmOrder, std.dev, group = StrmOrder)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Stream Order", breaks = seq(1:8)) + 
  ylab("Standard Deviation of Function IFI\n") +
  theme_linedraw() +
  theme(text = element_text(size=20), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  geom_text(data = count.data, aes(StrmOrder, y = max(func.sensitivity$std.dev), label = Freq), 
            nudge_y = 0.05, size = 5)
sd.SO

cv.SO <- ggplot(func.sensitivity, aes(StrmOrder, CV, group = StrmOrder)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Stream Order", breaks = seq(1:8)) + 
  ylab("Coefficient of Variation of Function IFI\n") +
  theme_linedraw() +
  theme(text = element_text(size=20), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  geom_text(data = count.data, aes(StrmOrder, y = 2.02, label = Freq), size = 5)
cv.SO

# Plot minimum function by stream order
min.func.SO <- func.sensitivity %>%
  count(min.func.name, StrmOrder) %>%
  group_by(StrmOrder) %>%
  mutate(percent = n/sum(n))

# plot by percent
min.func.plot <- ggplot(min.func.SO, aes(x= StrmOrder, y = percent, fill = min.func.name)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set1") +
  xlab("Stream Order") +
  theme(text = element_text(size=16)) +
  labs(fill = "Minimum Function")
min.func.plot

min.func.plot2 <- ggplot(min.func.SO, aes(x= StrmOrder, y = n, fill = min.func.name)) +
  geom_col() +
  scale_y_continuous(name = "Count of Floodplain Units") +
  scale_fill_brewer(palette = "Set1") +
  xlab("Stream Order") +
  theme(text = element_text(size=16)) +
  labs(fill = "Minimum Function")
min.func.plot2

# output sensitivity result as csv
write.csv(func.sensitivity, file = paste(out.path, "/IFI_sensitivity.csv", sep=""))

############################
# IFI function to overall ratio

func.ratio <- apply(func.IFI, 2, function(x) x/all.data$IFI_geomean)

# Plot boxplots
ratio.df <- melt(func.ratio)
levels(ratio.df$Var2) = c("Flood Reduction", "Groundwater Storage", "Sediment Regulation",
                             "Organics/Solutes Regulation", "Habitat Provision")

ratio.plot <- ggplot(ratio.df, aes(x = Var2, y = value)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs( y = "Ratio of Function to Overall IFI\n", x = NULL) +
  theme_linedraw() +
  theme(text = element_text(size=16), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(), legend.position = "none")
ratio.plot

# Test for significant differences 

# clean INF and remove
ratio.df <- ratio.df[!is.infinite(ratio.df$value),]
ratio.lm <- lm(value ~ Var2, data = ratio.df)
ratio.pairwise <- lsmeans(ratio.lm, pairwise ~ Var2)
method.contrasts <- ratio.pairwise$contrasts
method.contrasts
# results: all significantly different except sediment and organics
