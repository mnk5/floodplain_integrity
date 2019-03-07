#####################
# Floodplain Integrity Assessment
# Stressor data correlation analysis
# M. Karpack, Spring 2019


# Code to take all data tables from GIS data exports and assimilate
# And then check for correllation using Pearson's correlation coeffcient

library(lsmeans)
library(corrplot)
library(foreign)
library(tools)
library(data.table)
library(RColorBrewer)

basepath <- "C:/Users/mnk5/Documents/floodplain_integrity"

# Load all csv files in folder into list
data.path <- paste(basepath, "/RawData/", sep="")
filelist <- list.files(path = data.path, pattern="*.csv")

# read in each .csv file in folder and create a data frame with the same name as the .csv file
for (i in 1:length(filelist)){
  
  assign(file_path_sans_ext(filelist[i]), 
         read.csv(paste(data.path, filelist[i], sep=''))
  )}

## Get all information relevant into one df using FP HUC-12 as basis

data.merge <- merge(FP_Info[,c("HUC12", "FP_Areakm2", "StrmOrder")], AgricultureArea[, c("HUC12", "MEAN")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="MEAN"] <- "Agriculture"

data.merge <- merge(data.merge, Buildings[, c("HUC12", "Area_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Area_Density"] <- "Buildings"

data.merge <- merge(data.merge, CanalDitch[, c("HUC12", "Line_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Line_Density"] <- "Ditches"

data.merge <- merge(data.merge, DevelopedArea[, c("HUC12", "MEAN")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="MEAN"] <- "Developed"

data.merge <- merge(data.merge, ForestLoss[, c("HUC12", "MEAN")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="MEAN"] <- "ForestLoss"

data.merge <- merge(data.merge, ImperviousArea[, c("HUC12", "MEAN")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="MEAN"] <- "Impervious"

data.merge <- merge(data.merge, LeveedArea[, c("HUC12", "Area_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Area_Density"] <- "LeveedArea"

data.merge <- merge(data.merge, NonNativeVeg[, c("HUC12", "MEAN")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="MEAN"] <- "NonNativeVeg"

data.merge <- merge(data.merge, RoadsRailroads[, c("HUC12", "Line_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Line_Density"] <- "Roads_Rail"

# Combined roads and railroads into one line file

# data.merge <- merge(data.merge, Railroads[, c("HUC12", "Line_Density")], by = "HUC12", all.x = TRUE)
# colnames(data.merge)[colnames(data.merge)=="Line_Density"] <- "Railroads"
# 
# data.merge <- merge(data.merge, Roads[, c("HUC12", "Line_Density")], by = "HUC12", all.x = TRUE)
# colnames(data.merge)[colnames(data.merge)=="Line_Density"] <- "Roads"

data.merge <- merge(data.merge, WellStructures[, c("HUC12", "Point_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Point_Density"] <- "Wells"

data.merge <- merge(data.merge, MeanHA_MaxOrderOnly[, c("HUC12", "MEAN_pnMH20", "MEAN_pnFH1","MEAN_pnFH6",
                                                        "MEAN_pnFH7","MEAN_pnDH1", "MEAN_pnDH2","MEAN_pnDH3",
                                                        "MEAN_pnDH4", "MEAN_pnDH5","MEAN_pnDH15")], by = "HUC12", all.x = TRUE)
setnames(data.merge, old =c("MEAN_pnMH20", "MEAN_pnFH1","MEAN_pnFH6", "MEAN_pnFH7","MEAN_pnDH1", "MEAN_pnDH2","MEAN_pnDH3",
                            "MEAN_pnDH4", "MEAN_pnDH5","MEAN_pnDH15" ), new = c("MH20", "FH1", "FH6","FH7", "DH1", "DH2",
                                                                                 "DH3","DH4","DH5","DH15" ))



## Version with zeros changed to NA 
data.merge.NA <- data.merge
data.merge.NA[data.merge.NA==0] <- NA

# version with NA changed to zero
data.merge[is.na(data.merge)] <- 0


# Save as .csv file 
out.path <- paste(basepath, "/Outputs/", sep="")
out.file <- paste(out.path, "Combined_Data.csv", sep="")
write.csv(data.merge, file = out.file, row.names = FALSE)


##########################################################

# Correlation analysis (omitting NAs)
Correl.NA <- cor(data.merge.NA[,4:length(data.merge.NA)], use = "pairwise.complete.obs")

# Significance test
res.NA <- cor.mtest(data.merge.NA[,4:length(data.merge.NA)], conf.level =0.95)

# Plotting (and saving) correlations
out.graph.NA <- paste(out.path, "Correlation_NA.jpg", sep="")
jpeg(out.graph.NA, width = 2000, height = 2000, units = "px")
corrplot(Correl.NA, type = "upper", method = "color", tl.col="black", tl.srt=45,
         tl.cex= 2.5, diag=FALSE, addCoef.col = "#9fa0a5",number.cex = 2, cl.pos ="n")
dev.off()


# Correlation analysis (w zero instead of NA)
Correl <- cor(data.merge[,4:length(data.merge)], use = "pairwise.complete.obs")

# Significance test
res <- cor.mtest(data.merge[,4:length(data.merge)], conf.level =0.95)

# Plotting (and saving) correlations
out.graph <- paste(out.path, "Correlation.jpg", sep="")
jpeg(out.graph, width = 2000, height = 2000, units = "px")
corrplot(Correl, type = "upper", method = "color", tl.col="black", tl.srt=45, 
         tl.cex= 2.5, diag=FALSE, addCoef.col = "#9fa0a5", number.cex = 2, cl.pos ="n")
dev.off()


