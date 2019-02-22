#####################

# Floodplain Integrity Metric analysis

# Code to take all data tables and assimilate
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
colnames(data.merge)[colnames(data.merge)=="MEAN"] <- "Agr"

data.merge <- merge(data.merge, Buildings[, c("HUC12", "Area_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Area_Density"] <- "Build"

data.merge <- merge(data.merge, CanalDitch[, c("HUC12", "Line_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Line_Density"] <- "Ditch"

data.merge <- merge(data.merge, DevelopedArea[, c("HUC12", "MEAN")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="MEAN"] <- "Dev"

data.merge <- merge(data.merge, ForestLoss[, c("HUC12", "MEAN")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="MEAN"] <- "ForLoss"

data.merge <- merge(data.merge, ImperviousArea[, c("HUC12", "MEAN")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="MEAN"] <- "Imperv"

data.merge <- merge(data.merge, LeveedArea[, c("HUC12", "Area_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Area_Density"] <- "Levee"

data.merge <- merge(data.merge, Railroads[, c("HUC12", "Line_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Line_Density"] <- "RR"

data.merge <- merge(data.merge, Roads[, c("HUC12", "Line_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Line_Density"] <- "Road"

data.merge <- merge(data.merge, WellStructures[, c("HUC12", "Point_Density")], by = "HUC12", all.x = TRUE)
colnames(data.merge)[colnames(data.merge)=="Point_Density"] <- "Well"

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

# Plotting correlations
corrplot(Correl.NA, type = "upper", method = "circle", tl.col="black", tl.srt=45,
         diag=FALSE, addCoef.col = "#909196",number.cex = .7, cl.pos ="n")


# Correlation analysis (w zero instead of NA)
Correl <- cor(data.merge[,4:length(data.merge)], use = "pairwise.complete.obs")

# Significance test
res <- cor.mtest(data.merge[,4:length(data.merge)], conf.level =0.95)

# Plotting correlations
corrplot(Correl, type = "upper", method = "circle", tl.col="black", tl.srt=45, 
         diag=FALSE, addCoef.col = "#909196", number.cex = .7, cl.pos ="n")
