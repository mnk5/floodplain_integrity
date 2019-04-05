################
# Floodplain Integrity Assessment 
# Mapping of floodplain integrity results
# M. Karpack, Spring 2019

# Plots the results of the IFI analysis
# including figures for publication

library(ggplot2)
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(dplyr)
library(tidyr)
library(broom)

# Set working directory
setwd("C:/Users/mnk5/Documents/floodplain_integrity")

# read in files
floodplain <- readOGR(dsn = "RawData/SpatialData", layer = "CO_FP_IFI")
CO.boundary <- readOGR(dsn = "RawData/SpatialData", layer = "CO_StateBoundary_UTM")
CO.HUC12 <- readOGR(dsn = "RawData/SpatialData", layer = "CO_HUC12")

floodplain$HUC12 <- as.character(floodplain$HUC12)

# transform for ggplot
floodplain_tidy <- tidy(floodplain, region = "HUC12")
floodplain.df <- left_join(floodplain_tidy, floodplain@data, by = c("id" = "HUC12"))

CO.boundary@data$id <- row.names(CO.boundary@data)
CO.boundary_tidy <- tidy(CO.boundary, region = 'id')


# Plot results


map1 <- ggplot(data = floodplain.df, aes(x = long, y = lat, group = group, fill = IFI_geomea)) + 
  geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), fill = "grey93") +
  geom_polygon(data = floodplain.df, aes(x = long, y = lat, group = group, fill = IFI_geomea)) +
  coord_equal() +
  scale_fill_gradientn(colours = c("chocolate4", "wheat1" ,"darkcyan"), breaks = seq(0, 1, by = 0.1))
map1 <- map1 + labs(x = NULL, y = NULL, fill = "IFI")
map1 <- map1 + theme_minimal(base_size = 14) + 
  theme(legend.text = element_text(size = 8)) +
  theme(axis.text=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

map1

