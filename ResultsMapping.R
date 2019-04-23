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
library(gridExtra)
library(reshape2)
library(ggmap)

# Set working directory
setwd("C:/Users/mnk5/Documents/floodplain_integrity")

# read in files
floodplain <- readOGR(dsn = "RawData/SpatialData", layer = "CO_FP_IFI")
CO.boundary <- readOGR(dsn = "RawData/SpatialData", layer = "CO_StateBoundary_UTM")
CO.HUC12 <- readOGR(dsn = "RawData/SpatialData", layer = "CO_HUC12_IFI")
# NHD v1 segments order 4 and larger in Colorado
CO.rivers <- readOGR(dsn = "RawData/SpatialData", layer = "NHDv1_Order4_CO")

# Clean data
floodplain$HUC12 <- as.character(floodplain$HUC12)
CO.HUC12$HUC12 <- as.character(CO.HUC12$HUC12)

# transform for ggplot
floodplain_tidy <- tidy(floodplain, region = "HUC12")
floodplain.df <- left_join(floodplain_tidy, floodplain@data, by = c("id" = "HUC12"))

HUC12_tidy <- tidy(CO.HUC12, region = "HUC12")
HUC12.df <- left_join(HUC12_tidy, CO.HUC12@data, by = c("id" = "HUC12"))

CO.boundary@data$id <- row.names(CO.boundary@data)
CO.boundary_tidy <- tidy(CO.boundary, region = 'id')

CO.rivers@data$id <- row.names(CO.rivers@data)
CO.rivers_tidy <- tidy(CO.rivers, region = 'id')

###############################
# Plot results

# choose bounding box area for zoomed in area
zoomsize <- 46000
xlimits <- c(494000,494000 + zoomsize)
ylimits <- c(4506000, 4506000 - zoomsize)

# Floodplains in state
map <- ggplot(data = floodplain.df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), fill = "grey97") +
  geom_polygon(data = floodplain.df, aes(x = long, y = lat, group = group), fill = "grey50") +
  geom_path(data = CO.rivers_tidy, aes(x = long, y = lat, group = group), color = "mediumblue", size = 0.25) +
  geom_rect(aes(xmin = min(xlimits), xmax = max(xlimits), ymin = min(ylimits), ymax = max(ylimits)),
            fill = "transparent", color = "red", size = 1.5) +
  coord_equal() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 14) + 
  theme(legend.text = element_text(size = 8)) +
  theme(axis.text=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

map

# Overall IFI

map1 <- ggplot(data = floodplain.df, aes(x = long, y = lat, group = group, fill = IFI_geomea)) + 
  geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), fill = "grey93") +
  geom_polygon(data = floodplain.df, aes(x = long, y = lat, group = group, fill = IFI_geomea)) +
  coord_equal() +
  # coord_fixed(ratio = 1, xlim = xlimits, ylim = ylimits) +
  scale_fill_gradientn(colours = c("chocolate4", "wheat1" ,"darkcyan"), breaks = seq(0, 1, by = 0.2))
map1 <- map1 + labs(x = NULL, y = NULL, fill = "IFI")
map1 <- map1 + theme_minimal(base_size = 14) + 
  theme(legend.text = element_text(size = 14)) +
  theme(axis.text=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

map1

# Overall IFI mapped to HUC-12 units

map2 <- ggplot(data = HUC12.df, aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = HUC12.df, color = "grey27", size = 0.1, aes(x = long, y = lat, group = group, fill = IFI_geomea)) +
  geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", size = 1.5) +
  geom_path(data = CO.rivers_tidy, aes(x = long, y = lat, group = group), color = "navy", size = 1) +
  coord_equal() +
  # coord_fixed(ratio = 1, xlim = xlimits, ylim = ylimits) +
  scale_fill_gradientn(colours = c("chocolate4", "wheat1" ,"darkcyan"), breaks = seq(0, 1.0, by = 0.2), 
                       labels = c("0.0","0.2", "0.4", "0.6", "0.8", "1.0"), limits = c(0,1)) +
  labs(x = NULL, y = NULL, fill = "IFI") +
  theme_minimal(base_size = 12) + 
  theme(legend.text = element_text(size = 12)) +
  theme(axis.text=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

map2
# # All IFI by function

# Mapping zoomed in area IFI by function
fp.df <- melt(floodplain.df, id = 1:9, measure = 13:18)
levels(fp.df$variable) = c("Flood Reduction", "Groundwater Storage", "Sediment Regulation",
                           "Organics/Solutes Regulation", "Habitat Provision", "Overall IFI")

# change projection to WGS84


map7 <- ggplot(data = fp.df, aes(x = long, y = lat, group = group, fill = value)) + 
  # geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), fill = "grey93") +
  # ggmap(get_map(location = bbox(CO.boundary), source = "osm", maptype = "toner", color = "bw", zoom = 1)) +
  geom_polygon() +
  # coord_equal() +
  coord_fixed(ratio = 1, xlim = xlimits, ylim = ylimits) +
  facet_wrap(~ variable, ncol = 3) +
  scale_fill_gradientn(colours = c("chocolate4", "wheat1" ,"darkcyan"), breaks = seq(0, 1, by = 0.2)) +
  labs(x = NULL, y = NULL, fill = "IFI") +
  theme_minimal(base_size = 16) + 
  theme(panel.background = element_rect(fill = "grey93"),
        panel.border = element_rect(fill = NA, colour = "black"),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.key.width = unit(1, "cm"))
        # axis.text=element_blank(),
        # panel.grid.major = element_blank(), panel.grid.minor = element_blank())
map7
