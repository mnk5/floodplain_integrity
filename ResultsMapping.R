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

# Set working directory
setwd("C:/Users/mnk5/Documents/floodplain_integrity")

# read in files
floodplain <- readOGR(dsn = "RawData/SpatialData", layer = "CO_FP_IFI")
CO.boundary <- readOGR(dsn = "RawData/SpatialData", layer = "CO_StateBoundary_UTM")
CO.HUC12 <- readOGR(dsn = "RawData/SpatialData", layer = "CO_HUC12")

# Clean data
floodplain$HUC12 <- as.character(floodplain$HUC12)


# transform for ggplot
floodplain_tidy <- tidy(floodplain, region = "HUC12")
floodplain.df <- left_join(floodplain_tidy, floodplain@data, by = c("id" = "HUC12"))

CO.boundary@data$id <- row.names(CO.boundary@data)
CO.boundary_tidy <- tidy(CO.boundary, region = 'id')


# Plot results

# Overall IFI
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

# All IFI by function
# 
# map2 <- ggplot(data = floodplain.df, aes(x = long, y = lat, group = group, fill = Floods)) + 
#   geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), fill = "grey93") +
#   geom_polygon() +
#   coord_equal() +
#   scale_fill_gradientn(colours = c("chocolate4", "wheat1" ,"darkcyan"), breaks = seq(0, 1, by = 0.1)) +
#   labs(x = NULL, y = NULL, fill = "IFI") +
#   ggtitle("a) Flood Reduction IFI") +
#   theme_minimal(base_size = 10) + 
#   theme(legend.position = "none") +
#   theme(axis.text=element_blank()) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# map3 <- ggplot(data = floodplain.df, aes(x = long, y = lat, group = group, fill = Groundwate)) + 
#   geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), fill = "grey93") +
#   geom_polygon() +
#   coord_equal() +
#   scale_fill_gradientn(colours = c("chocolate4", "wheat1" ,"darkcyan"), breaks = seq(0, 1, by = 0.1)) +
#   labs(x = NULL, y = NULL, fill = "IFI") +
#   ggtitle("b) Groundwater Storage IFI") +
#   theme_minimal(base_size = 10) + 
#   theme(legend.position = "none") +
#   theme(axis.text=element_blank()) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# map4 <- ggplot(data = floodplain.df, aes(x = long, y = lat, group = group, fill = Sediment)) + 
#   geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), fill = "grey93") +
#   geom_polygon() +
#   coord_equal() +
#   scale_fill_gradientn(colours = c("chocolate4", "wheat1" ,"darkcyan"), breaks = seq(0, 1, by = 0.1)) +
#   labs(x = NULL, y = NULL, fill = "IFI") +
#   ggtitle("c) Sediment Regulation IFI") +
#   theme_minimal(base_size = 10) + 
#   theme(legend.position = "none") +
#   theme(axis.text=element_blank()) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# map5 <- ggplot(data = floodplain.df, aes(x = long, y = lat, group = group, fill = Organics_S)) + 
#   geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), fill = "grey93") +
#   geom_polygon() +
#   coord_equal() +
#   scale_fill_gradientn(colours = c("chocolate4", "wheat1" ,"darkcyan"), breaks = seq(0, 1, by = 0.1)) +
#   labs(x = NULL, y = NULL, fill = "IFI") +
#   ggtitle("d) Organics/Solutes Regulation IFI") +
#   theme_minimal(base_size = 10) + 
#   theme(legend.position = "none") +
#   theme(axis.text=element_blank()) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# map6 <- ggplot(data = floodplain.df, aes(x = long, y = lat, group = group, fill = Habitat)) + 
#   geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), fill = "grey93") +
#   geom_polygon() +
#   coord_equal() +
#   scale_fill_gradientn(colours = c("chocolate4", "wheat1" ,"darkcyan"), breaks = seq(0, 1, by = 0.1)) +
#   labs(x = NULL, y = NULL, fill = "IFI") +
#   ggtitle("e) Habitat Provision IFI") +
#   theme_minimal(base_size = 10) + 
#   theme(legend.position = "none") +
#   # theme(legend.text = element_text(size = 8)) +
#   theme(axis.text=element_blank()) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# 
# grid.arrange(map2, map3, map4, map5, map6, nrow = 3)


# Alternate Method
fp.df <- melt(floodplain.df, id = 1:10, measure = 10:14 )
levels(fp.df$variable) = c("Flood Reduction", "Groundwater Storage", "Sediment Regulation",
                           "Organics/Solutes Regulation", "Habitat Provision", "Overall IFI")

map7 <- ggplot(data = fp.df, aes(x = long, y = lat, group = group, fill = value)) + 
  geom_polygon(data = CO.boundary_tidy, aes(x = long, y = lat, group = group), fill = "grey93") +
  geom_polygon() +
  coord_equal() +
  facet_wrap(~ variable, ncol = 2) +
  scale_fill_gradientn(colours = c("chocolate4", "wheat1" ,"darkcyan"), breaks = seq(0, 1, by = 0.1)) +
  labs(x = NULL, y = NULL, fill = "IFI") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = c(0.8,0.08), legend.justification = c(1,0)) +
  theme(legend.text = element_text(size = 8)) +
  theme(axis.text=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
map7
