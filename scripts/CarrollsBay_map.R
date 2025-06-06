######################
###
### Hamilton Harbour 
### Acoustic telemetry
### Goldfish

#######################
library(rgdal)
library(sp)
library(sf)
library(ggplot2)

library(dplyr)       # For data manipulation
library(KernSmooth)  # For 2D kernel density estimation
library(raster)      # For raster operations
library(viridis)     # For color palettes
library(lubridate)   # For date handling
library(patchwork)

#add in polygon shapeile first
###need to convert to lat long from UTMS or points
shorelinemap <- st_read("data/lakeontarionorthshoreline/LakeOntShoreline_MajorWaters.shp")

ggplot(shorelinemap) +
  geom_sf(fill = "lightblue", color = "black", size = 0.5) +
  theme_minimal() +
  theme(axis.text = element_text(size = 8))


# Now use geom_polygon
ggplot(shoreline_df, aes(x = lon, y = lat, group=L2)) +
  geom_polygon(fill = "lightblue", color = "darkblue") +
  coord_equal() +
  theme_minimal()


#map for bubbleplots and animations
##see shapefile_HH for map code
#cootes paradise, Grindstone, west end harbour
x_limits_HH <- c(-79.96, -79.76)  # Define your desired range for x-axis
y_limits_HH <- c(43.25, 43.32)  # Define your desired range for y-axis

#hamilton harbour
x_limits_HHall <- c(-79.94, -79.78)  # Define your desired range for x-axis
y_limits_HHall <- c(43.265, 43.315)  # Define your desired range for y-axis

#GCzoom in 
x_limits_GC <- c(-79.891, -79.88)  # Define your desired range for x-axis
y_limits_GC <- c(43.28, 43.291)  # Define your desired range for y-axis

##HH and lko
x_limits_lko<- c(-79.90, -77) 
y_limits_lko<- c(43.18, 44.0)

plot(shorelinemap)

#cootes and harbour
X_limits_CPM_HH<-c(-79.9, -79.86) 
y_limits_CPM_HH<-c(43.18, 44.0)
################
# Plot the shapefile with specified x and y limits
CarrollsBay<-ggplot(shorelinemap) +
  geom_sf(fill="lightgrey", color="black") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC) +
  theme_minimal()

plot(CarrollsBay)

################################

