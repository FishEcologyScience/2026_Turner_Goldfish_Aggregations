##mapping data first
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

#
setwd("C:/Users/TURNERN/Documents/For Github/Aggregations-Goldfish-HH")

#add in polygon shapeile first
###need to convert to lat long from UTMS or points
#this is the less detailed shoreline map for grindstone
#shorelinemap <- st_read("data/lakeontarionorthshoreline/LakeOntShoreline_MajorWaters.shp")

#add in the other shapefile


ggplot(shorelinemap) +
  geom_sf(fill = "lightblue", color = "black", size = 0.5) +
  theme_minimal() +
  theme(axis.text = element_text(size = 8))


# Now use geom_polygon
ggplot(shoreline_df, aes(x = lon, y = lat, group=L2)) +
  geom_polygon(fill = "lightblue", color = "darkblue") +
  coord_equal() +
  theme_minimal()


#GCzoom in 
x_limits_GC <- c(-79.891, -79.88)  # Define your desired range for x-axis
y_limits_GC <- c(43.28, 43.291)  # Define your desired range for y-axis


################
# Plot the shapefile with specified x and y limits
CarrollsBay<-ggplot(shorelinemap) +
  geom_sf(fill="lightgrey", color="black") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC) +
  theme_minimal()

plot(CarrollsBay)
#add in transect sampling and elevation map
CB_transects <- st_read("data/GF_FieldPlanning_2025/GF_CarrollsBayTransects_Apr2025.shp")
CB_boundary<-st_read("data/GF_FieldPlanning_2025/GF_CarrollsZoneBoundaries_Apr2025.shp")
CB_points<-st_read("data/GF_Points/GF_CarrollsBay_Transect_points.shp")

#add to basemap
#adds in the designated areas, south, north, west, east
#and the transect sampling lines 

CarrollsBay1<-ggplot() +
  geom_sf(data= shorelinemap,fill="lightblue", color="black") +
  geom_sf(data=CB_transects, linewidth=1, color="gray30")+
  geom_sf(data=CB_boundary, fill=alpha("lightgreen", 0.2))+
 # geom_sf(data=CB_points, color="black")+
   coord_sf(xlim = x_limits_GC, ylim = y_limits_GC) +
  theme_minimal()

################################



#load in aggregations data file and plot on shape file (HH_shapefile code)

Aggdata_June52025 <- read_excel("data/raw/Aggdata_June52025.xlsx")


#could condense the data down further so its not singular aggregation locations but rather 
#transect aggregations, ie summarize the total captured/missed per day sampled 

#for now lets just see if we can get the data plotting up on a map
#bring in the carrolls bay shapefile - CarrollsBay_map
#this makes it a ggplot object 
CarrollsBay<-ggplot(shorelinemap) +
  geom_sf(fill="lightgrey", color="black") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC) +
  theme_minimal()

#for plotting points dont want to use a ggplot object 
CarrollsBay_with_points <- CarrollsBay +
  geom_point(data = Aggdata_June52025, 
             aes(x = Lon, y = Lat), 
             size = 2, color = "red")+
 facet_wrap(~Date)
#
#just for second round of sampling to deal with missing transect names 

secondsampling <- Aggdata_June52025 %>% 
  filter(Date == as.Date("2025-04-14"))


p <- plot_ly(data = secondsampling, 
             x = ~Lon, y = ~Lat, 
             type = 'scatter', 
             mode = 'markers',
             marker = list(size = 8, color = 'red'),
             hovertemplate = paste('<b>Waypoint:</b> %{text}<br>',
                                   '<b>Longitude:</b> %{x}<br>',
                                   '<b>Latitude:</b> %{y}',
                                   '<extra></extra>'),
             text = ~Waypoint) %>%
  layout(title = "Carroll's Bay Data Points",
         xaxis = list(title = "Longitude"),
         yaxis = list(title = "Latitude"))

p


#additonal layers we can include in the maps
#should add in a bathy layer then we cna just grab depths right from each point  GIS?
CarrollsBay_with_points_1 <- CarrollsBay +
  geom_point(data = Aggdata_June52025, 
             aes(x = Lon, y = Lat, 
             size = "Number_captured")+
  theme_minimal())

CarrollsBay_with_points_1 <- CarrollsBay +
  geom_point(data = Aggdata_June52025, 
             aes(x = Lon, y = Lat, size = Number_captured, color = Number_captured), 
             alpha = 0.7) +
  scale_size_continuous(name = "Number\nCaptured", range = c(2, 10)) +
  scale_color_viridis_c(name = "Number\nCaptured") +
  facet_wrap(~Date)+
  theme_minimal()

plot(CarrollsBay_with_points_1)

#saved the above code right into the github workspace plots folder
ggsave("plots/CarrollsBay_captures_bubbleplot.png", 
       plot = CarrollsBay_with_points_1,
       width = 12, height = 9, 
       dpi = 600,
       bg = "white")

#############


#from this we can add our points of where fish were captured

#all points
CarrollsBay_with_points <- CarrollsBay1 +
  geom_point(data = Aggdata_June52025, 
             aes(x = Lon, y = Lat), 
             size = 2, color = "darkred")+
 facet_wrap(~Date)

ggsave("plots/GF_locations_transects.png", 
       plot = CarrollsBay_with_points,
       width = 12, height = 9, 
       dpi = 600,
       bg = "white")


#same plot but now only with <5 individuals at each location plotted showing aggregations
CarrollsBay_with_points <- CarrollsBay1 +
  geom_point(data = Aggdata_June52025, 
             aes(x = Lon, y = Lat, color = Total >= 5), 
             size = 2) +
  facet_wrap(~Date)


ggsave("plots/CarrollsBay_transectlocations_aggregationsbyTOTAL.png", 
       plot = CarrollsBay_with_points,
       width = 12, height = 9, 
       dpi = 600,
       bg = "white")


#same plot but now only with <5 individuals at each location plotted showing aggregations
CarrollsBay_with_points_aggs <- CarrollsBay1 +
  geom_point(data = Aggdata_June52025, 
             aes(x = Lon, y = Lat, color = Total >= 5, label=Waypoint), 
             size = 2) +
  scale_color_manual(values = c("FALSE" = "orange", "TRUE" = "darkred"),
                     labels = c("< 5", "≥ 5"),
                     name = "Goldfish") +
  facet_wrap(~Date)

ggplotly(CarrollsBay_with_points_aggs)
