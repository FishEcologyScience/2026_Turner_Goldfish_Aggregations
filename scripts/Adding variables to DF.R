##add in map baselayer
HH_gcmap <- st_read("data/HH_WaterLinesToPoly_21Mar2025.shp")

library(sf)
library(ggplot2)


# Transform CRS
HH_gcmap <- st_transform(HH_gcmap, crs = 4326)

# Polygonize the linestrings into polygon features
polygons_sf <- st_polygonize(st_union(HH_gcmap)) |> st_as_sf()


#####
#things we need to add to the dataset. 
#1. distance to shoreline 
#2. distance to rivermouth

#1. Gathering a distance to shoreline variable for each goldfish occurance 

# Load required libraries
library(sf)
library(dplyr)
library(units)

# Assuming your data is already loaded:
# points_data <- your point dataset (sf object)
# shoreline_polygon <- your shoreline polygon (sf object)
# river_mouths <- your river mouth points (sf object)

# Make sure all data has the same CRS (coordinate reference system)
# Check CRS
GF_locs <- st_as_sf(Aggdata_June52025, coords = c("Lon", "Lat"), crs = 4326)

st_crs(GF_locs)
st_crs(HH_gcmap)

# ===============================
# 1. Distance to Shoreline
# ===============================

# Convert shoreline polygon to boundary
shoreline_boundary <- st_boundary(HH_gcmap)
# Calculate distance from points to shoreline boundary
dist_to_shore <- st_distance(GF_locs, HH_gcmap)
# Extract distances
Aggdata_June52025$dist_to_shoreline_m <- as.numeric(dist_to_shore[,1])
colnames(GF_locs)
# ===============================
# 2. Distance to River Mouth
# ===============================
#add in a point that defines the location of the river mouth 
river_mouth_lat <- 43.289567
river_mouth_lon <- -79.887236

GF_locs <- st_as_sf(Aggdata_June52025, coords = c("Lon", "Lat"), crs = 4326)

river_mouth_sf <- st_sfc(st_point(c(river_mouth_lon, river_mouth_lat)), 
                         crs = 4326)

distances_m <- st_distance(GF_locs, river_mouth_sf)
Aggdata_June52025$distance_to_river_mouth_m <- as.numeric(distances_m)


