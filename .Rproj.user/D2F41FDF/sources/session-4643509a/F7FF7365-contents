#heatmap of goldfish 

#load in packages
library(sf)          
library(ggplot2)     
library(dplyr)     
library(KernSmooth)  
library(raster)    
library(viridis)    
library(lubridate)  
library(patchwork)  
library(MASS)


#change goldfish lat lon locations to mapping format
GF_locs <- st_as_sf(Aggdata_June52025, coords = c("Lon", "Lat"), crs = 4326)

#use carrolls bay plot from CarrollsBay_map code

#check crs
GF_locs <- st_transform(GF_locs, st_crs(CarrollsBay))

#format date column
GF_locs$Date <- as.Date(GF_locs$Date)

#groups for the 4 sampling periods
GF_locs <- GF_locs %>%
  mutate(
    sampling_period = case_when(
      Date == as.Date("2025-04-09")~ "S1",
    Date == as.Date("2025-04-14") ~ "S2",
      Date == as.Date("2025-04-22") ~ "S3",
      Date == as.Date("2025-04-28") ~ "S4",
      TRUE ~ "Other"
    )
  )
CarrollsBay<-gg
plot(shorelinemap) +
  geom_sf(fill="lightgrey", color="black") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC) +
  theme_minimal()

##simple heatmap with ggplot that can be clipped to the boudary of the polygon afterwards 
#make sure your boundary map and your goldfish catch coordinates are all in the same coordiante reference system

#shorelinemap <- st_transform(shorelinemap, crs = 4326)  # transforms to WGS84 (lon/lat)
#heat_sf <- st_as_sf(Aggdata_June52025, coords = c("Lon", "Lat"), crs = 4326)
#heat_coords <- cbind(heat_sf, st_coordinates(heat_sf))

##simple plot with basic heatmap function, geom_tile does the heatmap can adjust width, heigth for smaller/larger box points
ggplot() +
  geom_sf(data = shorelinemap, color = "black", fill = "lightgrey") +
  geom_tile(data = Aggdata_June52025,
            aes(x = Lon, y = Lat, fill = Total),
            width = 0.001, height = 0.001) +  # tweak these values
  scale_fill_gradient(low = "blue", high = "red", name = "Number Captured") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC, expand = FALSE) +
  facet_wrap(~Date)
  theme_minimal()


  #below plot adds a KDE smoothing function to make the heatmap look alot like a regualr heatmap
  #BUT not clipped to the shoreline 
#needs to be weighted to show hot pockets of fish aggregetations vs. not aggregation locations   
  ggplot() +
  geom_sf(data = shorelinemap, fill = "lightgrey", color = "black") +
  geom_density_2d_filled(
    data = Aggdata_June52025,
    aes(x = Lon, y = Lat, fill = after_stat(level), weight = Total), #after_stat(level) this is your density stat - divides the 
    #continuous variable of catch into discrete bins or contour levels as seen on the map (legend and plotted)
    contour_var = "ndensity",  # controls how the density values are scaled for contours = normal density between 0 and 1 
    #makes it consistent, esp betwen plots for when we break it out into facetwrap ~date
    adjust = 0.5  # tweak this to smooth more or less
  ) +
  scale_fill_viridis_d(option = "C", name = "Relative Density") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC, expand = FALSE) +
    facet_wrap(~Date)
  theme_minimal() +
  labs(title = "KDE Heatmap of Goldfish captures")

  
  
  ###creating the same maps as above but now incoporating a clip function to only estimate KDE heatmap within bounds of polygon
  
  library(ggplot2)
library(sf)
library(MASS)
library(stars)
library(dplyr)
  library(ks)
  
  
# 1. Prepare the data: Coordinates and weights
coords <- Aggdata_June52025 %>%dplyr::select(Lon, Lat)

weights <- Aggdata_June52025$Total
H <- Hpi(x = coords)  # bandwidth matrix estimation
kde <- kde(x = coords, H = H, w = weights, gridsize = c(300, 300))  # weighted KDE, this is just cellsize so 
#if it looks pixelated depending how zoomed in/out you are you will have to adjust accordingly

# 3. Convert to a stars object
kde_grid <- st_as_stars(list(density = kde$estimate),
                        dimensions = st_dimensions(
                          x = kde$eval.points[[1]],
                          y = kde$eval.points[[2]],
                          cell_midpoints = TRUE
                        ))

# 4. Match CRS
st_crs(kde_grid) <- st_crs(shorelinemap)
shorelinemap <- st_transform(shorelinemap, st_crs(kde_grid))
shorelinemap <- st_make_valid(shorelinemap)
# 5. Clip to polygon (mask KDE by the shoreline polygon)
kde_clipped <- kde_grid[shorelinemap]

# 6. Plot
kde_grid$density <- kde_grid$density / max(kde_grid$density, na.rm = TRUE)

allheatmap<-ggplot() +
  geom_sf(data = shorelinemap, fill = "lightgrey", color = "black") +
  geom_stars(data = kde_clipped) +
  scale_fill_viridis_c(name = "Relative Density") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC, expand = FALSE) +
  #facet_wrap(~Date) +
  theme_minimal() +
  labs(title = "KDE Heatmap of Goldfish Captures (Clipped to Polygon)")

#save this plot as its ALL the data
ggsave("plots/CBay_KDEheatmap_allsamplingevents.png", 
       plot = allheatmap,
       width = 12, height = 9, 
       dpi = 600,
       bg = "white")


#now we cant use fact_wrap in the kde plot so we have to pull each date out and then plot for each 
  
#first event 
# 1. Prepare the data: Coordinates and weights
firstsamp<-Aggdata_June52025 %>%
  filter(Date == as.Date("2025-04-09"))


coords <- firstsamp %>%dplyr::select(Lon, Lat)

weights <- firstsamp$Total
H <- Hpi(x = coords)  # bandwidth matrix estimation
kde <- kde(x = coords, H = H, w = weights, gridsize = c(300, 300))  # weighted KDE, this is just cellsize so 
#if it looks pixelated depending how zoomed in/out you are you will have to adjust accordingly

# 3. Convert to a stars object
kde_grid <- st_as_stars(list(density = kde$estimate),
                        dimensions = st_dimensions(
                          x = kde$eval.points[[1]],
                          y = kde$eval.points[[2]],
                          cell_midpoints = TRUE
                        ))

st_crs(kde_grid) <- st_crs(shorelinemap)
shorelinemap <- st_transform(shorelinemap, st_crs(kde_grid))
shorelinemap <- st_make_valid(shorelinemap)
# 5. Clip to polygon (mask KDE by the shoreline polygon)
kde_clipped <- kde_grid[shorelinemap]

# 6. Plot
kde_grid$density <- kde_grid$density / max(kde_grid$density, na.rm = TRUE)

firstsamp11<-ggplot() +
  geom_sf(data = shorelinemap, fill = "lightgrey", color = "black") +
  geom_stars(data = kde_clipped) +
  scale_fill_viridis_c(name = "Relative Density") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC, expand = FALSE) +
  theme_minimal() +
  labs(title = "KDE Heatmap of Goldfish April 9th 2025")

#save this plot as its ALL the data
ggsave("plots/CBay_KDEheatmap_fourthevent.png", 
       plot = firstsamp11,
       width = 12, height = 9, 
       dpi = 600,
       bg = "white")
#second sampling event 
#April 14th 2025

# 1. Prepare the data: Coordinates and weights
secondsamp<-Aggdata_June52025 %>%
  filter(Date == as.Date("2025-04-14"))

coords <- secondsamp %>%dplyr::select(Lon, Lat)

weights <- secondsamp$Total
H <- Hpi(x = coords)  # bandwidth matrix estimation
kde <- kde(x = coords, H = H, w = weights, gridsize = c(300, 300))  # weighted KDE, this is just cellsize so 
#if it looks pixelated depending how zoomed in/out you are you will have to adjust accordingly

# 3. Convert to a stars object
kde_grid <- st_as_stars(list(density = kde$estimate),
                        dimensions = st_dimensions(
                          x = kde$eval.points[[1]],
                          y = kde$eval.points[[2]],
                          cell_midpoints = TRUE
                        ))

st_crs(kde_grid) <- st_crs(shorelinemap)
shorelinemap <- st_transform(shorelinemap, st_crs(kde_grid))
shorelinemap <- st_make_valid(shorelinemap)
# 5. Clip to polygon (mask KDE by the shoreline polygon)
kde_clipped <- kde_grid[shorelinemap]

# 6. Plot
kde_grid$density <- kde_grid$density / max(kde_grid$density, na.rm = TRUE)

secondsampevent<-ggplot() +
  geom_sf(data = shorelinemap, fill = "lightgrey", color = "black") +
  geom_stars(data = kde_clipped) +
  scale_fill_viridis_c(name = "Relative Density") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC, expand = FALSE) +
  theme_minimal() +
  labs(title = "KDE Heatmap of Goldfish April 14th 2025")

#save this plot as its ALL the data
ggsave("plots/CBay_KDEheatmap_SECONDevent.png", 
       plot = secondsampevent,
       width = 12, height = 9, 
       dpi = 600,
       bg = "white")

########################
#Fourth sampling event 
#April 28th 2025

# 1. Prepare the data: Coordinates and weights
fourthsamp<-Aggdata_June52025 %>%
  filter(Date == as.Date("2025-04-28"))

coords <- fourthsamp %>%dplyr::select(Lon, Lat)

weights <- fourthsamp$Total
H <- Hpi(x = coords)  # bandwidth matrix estimation
kde <- kde(x = coords, H = H, w = weights, gridsize = c(300, 300))  # weighted KDE, this is just cellsize so 
#if it looks pixelated depending how zoomed in/out you are you will have to adjust accordingly

# 3. Convert to a stars object
kde_grid <- st_as_stars(list(density = kde$estimate),
                        dimensions = st_dimensions(
                          x = kde$eval.points[[1]],
                          y = kde$eval.points[[2]],
                          cell_midpoints = TRUE
                        ))

st_crs(kde_grid) <- st_crs(shorelinemap)
shorelinemap <- st_transform(shorelinemap, st_crs(kde_grid))
shorelinemap <- st_make_valid(shorelinemap)
# 5. Clip to polygon (mask KDE by the shoreline polygon)
kde_clipped <- kde_grid[shorelinemap]

# 6. Plot
kde_grid$density <- kde_grid$density / max(kde_grid$density, na.rm = TRUE)

fourthsampevent<-ggplot() +
  geom_sf(data = shorelinemap, fill = "lightgrey", color = "black") +
  geom_stars(data = kde_clipped) +
  scale_fill_viridis_c(name = "Relative Density") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC, expand = FALSE) +
  theme_minimal() +
  labs(title = "KDE Heatmap of Goldfish April 28th 2025")

#save this plot as its ALL the data
ggsave("plots/CBay_KDEheatmap_Thirdevent.png", 
       plot = thirdsampevent,
       width = 12, height = 9, 
       dpi = 600,
       bg = "white")


# 1. Prepare the data: Coordinates and weights
thirdsamp<-Aggdata_June52025 %>%
  filter(Date == as.Date("2025-04-22"))

coords <- thirdsamp %>%dplyr::select(Lon, Lat)

weights <- thirdsamp$Total
H <- Hpi(x = coords)  # bandwidth matrix estimation
kde <- kde(x = coords, H = H, w = weights, gridsize = c(300, 300))  # weighted KDE, this is just cellsize so 
#if it looks pixelated depending how zoomed in/out you are you will have to adjust accordingly

# 3. Convert to a stars object
kde_grid <- st_as_stars(list(density = kde$estimate),
                        dimensions = st_dimensions(
                          x = kde$eval.points[[1]],
                          y = kde$eval.points[[2]],
                          cell_midpoints = TRUE
                        ))

st_crs(kde_grid) <- st_crs(shorelinemap)
shorelinemap <- st_transform(shorelinemap, st_crs(kde_grid))
shorelinemap <- st_make_valid(shorelinemap)
# 5. Clip to polygon (mask KDE by the shoreline polygon)
kde_clipped <- kde_grid[shorelinemap]

# 6. Plot
kde_grid$density <- kde_grid$density / max(kde_grid$density, na.rm = TRUE)

fourthsamplingevent<-ggplot() +
  geom_sf(data = shorelinemap, fill = "lightgrey", color = "black") +
  geom_stars(data = kde_clipped) +
  scale_fill_viridis_c(name = "Relative Density") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC, expand = FALSE) +
  theme_minimal() +
  labs(title = "KDE Heatmap of Goldfish April 28th 2025")

#save this plot as its ALL the data
ggsave("plots/CBay_KDEheatmap_fourthevent.png", 
       plot = thirdsampevent,
       width = 12, height = 9, 
       dpi = 600,
       bg = "white")
#combine the plots 

library(patchwork)

cmdplots<-(firstsamplingdate | secondsampevent) /
(thirdsampevent | fourthsamplingevent)


ggsave("plots/Allsamplingevents_heatmapscmbd.png", 
       plot = cmdplots,
       width = 12, height = 9, 
       dpi = 600,
       bg = "white")
