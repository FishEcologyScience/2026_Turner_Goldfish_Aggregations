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
plot(CarrollsBay_with_points)

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