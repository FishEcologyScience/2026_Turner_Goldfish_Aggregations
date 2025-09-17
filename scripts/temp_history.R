###creating a history of temp data that we have based on point sampling location 
#incluidng all the aggregation data temps and transect sampling temps 

#need to assign an location, could just be all the starts of transects 
#CB_points<-st_read("data/GF_Points/GF_CarrollsBay_Transect_points.shp")

print(CB_points)
# Convert to regular dataframe (removes geometry column)
Points_CB <- st_drop_geometry(CB_points)

#pair down the dataframe take the middle to start

CB_pointsmiddle <- dplyr::filter(Points_CB, PointID %in% c("CS2", "ES2", "WS2", "WA2", "WB2", 
                                                           "CA2", "CB2", "SA2", "SB2", "SC2", "SD2",
                                                           "EA2", "EB2", "EC2"))
#now pull the location data from this into our larger dataframe with temp data

combined_data <- dplyr::left_join(Transect_samplingdata_cleanJune2025, CB_pointsmiddle, 
                                  by = "Transect")
#transect sampling data now how individual waypoints from the middle of each transect
  combined_data1<-combined_data%>%filter(!is.na(Temp))
  
# Use shorelinemap (the actual spatial data) instead of CarrollsBay
ggplot() +
  geom_sf(data = shorelinemap, fill = "lightgray", color = "black") +
  geom_point(data = combined_data1, 
             aes(x = POINT_X, y = POINT_Y, color = Temp), 
             size = 3) +
  scale_color_viridis_c(name = "Water Temp (°C)", option = "plasma") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC) +
  facet_wrap(~Date)
  theme_minimal() +
  labs(title = "Water Temperature by Location")


  #now add in the temp dta from the aggregations 
  
  
  #first remova all temps that have an NA value 
  Aggdata_June52025$Temp<-as.numeric(Aggdata_June52025$Temp)
  agg_temponly<-Aggdata_June52025%>%filter(!is.na(Temp))
  
  
  ggplot() +
  geom_sf(data = shorelinemap, fill = "lightgray", color = "black") +
  geom_point(data = combined_data1, 
             aes(x = POINT_X, y = POINT_Y, color = Temp), 
             size = 3) +
    geom_point(data=agg_temponly, 
               aes(x=Lon, y=Lat, color=Temp, label=Temp), 
               size=3, vjust=-1.5, fill="white", alpha=0.8)+
  scale_color_viridis_c(name = "Water Temp (°C)", option = "plasma") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC) +
  facet_wrap(~Date)
  theme_minimal() +
  labs(title = "Water Temperature by Location")
  
  combined_data1$StartTime<-as.POSIXct(combined_data1$StartTime)
  
  combined_data1$date2<-ymd_hms(combined_data1$StartTime)
  
 combined_data1$Time1 <- format(combined_data1$date2, "%H:%M:%S")  # This extracts HMS as character
 
 
 #add just a time column to the transect sampled datafrave
Transect_samplingdata_cleanJune2025$Temp<-as.numeric(Transect_samplingdata_cleanJune2025$Temp)
    Transect_samplingdata_cleanJune2025$StartTime<-ymd_hms(Transect_samplingdata_cleanJune2025$StartTime)
    Transect_samplingdata_cleanJune2025$Time <- format(Transect_samplingdata_cleanJune2025$StartTime, "%H:%M:%S")  # This extracts HMS as character
   Transect_samplingdata_cleanJune2025<-Transect_samplingdata_cleanJune2025%>%filter(!is.na(Temp))
  
ggplot(Transect_samplingdata_cleanJune2025, aes(x = Time, y = Temp)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "Temperature Over Time",
       x = "Time",
       y = "Temperature (°C)") +
  facet_wrap(~Date)
  theme_minimal()   


 # plot with temp on the y and time on the x so we can see if there was change over the night 
 #or also as a group so were these areas aleasy cooler/ warmer 
 
 #think of it as a model 
  
  #boxplot of the spread of temp data iwthin a zone
  Transectdata_June52025$Temp<-as.numeric(Transectdata_June52025$Temp)
 
   ggplot(Transect_samplingdata_cleanJune2025, aes(x=Time, y=Temp, group=Polygon, color=Polygon))+
    geom_boxplot()+
     # coord_cartesian(xlim = c("17:00:00", "16:59:59")) +  # 5 PM to 5 PM next day
       scale_y_continuous(breaks = seq(0, 30, by = 1)) +  # Tick every 2 degrees
    facet_wrap(~Date)+
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
   

 #temp as the independent varaible, what explains the variation, time? polygon area? etc. what explains the variance
   
    ggplot(Transect_samplingdata_cleanJune2025, aes(x=Time, Temp, group=Polygon, color=Polygon, fill=Polygon))+
    geom_point()+
       scale_y_continuous(breaks = seq(0, 30, by = 1)) +  # Tick every 2 degrees
    facet_wrap(~Date)+
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   ggplotly(tempplot)

   #for whatever reason sampling event 2 have the east sampled in middle of the south stransects
   #would move it but the temp isreally low and wouldnt fit 
   
   
   ##add on aggregation temp data to this plot
 #add just a time column to the transect sampled datafrave
agg_temponly$Temp<-as.numeric(agg_temponly$Temp)
    agg_temponly$StartTime<-ymd_hms(agg_temponly$StartTime)
    agg_temponly$Time <- format(agg_temponly$StartTime, "%H:%M:%S")  # This extracts HMS as character
   agg_temponly<-agg_temponly%>%filter(!is.na(Temp))
  
   
plot1 <- ggplot() +
  geom_point(data = Transect_samplingdata_cleanJune2025,
             aes(x = Time, y = Temp, group = Polygon, fill = Polygon), 
             shape = 21, size = 5, color = "black", stroke = 1) +  # Shape 21 supports fill + border
  geom_point(data = agg_temponly,
             aes(x = Time, y = Temp, group = Polygon, fill = Polygon),
             shape = 25, size = 3, color = "black", stroke = 1) +  # Shape 25 is triangle-down
  facet_wrap(~Date) +
  scale_y_continuous(breaks = seq(0, 30, by = 1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


       