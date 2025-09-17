##figure plots for goldfish aggregation paper 

###FIGURE 1 receiver locations and area locaiton map - SEE ARCGIS 
#mapping for goldfish management report 
library(rgdal)
library(sp)
library(sf)
library(ggplot2)
library(rgdal)

###need to convert to lat long from UTMS or points
#location of the more detailed hamilton harbour map 
#shorelinemap <- readOGR(dsn = "U:/HH_shapefile/HH_outline.shp", 
 #                       layer = "HH_outline")


hamrecs<-read.csv("/ham_rec_locs_upto2024.csv")
#####need to sub in and use the receiver workbook data
hamrecs <- hamrecs %>%
  mutate(date = as.Date(deploy_date_time))

hamrecs <- hamrecs %>%
  mutate(year = year(deploy_date_time))
#need to assign groupings to the recs
hamrecs$rec_group_adjusted_new
hamrecs$rec_group_adjusted_new <- case_when(
  hamrecs$station %in% c("HAM-052", "HAM-053", "HAM-054", "HAM-055", "HAM-056", "HAM-057", "HAM-016", "HAM-051","HAM-036",
                               "HAM-082", "HAM-081") ~ "Bayfront area",
  hamrecs$station %in% c("HAM-030", "HAM-031", "HAM-042", "HAM-044", "HAM-065", "HAM-096", "HAM-098", "HAM-095", "HAM-094", "HAM-093", "HAM-088", "HAM-090", "HAM-092", "HAM-091", "HAM-089", "HAM-097", "HAM-030", "HAM-042") ~ "Cootes Paradise",
  hamrecs$station %in% c( "HAM-033", "HAM-059", "HAM-060", "HAM-061", "HAM-062", "HAM-064", "HAM-066", "HAM-067", "HAM-068", "HAM-086", "HAM-087") ~ "Grindstone",
  hamrecs$station %in% c("HAM-048", "HAM-018", "HAM-028", "HAM-037", "HAM-024", "HAM-026", "HAM-072", "HAM-073", "HAM-074", "HAM-075", "HAM-076", "HAM-079", "HAM-078", 
                               "HAM-077", "HAM-085", "HAM-084", "HAM-083", "HAM-005", "HAM-011", "HAM-027", "HAM-023", "HAM-080") ~ "West End",
  hamrecs$station %in% c("HAM-008", "HAM-010", "HAM-035") ~ "East End",
  hamrecs$station %in% c("HAM-013", "HAM-003", "HAM-002", "HAM-012", "HAM-045", "HAM-006", "HAM-025", "HAM-058", "HAM-009", "HAM-008", "HAM-034", "HAM-035") ~ "East End",
  hamrecs$station %in% c("HAM-022") ~ "Outside Harbour",
  hamrecs$station %in% c("HAM-046", "HAM-047", "HAM-007", "HAM-015", "HAM-014", "HAM-001") ~ "North shore",
  hamrecs$station %in% c("HAM-021", "HAM-004", "HAM-017", "HAM-020", "HAM-019", "HAM-039", "HAM-041") ~ "Central",
  hamrecs$station %in% c("HAM-070", "HAM-099" ,"HAM-071", "HAM-063", "HAM-032", "HAM-043", "HAM-029") ~ "Carrolls Bay",
  TRUE ~ NA_character_ # Default to NA if no match
)


#check NA recs and assign them to a location 

###combine 2022 and 2023 
combined_recs <- bind_rows(unique_receivers_adjusted2022, unique_receivers_adjusted2023)

combined_recs_final <- combined_recs %>%
  group_by(station_no, deploy_lat , deploy_long , rec_group) %>%
  summarise(
    years_present = list(year),
    year_status = case_when(
      length(year) == 2 ~ "Both_Years",
      2022 %in% year ~ "2022_Only", 
      2023 %in% year ~ "2023_Only"
    ),
    has_2022 = as.integer(2022 %in% year),
    has_2023 = as.integer(2023 %in% year),
    .groups = 'drop'
  )

combined_recs_final_clean <- combined_recs_final %>% 
  select_if(~!is.list(.))

#write.csv(combined_recs_final_clean, "recs_combined22223_forGIS.csv")

###FIGURE 2##################
##completed###
##light category by depth 
##bubble plot that shows shallow aggregation filter (<75% detections and depth of <2m)
##figure 2A light category by depth for spring time March 15th - May 15th 2022 and 2023 years 

library(suncalc)
library(dplyr)

goldfish204_2 <- goldfish204_2 %>%
  mutate(date = as.Date(detection_timestamp_EST))
#original used for managment plan due April 1 2025
#spring2022_goldfish<-goldfish204_2[goldfish204_2$date >= "2022-03-01" & 
 #                                              goldfish204_2$date <= "2022-05-31",]


spring_goldfish_22_23 <- goldfish204_2[
  (goldfish204_2$date >= "2022-03-15" & goldfish204_2$date <= "2022-05-15") |
  (goldfish204_2$date >= "2023-03-15" & goldfish204_2$date <= "2023-05-15"), ]

spring_goldfish_22_23

#just keep pressure tage 

spring_goldfish_22_23Ponly <- subset(spring_goldfish_22_23, SensorType == "P")

###use getSunlightTimes in suncalc package
spring_goldfish_22_23Ponly$lat<-spring_goldfish_22_23Ponly$deploy_lat
spring_goldfish_22_23Ponly$lon<-spring_goldfish_22_23Ponly$deploy_long
hmmm<-getSunlightTimes(data=spring_goldfish_22_23Ponly,  keep=c("nauticalDawn", "goldenHourEnd", "goldenHour", "nauticalDusk","night","nightEnd"), tz = "America/Toronto")
head(hmmm)

hmmm<-subset(hmmm, select=c("nauticalDawn", "goldenHourEnd", "goldenHour", "nauticalDusk","night","nightEnd"))
spring_goldfish_22_23_1<-cbind(spring_goldfish_22_23Ponly,hmmm)

##below we make 4 categories but could easily customize
spring_goldfish_22_23_1 <- spring_goldfish_22_23_1 %>%
  mutate(
    Lightcat = case_when(
      detection_timestamp_EST >= nightEnd & detection_timestamp_EST < goldenHourEnd ~ "Dawn",
      detection_timestamp_EST >= goldenHourEnd & detection_timestamp_EST < goldenHour ~ "Day",
      detection_timestamp_EST >= goldenHour & detection_timestamp_EST < night ~ "Dusk",
      TRUE ~ "Night"
    )
  )
#spring_goldfish_22_23_1 <- spring_goldfish_22_23_1[, !duplicated(names(spring_goldfish_22_23_1))]


#### determine light category durations for each day. This helps to get relative or weighted detections at time of day for later analyses
detections.time<-spring_goldfish_22_23_1 %>% group_by(date)%>% summarise(nightEnd=min(nightEnd),night=min(night),goldenHourEnd=min(goldenHourEnd), goldenHour=min(goldenHour))
##gets the duration of each light category for each day as it changes throughout the year ##
detections.time$daytime<-as.numeric(detections.time$goldenHour-detections.time$goldenHourEnd)
### didn't work since wrong day it is subtracting from.
#detections.time$nighttime<-as.numeric(detections.time$night-detections.time$nightEnd)
detections.time$nighttime<- as.numeric(lead(detections.time$nightEnd)-detections.time$night)
detections.time$dawntime<-as.numeric(detections.time$goldenHourEnd-detections.time$nightEnd)
detections.time$dusktime<-as.numeric(detections.time$night-detections.time$goldenHour)

detections.time <-detections.time[,-c(2:5)]

sum(is.na(detections.time))
###check to see what the amount should be for that nighttime duration
detections.time$nighttime[is.na(detections.time$nighttime)] <- 11.19

detections_updated<-left_join(spring_goldfish_22_23_1,detections.time, by=c("date"))
detections_updated$Lightcat
colnames(detections_updated)


detections_updated$detection_timestamp_EST <- as.POSIXct(detections_updated$detection_timestamp_EST, format = "%Y-%m-%d %H:%M:%S")

library(lubridate)
detections_updated <- detections_updated %>%
  mutate(time_of_day = hour(detection_timestamp_EST))  # Extracts hour (0–23)

library(dplyr)

#depth_proportions <- detections_updated %>%
 # group_by(time_of_day, Sensor.Val) %>% 
  #summarise(count = n(), .groups = "drop") %>%
  #group_by(time_of_day) %>%
  #mutate(proportion = count / sum(count)) %>%
  #ungroup()

# View result
#head(depth_proportions)

library(ggplot2)

#ggplot(depth_proportions, aes(x = time_of_day, y = Sensor.Val, fill = proportion)) +
#  geom_tile() +
#  geom_hline(yintercept = 2, color = "red", linetype = "dashed", linewidth = 1) +
#  scale_fill_viridis_c(name = "Proportion") +  # Use Viridis color scale for better readability
#  scale_y_reverse(limits=c(6,0))+
#  labs(x = "Time of Day (Hours)", y = "Depth", title = "Proportion of Time Spent at Each Depth by Hour March 1st - May 31st 2022") +
#  theme_minimal() +
#  theme(axis.text.y = element_text(size = 12),  # Adjust text size for readability
 #       axis.text.x = element_text(size = 12),
  #      legend.position = "right")

####
head(depth_proportions_lightcat)

depth_proportions_lightcat <- detections_updated %>%
  group_by(Lightcat, Sensor.Val) %>% 
  summarise(count = n(), .groups = "drop") %>%
  group_by(Lightcat) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

library(ggplot2)


excluded<-subset(depth_proportions_lightcat,
       is.na(proportion) | Sensor.Val > 6 | Sensor.Val < 0)
#some are coming in as negative change them to be zeros 
depth_proportions_lightcat <- depth_proportions_lightcat %>%
  mutate(Sensor.Val = ifelse(Sensor.Val < 0, 0.1, Sensor.Val))

library(dplyr)
library(tidyr)

light <- ggplot(depth_proportions_lightcat, aes(x = Lightcat, y = Sensor.Val, fill = proportion)) +
  geom_tile(height = 0.15) +  # Manually set tile height to cover gaps
 # geom_hline(yintercept = 1.5, color = "red", linetype = "dashed", linewidth = 1) +
  scale_fill_viridis_c(name = "Proportion") +
  scale_y_reverse(limits = c(5,0)) +
  labs(x = "Light Category", y = "Depth (m)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))
light

##figure 2 panel B
##bubble plot with the shallow aggregations filter on it showing entire timeframe on the x. 
#in figure caption can add see supp figure X for a bubble 

##figure showing the shallow aggregations dataframe
#<2m and >75% of detections on rec groups 

#data filtering 
aggregations_props<-dailyprops_recgroups_alldata_plusavgdepth_FINAL %>%
  filter(proportion >=75)


shallowaggregations <- aggregations_props %>%
  filter(avg_depth <= 2.0)


final_locations_withavgdepth_Proprotions
##updated
aggregations_props<-final_locations_withavgdepth_Proprotions %>%
  filter(proportion >=75)


shallowaggregations <- aggregations_props %>%
  filter(avg_depth <= 2.0)
##remake the plots with updated data
##plot for heat map with recs on the y and date on the x proportional data plotted over time 

desired_order <- c("Cootes Paradise","Bayfront area", "Grindstone", "Carrolls Bay", "West End", "Central", "North shore", "East End")  # Replace with your specific group names






final_locations_withavgdepth_Proprotions$rec_group_adjusted_new <- factor(final_locations_withavgdepth_Proprotions$rec_group_adjusted_new, levels = desired_order)
#change to proportion 
final_locations_withavgdepth_Proprotions$proportion1 <- final_locations_withavgdepth_Proprotions$proportion/100


heatmap2 <- ggplot(final_locations_withavgdepth_Proprotions, aes(x = date, y = rec_group_adjusted_new, fill = proportion1)) +
  geom_tile() +
  scale_fill_viridis_c() +
  
  # Set bi-monthly breaks with year in label
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  
  # Set the Y-axis as a factor for station numbers
  scale_y_discrete(breaks = unique(final_locations_withavgdepth_Proprotions$rec_group_adjusted_new)) +
  
  labs(title = "",
       x = "Date",
       y = "",
       fill = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=12), 
        axis.text.y = element_text(size=12))
        # Rotate X-axis labels for readability

heatmap2


##plots for depth bubble proportional plots 

final_locations_withavgdepth_Proprotions$date <- as.Date(final_locations_withavgdepth_Proprotions$date, format = "%Y-%m-%d")  # Adjust format if necessary
final_locations_withavgdepth_Proprotions$rec_group_adjusted_new <- as.factor(final_locations_withavgdepth_Proprotions$rec_group_adjusted_new)

colnames(final_locations_withavgdepth_Proprotions)
##keep colors same at the rec grouping map 
unique(dailyprops_recgroups_alldata_plusavgdepth_FINAL$rec_group_adjusted_new)

receiver_colorgroupings <- c(
  "North shore" = "#999999",
  "West End" = "#56B4E9",
  "East End" = "#D55E00",
  "Central" = "#F0E442",
  "Carrolls Bay" = "#E69F00",
  "Grindstone" = "#009E73",
  "Bayfront area" = "#CC79A7", 
  "Cootes Paradise"="#000000", 
  "Outside Harbour" = "#0072B2"
)


#still have a bunch of NA depths coming up in this dataframe....

#filter data to just include dates used here

shallowaggregations$proportions1<-shallowaggregations$proportion/100

SA<-ggplot(shallowaggregations, aes(
    x = date, 
    y = avg_depth, 
    color = rec_group_adjusted_new, 
    size = proportions1)) +
  geom_point(alpha = 0.7) +  
  scale_y_reverse() +  
  scale_color_manual(values = receiver_colorgroupings, 
                     name = "Receiver Group", 
                     guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_size_continuous(
    breaks = c(0.75, 0.9, 0.1),      # thresholds
    labels = c("0.75", "0.9", "1.0"),
    range = c(3, 6),             # controls actual bubble size
    name = "Proportion"
  ) +
  theme_minimal() +
  labs(
    x = "Date",
    y = "Depth (m)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size=14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

SA
#need to stake the light depth and shallow aggregation bubble plot to be on one figure
library(patchwork)
stacked_plot <- light / SA 
stacked_plot

stacked_plotflip <- SA / light 
stacked_plotflip


library(plotly)
plotly(SA)
shallowaggregations2024<-filter


shallowaggregations2024 <- shallowaggregations[
  (shallowaggregations$date >= "2024-01-01" )]

#figures for supplemetary materials
##bubble plot with all daya al depths 



#clip data to just have 
#clip off  until January 2024

#changing out to new dataframe saved as write.csv(final_locations_withavgdepth_Proprotions,"dailyprops_avgdepth_droppedTtagsSept25.csv")

#alldata_clipped <- final_locations_withavgdepth_Proprotions %>%
#  filter(date <= as.Date("2023-12-31"))

alldata_clipped1 <- final_locations_withavgdepth_Proprotions %>%
  filter(date >= as.Date("2022-01-01"))

alldata_clipped15 <- alldata_clipped1 %>%
  filter(avg_depth <= 15.0)

shallowaggregations <- aggregations_props %>%
  filter(avg_depth <= 2.0)

alldata_clipped1$proportion1 <- alldata_clipped1$proportion/100

ggplot(alldata_clipped1, aes(x = date, y = avg_depth, color = rec_group_adjusted_new , size = proportion1)) +
  geom_point(alpha = 0.7) +  
  scale_y_reverse() +  
    scale_color_manual(values = receiver_colorgroupings, 
                     name = "Receiver Group", 
                     guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
#  scale_color_manual(values = rainbow(length(unique(shallow_data$rec_group)))) +  
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme_minimal() +
  labs(
    title = "",
    x = "Date",
    y = "Depth (m)",
    color = "Receiver Group",
    size = "Proportion"
  ) +
  theme(
  axis.text.x = element_text(angle = 45, hjust = 1, size=14),
  axis.text.y = element_text(size = 14),  # Fix `y.axis.text`
  axis.title.x = element_text(size = 14),  # Fix `x.title.text`
  axis.title.y = element_text(size = 14),   # Fix `y.title.text`
   legend.title = element_text(size = 14),  # Increases legend title size  
    legend.text = element_text(size = 14)  # Increases legend item text size  
  )


#how are there grindstone detections in June 2023 with depths of 5-12m


### targeting aggregations in Carrolls Bay over the month of April 2025
HH_gcmap <- st_read("/HH_WaterLines_02June2023.shp")
HH_gcmap <- st_transform(HH_gcmap, crs = 4326)  # Convert to lat/lon if needed
#
ggplot(HH_gcmap) +
  geom_sf(fill = "lightblue", color = "black", size = 0.5) +
  theme_minimal() +
  theme(axis.text = element_text(size = 8))

#GCzoom in 
x_limits_GC <- c(-79.891, -79.88)  # Define your desired range for x-axis
y_limits_GC <- c(43.281, 43.291)  # Define your desired range for y-axis


################
# Plot the shapefile with specified x and y limits
library(ggspatial)
library(plotly)
library(readxl)

CarrollsBay<-ggplot(HH_gcmap) +
  geom_sf(fill="lightgrey", color="black") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC) +
  theme_minimal()

plot(HH_gcmap)

plot(CarrollsBay)

#setwd("C:/Users/TURNERN/Documents/For Github/Aggregations-Goldfish-HH")
#add in transect sampling and elevation map
CB_transects <- st_read("data/GF_FieldPlanning_2025/GF_CarrollsBayTransects_Apr2025.shp")
CB_boundary<-st_read("data/GF_FieldPlanning_2025/GF_CarrollsZoneBoundaries_Apr2025.shp")
CB_points<-st_read("data/GF_Points/GF_CarrollsBay_Transect_points.shp")
CB_label<-st_read("data/zonelabel/GF_CarrollsZoneBoundaries_Apr2025Anno.shp")


#load in aggregations data file and plot on shape file (HH_shapefile code)

Aggdata_June52025 <- read_excel("data/raw/Aggdata_June52025.xlsx")

#filter out to just show all catch of aggregations so >=5

Aggdata_June52025_onlyaggs<-Aggdata_June52025 %>%
  filter(Total >=5)

noneaggs_2025<-Aggdata_June52025 %>%
  filter(Total <5)


#location of the receivers - from arc gis after manual edits
recs2223 <- read_excel("data/raw/recs_edit2223.xls")


receiver_colorgroupings <- c(
  "North shore" = "#999999",
  "West End" = "#56B4E9",
  "East End" = "#D55E00",
  "Central" = "#F0E442",
  "Carrolls Bay" = "#E69F00",
  "Grindstone" = "#009E73",
  "Bayfront area" = "#CC79A7", 
  "Cootes Paradise"="#000000", 
  "Outside Harbour" = "#0072B2"
)


#########
# First, create the first date identifier
# First, create the first date identifier
first_date <- min(Aggdata_June52025_onlyaggs$Date)

# Create filtered datasets for first plot only
first_date_data <- Aggdata_June52025_onlyaggs[Aggdata_June52025_onlyaggs$Date == first_date, ]

# Add Date column to receiver data so it appears in first facet only
recs_with_date <- recs2223
recs_with_date$Date <- first_date

# Add Date column to labels so they appear in first facet only
labels_with_date <- CB_label
labels_with_date$Date <- first_date



names(recs_with_date)

# Check for missing values in the fill column
summary(recs_with_date$receiver_colorgroupings)

# Check the structure
str(recs_with_date$receiver_colorgroupings)



CarrollsBay3 <- ggplot() +
  geom_sf(data = HH_gcmap, fill = "lightblue", color = "black", linewidth=2) +
  geom_sf(data = CB_transects, linewidth = 1, color = "grey30") +
  geom_sf(data = CB_boundary, fill = alpha("darkolivegreen3", 0.3, linewidth=1)) +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC) +
  
  # Labels only on first plot
#  geom_sf_text(data = labels_with_date, aes(label = TextString), size = 4, fontface="bold") +
  
  # North arrow only on first plot
  annotation_north_arrow(location = "tr", which_north = "true", 
                        style = north_arrow_fancy_orienteering,
                        data = first_date_data) +
  
  # Main data points on all panels
  geom_point(data = Aggdata_June52025_onlyaggs, 
             aes(x = Lon, y = Lat), 
             size = 3.5, 
             fill = "gold1",      
             color = "black",     
             shape = 23) +
  geom_point(data=noneaggs_2025, 
             aes(x=Lon, y=Lat) ,
             size=2,
             fill="firebrick", 
             color="black", 
            shape=21)+
  
  # Receiver locations only on first plot
  geom_point(data = recs_with_date,
             aes(x = deploy_long, y = deploy_lat, 
                 fill = rec_group,
                 shape = factor(year_status)), 
             size = 4,
             color = "black",
             stroke = 0.8) +
    scale_fill_manual(values = receiver_colorgroupings) +
  scale_shape_manual(values = c("2022_Only" = 22, "2023_Only" = 24, "Both_Years" = 21)) +
  facet_wrap(~Date) +
  
  # Scale bar only on first plot
  annotation_scale(location = "bl", data = first_date_data) +
  
guides(fill = "none", shape = "none") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 12), 
      axis.text.y = element_text(size = 12),  # Try larger size
      strip.text = element_text(size = 12),
      legend.position = "none")

CarrollsBay3

