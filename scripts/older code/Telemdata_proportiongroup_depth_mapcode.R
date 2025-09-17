#mapping for goldfish management report 
library(rgdal)
library(sp)
library(sf)
library(ggplot2)
library(rgdal)

###need to c

HHmap_GC
hamrecs<-read.csv("U:/Goldfish_Telem_Jan25/Telem2025/ham_rec_locs_upto2024.csv")
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


unique(hamrecs$rec_group_adjusted_new)

hamrecs_2021 <- hamrecs %>%
  filter(year == 2021)

#2022 data


goldfish_recs2021<-hamrecs_2021 %>%
  group_by(station_no) %>%
  summarize(
    deploy_lat = mean(deploy_lat, na.rm = TRUE),  # Calculate mean latitude
    deploy_long = mean(deploy_long, na.rm = TRUE),  # Calculate mean longitude
    rec_group = first(rec_group_adjusted_new)  # Keep the first rec_group
  ) %>%
  ungroup()


unique_receivers_adjusted1 <- goldfish_recs2021 %>%
  group_by(station_no) %>%
  summarize(
    deploy_lat = mean(deploy_lat, na.rm = TRUE),  # Calculate mean latitude
    deploy_long = mean(deploy_long, na.rm = TRUE),  # Calculate mean longitude
    rec_group = first(rec_group)  # Keep the first rec_group
  ) %>%
  ungroup()

receiver_colors2021 <- c(
  "North shore" = "darkblue",
  "West End" = "darkred",
  "East End" = "darkgreen",
  "Central" = "purple3",
  "Carrolls Bay" = "darkorange",
  "Grindstone" = "pink3",
  "Bayfront area" = "yellow3", 
  "Cootes Paradise"="darkmagenta",
  "Outside Harbour" = "black"
)

Ham_rec_colored2021 <- HHmap +
  # Plot receiver points with different colors for each group
  geom_point(data = unique_receivers_adjusted1, 
             aes(x = deploy_long, y = deploy_lat, color = factor(rec_group)), 
             size = 4, shape = 16) +  # Adds points to the map (shape 16 = filled circle)
  
  # Add station number as labels above the points
 # geom_text(data = unique_receivers_adjusted1, 
  #          aes(x = deploy_long, y = deploy_lat, label = station_no), 
   #         size = 4, nudge_y = 0.002, color="black") +  # Moves text slightly upwards
  
  # Use a custom rainbow color palette for the receiver groups
 scale_color_manual(values = receiver_colors2021, 
                     name = "Receiver Group", 
                     guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  # Labels and theme
  labs(title = "2021",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()+
   theme(plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "none")  # Removes legend

# Display the map
Ham_rec_colored2021

# Subset the dataframe to include only rows from 2022
hamrecs_2022 <- hamrecs %>%
  filter(year == 2022)

#2022 data


goldfish_recs2022<-df_2022 %>%
  group_by(station_no) %>%
  summarize(
    deploy_lat = mean(deploy_lat, na.rm = TRUE),  # Calculate mean latitude
    deploy_long = mean(deploy_long, na.rm = TRUE),  # Calculate mean longitude
    rec_group = first(rec_group_adjusted_new)  # Keep the first rec_group
  ) %>%
  ungroup()


unique_receivers_adjusted <- goldfish_recs2022 %>%
  group_by(station_no) %>%
  summarize(
    deploy_lat = mean(deploy_lat, na.rm = TRUE),  # Calculate mean latitude
    deploy_long = mean(deploy_long, na.rm = TRUE),  # Calculate mean longitude
    rec_group = first(rec_group)  # Keep the first rec_group
  ) %>%
  ungroup()

unique(goldfish_recs2022$rec_group)

receiver_colors2022 <- c(
  "North shore" = "darkblue",
  "West End" = "darkred",
  "East End" = "darkgreen",
  "Central" = "purple3",
  "Carrolls Bay" = "darkorange",
  "Grindstone" = "pink3",
  "Bayfront area" = "yellow3", 
  "Cootes Paradise"="darkmagenta",
  "Outside Harbour" = "black"
)
Ham_rec_colored2022 <- HHmap +
  # Plot receiver points with different colors for each group
  geom_point(data = unique_receivers_adjusted, 
             aes(x = deploy_long, y = deploy_lat, color = factor(rec_group)), 
             size = 4, shape = 16) +  # Adds points to the map (shape 16 = filled circle)
  
  # Add station number as labels above the points
 # geom_text(data = unique_receivers_adjusted, 
  #          aes(x = deploy_long, y = deploy_lat, label = station_no, color = factor(rec_group)), 
   #         size = 4, nudge_y = 0.002, color="black") +  # Moves text slightly upwards
  
  # Use a custom rainbow color palette for the receiver groups
 scale_color_manual(values = receiver_colors2022, 
                     name = "Receiver Group", 
                     guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  # Labels and theme
  labs(title = "2022",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()+ theme(plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "none")  # Removes legend

# Display the map
Ham_rec_colored2022

#now do 2023
df_2023 <- hamrecs %>%
  filter(year == 2023)

goldfish_recs2023<-df_2023 %>%
  group_by(station_no) %>%
  summarize(
    deploy_lat = mean(deploy_lat, na.rm = TRUE),  # Calculate mean latitude
    deploy_long = mean(deploy_long, na.rm = TRUE),  # Calculate mean longitude
    rec_group = first(rec_group_adjusted_new)  # Keep the first rec_group
  ) %>%
  ungroup()


unique_receivers_adjusted3 <- goldfish_recs2023 %>%
  group_by(station_no) %>%
  summarize(
    deploy_lat = mean(deploy_lat, na.rm = TRUE),  # Calculate mean latitude
    deploy_long = mean(deploy_long, na.rm = TRUE),  # Calculate mean longitude
    rec_group = first(rec_group)  # Keep the first rec_group
  ) %>%
  ungroup()

unique(unique_receivers_adjusted3$rec_group)

receiver_colors2023 <- c(
  "North shore" = "darkblue",
  "West End" = "darkred",
  "East End" = "darkgreen",
  "Central" = "purple3",
  "Carrolls Bay" = "darkorange",
  "Grindstone" = "pink3",
  "Bayfront area" = "yellow3", 
  "Cootes Paradise"="darkmagenta"
)

Ham_rec_colored2023 <- HHmap +
  # Plot receiver points with different colors for each group
  geom_point(data = unique_receivers_adjusted3, 
             aes(x = deploy_long, y = deploy_lat, color = factor(rec_group)), 
             size = 4, shape = 16) +  # Adds points to the map (shape 16 = filled circle)
  
  # Add station number as labels above the points
 # geom_text(data = unique_receivers_adjusted, 
  #          aes(x = deploy_long, y = deploy_lat, label = station_no, color = factor(rec_group)), 
   #         size = 4, nudge_y = 0.002, color="black") +  # Moves text slightly upwards
  
  # Use a custom rainbow color palette for the receiver groups
 scale_color_manual(values = receiver_colors2022, 
                     name = "Receiver Group", 
                     guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  # Labels and theme
  labs(title = "2023",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()+
   theme(plot.title = element_text(hjust = 0, vjust = 0),
        legend.position = "none")  # Removes legend

# Display the map
Ham_rec_colored2023

#should also do 2024

#now do 2023
df_2024 <- hamrecs %>%
  filter(year == 2024)

goldfish_recs2024<-df_2024 %>%
  group_by(station_no) %>%
  summarize(
    deploy_lat = mean(deploy_lat, na.rm = TRUE),  # Calculate mean latitude
    deploy_long = mean(deploy_long, na.rm = TRUE),  # Calculate mean longitude
    rec_group = first(rec_group_adjusted_new)  # Keep the first rec_group
  ) %>%
  ungroup()


unique_receivers_adjusted4 <- goldfish_recs2024 %>%
  group_by(station_no) %>%
  summarize(
    deploy_lat = mean(deploy_lat, na.rm = TRUE),  # Calculate mean latitude
    deploy_long = mean(deploy_long, na.rm = TRUE),  # Calculate mean longitude
    rec_group = first(rec_group)  # Keep the first rec_group
  ) %>%
  ungroup()

unique(unique_receivers_adjusted3$rec_group)

Ham_rec_colored2024 <- HHmap +
  # Plot receiver points with different colors for each group
  geom_point(data = unique_receivers_adjusted4, 
             aes(x = deploy_long, y = deploy_lat, color = factor(rec_group)), 
             size = 4, shape = 16) +  # Adds points to the map (shape 16 = filled circle)
  
  # Add station number as labels above the points
 # geom_text(data = unique_receivers_adjusted, 
  #          aes(x = deploy_long, y = deploy_lat, label = station_no, color = factor(rec_group)), 
   #         size = 4, nudge_y = 0.002, color="black") +  # Moves text slightly upwards
  
  # Use a custom rainbow color palette for the receiver groups
 scale_color_manual(values = receiver_colors2022, 
                     name = "Receiver Group", 
                     guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  # Labels and theme
  labs(title = "2024",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

# Display the map
Ham_rec_colored2024

#arrange them side by side 
final_plot <-Ham_rec_colored2021+ Ham_rec_colored2022 + Ham_rec_colored2023 + Ham_rec_colored2024

# Print the final combined plot
final_plot




################################################################
#############proportions and depth final 
library(dplyr)
library(zoo)
library(tidyr)
library(ggplot2)


# Sample data structure
# acoustic_data <- data.frame(
#   animal_id = c("A", "A", "A", "B", "B"),
#   detection_timestamp = as.POSIXct(c("2024-09-10 12:00:00", "2024-09-10 14:00:00", "2024-09-11 10:00:00",
#                                      "2024-09-10 09:00:00", "2024-09-12 15:00:00")),
#   station_no = c("S1", "S1", "S2", "S3", "S4")
# )


setwd("U:/Goldfish_Telem_Jan25/Telem2025/HH_goldfish")
goldfish204<-readRDS("HHGOLDFISH_detections_clipped_dead_filtered_firstwoweeksremoved_2015-2025_correct.rds")

goldfish204<-`HHGOLDFISH_detections_clipped_dead_filtered_firstwoweeksremoved_2015-2025_correct`
#remove animal ID and sub in that for the tag.sn
goldfish204_1 <- goldfish204[, -1]
colnames(goldfish204_1)[colnames(goldfish204_1) == "tag_serial_number"] <- "animal_id"
unique(goldfish204_1$animal_id)

###################
##few extra things to check 
#red hill creek receiver
#outer harbour receiver
#indian creek receiever
#check my filtered data and also 

#should we add cootes paradse fishway then aso to the GC style daily detectoin and # fish detected


#Extract the date from the timestamp
goldfish204_2 <- goldfish204_1 %>%
  mutate(date = as.Date(detection_timestamp_utc))

goldfish204_2$rec_group_adjusted_new

goldfish204_2$rec_group_adjusted_new <- case_when(
  goldfish204_2$station %in% c("HAM-052", "HAM-053", "HAM-054", "HAM-055", "HAM-056", "HAM-057", "HAM-016", "HAM-051","HAM-036",
                               "HAM-082", "HAM-081") ~ "Bayfront area",
  goldfish204_2$station %in% c("HAM-030", "HAM-031", "HAM-042", "HAM-044", "HAM-065", "HAM-096", "HAM-098", "HAM-095", "HAM-094", "HAM-093", "HAM-088", "HAM-090", "HAM-092", "HAM-091", "HAM-089", "HAM-097", "HAM-030", "HAM-042") ~ "Cootes Paradise",
  goldfish204_2$station %in% c( "HAM-033", "HAM-059", "HAM-060", "HAM-061", "HAM-062", "HAM-064", "HAM-066", "HAM-067", "HAM-068", "HAM-086", "HAM-087") ~ "Grindstone",
  goldfish204_2$station %in% c("HAM-048", "HAM-018", "HAM-028", "HAM-037", "HAM-024", "HAM-026", "HAM-072", "HAM-073", "HAM-074", "HAM-075", "HAM-076", "HAM-079", "HAM-078", 
                               "HAM-077", "HAM-085", "HAM-084", "HAM-083", "HAM-005", "HAM-011", "HAM-027", "HAM-023", "HAM-080") ~ "West End",
  goldfish204_2$station %in% c("HAM-008", "HAM-010", "HAM-035") ~ "East End",
  goldfish204_2$station %in% c("HAM-013", "HAM-003", "HAM-002", "HAM-012", "HAM-045", "HAM-006", "HAM-025", "HAM-058", "HAM-009", "HAM-008", "HAM-034", "HAM-035") ~ "East End",
  goldfish204_2$station %in% c("HAM-022") ~ "Outside Harbour",
  goldfish204_2$station %in% c("HAM-046", "HAM-047", "HAM-007", "HAM-015", "HAM-014", "HAM-001") ~ "North shore",
  goldfish204_2$station %in% c("HAM-021", "HAM-004", "HAM-017", "HAM-020", "HAM-019", "HAM-039", "HAM-041") ~ "Central",
  goldfish204_2$station %in% c("HAM-070", "HAM-099" ,"HAM-071", "HAM-063", "HAM-032", "HAM-043", "HAM-029") ~ "Carrolls Bay",
  TRUE ~ NA_character_ # Default to NA if no match
)

unique(goldfish204_2$rec_group_adjusted_new)
#filter the dataframe for NAs
NAs<-goldfish204_2 %>% filter(is.na(rec_group_adjusted_new))


goldfish204_2_sf <- st_as_sf(goldfish204_2, coords = c("deploy_long", "deploy_lat"), crs = 4326)  # assuming lat/long in WGS84 (EPSG:4326)


daily_location <- goldfish204_2 %>%
  group_by(animal_id, date) %>%
  count(rec_group_adjusted_new, sort = TRUE) %>%
  slice(1) %>%   # Get the station with the highest count
  ungroup() %>%
  dplyr::select(animal_id, date,rec_group_adjusted_new)


#Create a sequence of all dates for each individual
full_dates <- daily_location %>%
  group_by(animal_id) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%  # Fill in missing dates
  ungroup()%>% 
  dplyr::select(animal_id, date)

#Merge full dates with daily location data and carry forward the last known location
final_locations <- full_dates %>%
  left_join(daily_location, by = c("animal_id", "date")) %>%
  group_by(animal_id) %>%
  arrange(date) %>%
  mutate(rec_group_adjusted_new = zoo::na.locf(rec_group_adjusted_new, na.rm = FALSE)) %>%  # Carry forward the last known location
  ungroup()

#data with all carried forward locatoins= final_locations

#daily proportional activity

library(dplyr)
#Count the total number of individuals detected per day
total_per_day <- final_locations %>%
  group_by(date) %>%
  summarise(total_individuals = n_distinct(animal_id))

#Count the number of individuals detected at each receiver per day
count_per_receiver <- final_locations %>%
  group_by(date, rec_group_adjusted_new) %>%
  summarise(individuals_at_receiver = n_distinct(animal_id)) %>%
  ungroup()

#Join the two dataframes and calculate the proportion
proportions <- count_per_receiver %>%
  left_join(total_per_day, by = "date") %>%
  mutate(proportion = (individuals_at_receiver / total_individuals)*100)

# View the result
print(proportions)

#this dataframe proportions has daily proportioanl activity 

#adding daily depth to the dataframe of proportional activity?

#keep all P sensor tags for this one
goldfish204_2Ponly <- goldfish204_2 %>%
  filter(SensorType == "P")

daily_depth <- goldfish204_2Ponly %>%
  group_by(animal_id, date) %>%
  summarise(avg_depth = mean(Sensor.Val, na.rm = TRUE), .groups = "drop")

#now bind this with the other dataframe
library(dplyr)


#Create a sequence of all dates for each individual
full_dates_depth <- daily_depth %>%
  group_by(animal_id) %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%  # Fill in missing dates
  ungroup()%>% 
  dplyr::select(animal_id, date)

#Merge full dates with daily avg depth data and carry forward the last known avg depth if not one avalibale
final_locations_depth <- full_dates_depth %>%
  left_join(daily_depth, by = c("animal_id", "date")) %>%
  group_by(animal_id) %>%
  arrange(date) %>%
   mutate(avg_depth = zoo::na.locf(avg_depth, na.rm = FALSE)) %>%  # Carry forward the last known location
  ungroup()
final_locations_depth
#merge these two dataframes 

# Merging avg_depth into final_locations
final_locations_withavgdepth <- final_locations %>%
  left_join(final_locations_depth %>% select(animal_id, date, avg_depth), by = c("animal_id", "date"))

library(dplyr)

summary_df_depth_recgroup <- final_locations_withavgdepth %>%
  group_by(date, rec_group_adjusted_new) %>%
  summarize(avg_depth = mean(avg_depth, na.rm = TRUE), .groups = "drop")

write.csv(final_locations_withavgdepth_Proprotions,"dailyprops_avgdepth_droppedTtagsSept25.csv")

##now take the proprotions dataframe and add the avg depth to it based on the date and rec group
proportions


final_locations_withavgdepth_Proprotions <- proportions %>%
  left_join(summary_df_depth_recgroup, by = c("date", "rec_group_adjusted_new"))

write.csv(final_locations_withavgdepth_Proprotions, "dailyprops_recgroups_alldata_plusavgdepth_FINAL.csv")
final_locations_withavgdepth_Proprotions<-read.csv("dailyprops_recgroups_alldata_plusavgdepth_FINAL.csv")

#data filtering 
aggregations_props<-dailyprops_recgroups_alldata_plusavgdepth_FINAL %>%
  filter(proportion >=75)

shallowaggregations <- aggregations_props %>%
  filter(avg_depth <= 2.0)
##remake the plots with updated data
##plot for heat map with recs on the y and date on the x proportional data plotted over time 

desired_order <- c("Cootes Paradise","Bayfront area", "Grindstone", "Carrolls Bay", "West End", "Central", "North shore", "East End")  # Replace with your specific group names

final_locations_withavgdepth_Proprotions$rec_group_adjusted_new <- factor(final_locations_withavgdepth_Proprotions$rec_group_adjusted_new, levels = desired_order)

heatmap2 <- ggplot(final_locations_withavgdepth_Proprotions, aes(x = date, y = rec_group_adjusted_new, fill = proportion)) +
  geom_tile() +
  scale_fill_viridis_c() +
  
  # Set bi-monthly breaks with year in label
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  
  # Set the Y-axis as a factor for station numbers
  scale_y_discrete(breaks = unique(final_locations_withavgdepth_Proprotions$rec_group_adjusted_new)) +
  
  labs(title = "",
       x = "Date",
       y = "Receiver group adjusted",
       fill = "Proportion") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate X-axis labels for readability

heatmap2


##plots for depth bubble proportional plots 

final_locations_withavgdepth_Proprotions$date <- as.Date(final_locations_withavgdepth_Proprotions$date, format = "%Y-%m-%d")  # Adjust format if necessary
final_locations_withavgdepth_Proprotions$rec_group_adjusted_new <- as.factor(final_locations_withavgdepth_Proprotions$rec_group_adjusted_new)

colnames(final_locations_withavgdepth_Proprotions)
##keep colors same at the rec grouping map 
receiver_colors2023 <- c(
  "North shore" = "darkblue",
  "West End" = "darkred",
  "East End" = "darkgreen",
  "Central" = "purple3",
  "Carrolls Bay" = "darkorange",
  "Grindstone" = "pink3",
  "Bayfront area" = "yellow3", 
  "Cootes Paradise"="darkmagenta"
)


#still have a bunch of NA depths coming up in this dataframe....


ggplot(final_locations_withavgdepth_Proprotions, aes(x = date, y = avg_depth , color = rec_group_adjusted_new)) +
  # Add a shaded rectangle for depths ≤2m
  annotate("rect", xmin = min(final_locations_withavgdepth_Proprotions$date), xmax = max(final_locations_withavgdepth_Proprotions$date), 
           ymin = 0, ymax = 2, fill = "darkgray", alpha = 0.4) +
  
  # Scatterplot of all depth points
  geom_point(alpha = 0.5, size = 2) +  
  scale_y_reverse() +  # Reverse y-axis so surface is at the top
   scale_color_manual(values = receiver_colors2023, 
                     name = "Receiver Group", 
                     guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  #scale_color_manual(values = rainbow(length(unique(final_locations_withavgdepth_Proprotions$rec_group_adjusted_new)))) +  # Rainbow colors
   scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  # Use colorblind-friendly colors
  theme_minimal() +
  # Labels
  labs(
    title = "Depth Over Time by Receiver Group",
    x = "Date",
    y = "Depth (m)",
    color = "Receiver Group"
  ) +
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# here we can see the grindstone pre spawn staging 

#lets just look at everything thats at a catchable depth
shallow_data <- final_locations_withavgdepth_Proprotions %>%
  filter(avg_depth <= 2.0)

#clip data to just have 

ggplot(shallow_data, aes(x = date, y = avg_depth, color = rec_group_adjusted_new , size = proportion)) +
  geom_point(alpha = 0.5) +  
  scale_y_reverse() +  
    scale_color_manual(values = receiver_colors2023, 
                     name = "Receiver Group", 
                     guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
#  scale_color_manual(values = rainbow(length(unique(shallow_data$rec_group)))) +  
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme_minimal() +
  labs(
    title = "Depth Over Time by Receiver Group",
    x = "Date",
    y = "Depth (m)",
    color = "Receiver Group",
    size = "Proportion"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


shallow_data_proportion50<-shallow_data %>%
  filter(proportion >=50)


ggplot(shallow_data, aes(x = date, y = avg_depth , color = rec_group)) +
  # Add a shaded rectangle for depths ≤2m
 # annotate("rect", xmin = min(shallow_data$date), xmax = max(props_avgdepth$date), 
  #         ymin = 0, ymax = 2, fill = "darkgray", alpha = 0.4) +
  
  # Scatterplot of all depth points
  geom_point(alpha = 0.5, size = 2) +  
  scale_y_reverse() +  # Reverse y-axis so surface is at the top
  scale_color_manual(values = rainbow(length(unique(shallow_data$rec_group)))) +  # Rainbow colors
   scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  # Use colorblind-friendly colors
  theme_minimal() +
  # Labels
  labs(
    title = "Depth Over Time by Receiver Group",
    x = "Date",
    y = "Depth (m)",
    color = "Receiver Group"
  ) +
  
  # Rotate x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###########depth


library(suncalc)
library(dplyr)

goldfish204_1 <- goldfish204_1 %>%
  mutate(date = as.Date(detection_timestamp_EST))

spring2022_goldfish<-goldfish204_1[goldfish204_1$date >= "2022-03-01" & 
                                               goldfish204_1$date <= "2022-05-31",]

###use getSunlightTimes in suncalc package
spring2022_goldfish$lat<-spring2022_goldfish$deploy_lat
spring2022_goldfish$lon<-spring2022_goldfish$deploy_long
hmmm<-getSunlightTimes(data=spring2022_goldfish,  keep=c("nauticalDawn", "goldenHourEnd", "goldenHour", "nauticalDusk","night","nightEnd"), tz = "America/Toronto")


hmmm<-subset(hmmm, select=c("nauticalDawn", "goldenHourEnd", "goldenHour", "nauticalDusk","night","nightEnd"))
spring2022_goldfish_1<-cbind(spring2022_goldfish,hmmm)
beep(5)
##below we make 4 categories but could easily customize
spring2022_goldfish_1$Lightcat <-ifelse(spring2022_goldfish_1$detection_timestamp_EST < spring2022_goldfish_1$nightEnd, "Night",
                             ifelse(spring2022_goldfish_1$detection_timestamp_EST < spring2022_goldfish_1$goldenHourEnd, "Dawn",
                                    ifelse(spring2022_goldfish_1$detection_timestamp_EST < spring2022_goldfish_1$goldenHour, "Day",
                                           ifelse(spring2022_goldfish_1$detection_timestamp_EST < spring2022_goldfish_1$night, "Dusk","Night")))) 


###check a day for each light category 
# Check a specific day for the light category cutoffs
specific_day <- spring2022_goldfish_1[spring2022_goldfish_1$date == "2022-03-01", ]

# Plot the key times for that day
library(ggplot2)

ggplot(specific_day) +
  geom_point(aes(x = nauticalDawn, y = 1), color = "blue", size = 4) +
  geom_point(aes(x = goldenHourEnd, y = 2), color = "orange", size = 4) +
  geom_point(aes(x = goldenHour, y = 3), color = "yellow", size = 4) +
  geom_point(aes(x = nauticalDusk, y = 4), color = "purple", size = 4) +
  geom_point(aes(x = night, y = 5), color = "red", size = 4) +
  geom_point(aes(x = nightEnd, y = 6), color = "green", size = 4) +
  scale_y_continuous(breaks = NULL) +
  labs(title = "Sunlight Times for March 1st, 2022",
       x = "Time of Day", y = "")


dusk_data <- filter(spring2022_goldfish_1, Lightcat == "Dusk")

# Find the minimum and maximum time within the 'Dusk' category
min_dusk_time <- min(dusk_data$detection_timestamp_EST, na.rm = TRUE)
max_dusk_time <- max(dusk_data$detection_timestamp_EST, na.rm = TRUE)
###
night_data <- filter(spring2022_goldfish_1, Lightcat == "Night")

# Find the minimum and maximum time within the 'Dusk' category
min_night_time <- min(night_data$detection_timestamp_EST, na.rm = TRUE)
max_night_time <- max(night_data$detection_timestamp_EST, na.rm = TRUE)


#### determine light category durations for each day. This helps to get relative or weighted detections at time of day for later analyses
detections.time<-spring2022_goldfish_1 %>% group_by(date)%>% summarise(nightEnd=min(nightEnd),night=min(night),goldenHourEnd=min(goldenHourEnd), goldenHour=min(goldenHour))
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

detections_updated<-left_join(spring2022_goldfish_1,detections.time, by=c("date"))
detections_updated$Lightcat
colnames(detections_updated)


detections_updated$detection_timestamp_EST <- as.POSIXct(detections_updated$detection_timestamp_EST, format = "%Y-%m-%d %H:%M:%S")

library(lubridate)
detections_updated <- detections_updated %>%
  mutate(time_of_day = hour(detection_timestamp_EST))  # Extracts hour (0–23)

library(dplyr)

depth_proportions <- detections_updated %>%
  group_by(time_of_day, Sensor.Val) %>% 
  summarise(count = n(), .groups = "drop") %>%
  group_by(time_of_day) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# View result
head(depth_proportions)

library(ggplot2)

ggplot(depth_proportions, aes(x = time_of_day, y = Sensor.Val, fill = proportion)) +
  geom_tile() +
  geom_hline(yintercept = 2, color = "red", linetype = "dashed", linewidth = 1) +
  scale_fill_viridis_c(name = "Proportion") +  # Use Viridis color scale for better readability
  scale_y_reverse(limits=c(6,0))+
  labs(x = "Time of Day (Hours)", y = "Depth", title = "Proportion of Time Spent at Each Depth by Hour March 1st - May 31st 2022") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),  # Adjust text size for readability
        axis.text.x = element_text(size = 12),
        legend.position = "right")

####
depth_proportions_lightcat <- detections_updated %>%
  group_by(Lightcat, Sensor.Val) %>% 
  summarise(count = n(), .groups = "drop") %>%
  group_by(Lightcat) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

library(ggplot2)

ggplot(depth_proportions, aes(x = time_of_day, y = Sensor.Val, fill = proportion)) +
  geom_tile() +
  geom_hline(yintercept = 2, color = "red", linetype = "dashed", linewidth = 1) +
  scale_fill_viridis_c(name = "Proportion") +  # Use Viridis color scale for better readability
  scale_y_reverse(limits=c(6,0))+
  labs(x = "Time of Day (Hours)", y = "Depth", title = "Proportion of Time Spent at Each Depth by Hour March 1st - May 31st 2022") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),  # Adjust text size for readability
        axis.text.x = element_text(size = 12),
        legend.position = "right")
#now we have 4 categories so we can get the proprotion of detections within the springtime spent at each of the 4 for targeted timing of removal
library(dplyr)

# Summarize proportion at ≤2m for each time period
daily_summary <- detections_updated %>%
  group_by(date, Lightcat) %>%  # Group by date and period (night, day, dusk, dawn)
  summarise(
    total_detections = n(),  # Total detections in this period
    detections_at_2m = sum(Sensor.Val <= 2, na.rm = TRUE),  # Detections where depth <=2m
    prop_at_2m = detections_at_2m / total_detections  # Proportion of detections at ≤2m
  ) %>%
  ungroup()

# View result
print(daily_summary)

write.csv(daily_summary, "spring2022_propat2m_bylightcat.csv")



library(lubridate)

monthly_summary <- daily_summary %>%
  mutate(month = floor_date(date, "month")) %>%  # Extract month
  group_by(month, Lightcat) %>%
  summarise(
    avg_prop_at_2m = mean(prop_at_2m, na.rm = TRUE)  # Average daily proportion
  ) %>%
  ungroup()

print(monthly_summary)



# Ensure month is a factor for ordered display
monthly_summary <- monthly_summary %>%
  mutate(month = factor(month, levels = unique(month)))  # Keep months in order

# Create the boxplot
ggplot(monthly_summary, aes(x = month, y = avg_prop_at_2m, fill = Lightcat)) +
  geom_bar(stat="identity", position="fill") +  # Box and whisker plot
  scale_fill_manual(values = c("Night" = "blue", "Day" = "yellow", 
                               "Dusk" = "orange", "Dawn" = "purple")) +  # Custom colors
  labs(title = "Monthly Proportion of Time at ≤2m Depth",
       x = "Month", 
       y = "Proportion at ≤2m",
       fill = "Light Category") +
  theme_minimal() +  # Clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

unique(monthly_summary$Lightcat)

ggplot(monthly_summary, aes(x = month, y = Lightcat, fill = avg_prop_at_2m)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") + 
  labs(title = "Heatmap of Time at ≤2m Depth",
       x = "Month", y = "Light Category",
       fill = "Proportion at ≤2m") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




####abacus plot of fish that moved into the CPM area passed the fishway 
goldfish204_2$rec_group_adjusted_new

unique(goldfish204_2$animal_id)


CPMfish <- dplyr::filter(goldfish204_2, animal_id == "1546510")6510

receiver_colors2023 <- c(
  "North shore" = "darkblue",
  "West End" = "darkred",
  "East End" = "darkgreen",
  "Central" = "purple3",
  "Carrolls Bay" = "darkorange",
  "Grindstone" = "pink3",
  "Bayfront area" = "yellow3", 
  "Cootes Paradise"="darkmagenta"
)



### can group sites in a certain order in future. 
CPMfish$animal_id<-as.factor(as.numeric(CPMfish$animal_id))

CPMfish_cootesdetections<-dplyr::filter(CPMfish, rec_group_adjusted_new=="Cootes Paradise")

###contains battery death date
#ggsave(plot=p1, "./Figures/HH_Fish_Detection_Duration_2015-2020.png",  width = 35, height = 40,units = "cm", dpi = 400)

fish_plot<-detections %>% group_by(common_name_e,transmitter_id) %>% summarize(min=min(detection_timestamp_EST), max=max(detection_timestamp_EST))

#####################################################################
fish_plot<-detections %>% group_by(common_name_e,transmitter_id, life_status) %>% summarize(min=min(detection_timestamp_EST), max=max(detection_timestamp_EST), end=max(Date.tag.dies))
sum(unique(fish_plot$transmitter_id))
##shows season and when battery life of tags ends. A bit harsh on the eyes tho. More of a fan of figure above.

  first <- min(CPMfish$detection_timestamp_EST) # time of first detection
  last <- max(CPMfish$detection_timestamp_EST) # time of last detection
  
  CPMfish$rec_group_adjusted_new<-as.factor(CPMfish$rec_group_adjusted_new)
  
  desired_order <- c("Cootes Paradise","Bayfront area", "Grindstone", "Carrolls Bay", "West End", "Central", "North shore", "East End")  # Replace with your specific group names
 
   desired_order <- c("East End","North shore", "Central", "Carrolls Bay" , "Cootes Paradise","Grindstone", "Bayfront area", "West End")  # Replace with your specific group names

  
  CPMfish$rec_group_adjusted_new <- factor(CPMfish$rec_group_adjusted_new, levels = desired_order)

CPMfish <- CPMfish %>%
  arrange(detection_timestamp_EST)  # Ensure chronological order
p1 <- ggplot(CPMfish, aes(x = detection_timestamp_EST, y = rec_group_adjusted_new, group = animal_id)) +
  geom_rect(
    xmin = as.POSIXct("2023-05-02"), xmax = as.POSIXct("2023-09-15"),
    ymin = -Inf, ymax = Inf, 
    fill = "lightgrey", alpha = 0.3  # Transparent highlight
  ) +
   geom_rect(
    xmin = as.POSIXct("2024-05-01"), xmax = as.POSIXct("2024-10-10"),
    ymin = -Inf, ymax = Inf, 
    fill = "lightgrey", alpha = 0.3  # Transparent highlight
  ) +
  geom_line(color = "black", size = 1) + 
  geom_point(color = "purple2", size = 2) +  
  labs(x = "Date", y = "Receiver Group") +
  scale_x_datetime(
    limits = c(first, last),  
    date_breaks = "1 month",
    date_labels = "%b-%Y"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14),
        axis.text.y=element_text(size=14))

p1
########################################################################

#####grindstone receiver number of fish number of detections plots

#########################################################################

HH_GConly <- goldfish204 %>%
  filter(station_no %in% c("29", "62", "33", 
                           "61", "60", "67", 
                           "66", "59", "68", "59", "64", "86", "87"))


#  goldfish204_2$station %in% c("HAM-029", "HAM-033", "HAM-059", "HAM-060", "HAM-061", "HAM-062", "HAM-064", "HAM-066", "HAM-067", "HAM-068", "HAM-086", "HAM-087") ~ "Grindstone",

######################
  

HH_GConly$rec_group <- case_when(
  HH_GConly$station %in% c("HAM-029") ~ "GC Mouth",
  HH_GConly$station %in% c("HAM-062") ~ "sunfish pond",
  HH_GConly$station %in% c("HAM-068") ~ "second bridge",
  HH_GConly$station %in% c("HAM-060") ~ "blackbird pond",
  HH_GConly$station %in% c("HAM-061") ~ "osprey pond",
  HH_GConly$station %in% c("HAM-033") ~ "Plains rd. bridge",
  HH_GConly$station %in% c("HAM-087") ~ "first bend",
  HH_GConly$station %in% c("HAM-066") ~ "ds pond 1",
  HH_GConly$station %in% c("HAM-067") ~ "pond 2",
  HH_GConly$station %in% c("HAM-059") ~ "SPS bridge",
  HH_GConly$station %in% c("HAM-086") ~ "Unsworth",
  TRUE ~ NA_character_ # Default to NA if no match
)################################

HH_GConly$year<-year(HH_GConly$detection_timestamp_EST)

HH_GConly_unique <- HH_GConly %>%
  filter(year %in% c(2022, 2023)) %>%  # Keep only 2022 and 2023
  distinct(station, year, .keep_all = TRUE)  # Keep one row per station per year


library(ggrepel)
library(ggplot2)
#get a map plot with these receivers plotted and the names showing up
gc <- ggplot() + 
  geom_polygon(data = shorelinemap, aes(x = long, y = lat, group = group), fill="lightblue3") +
  coord_cartesian(xlim = x_limits_GC, ylim = y_limits_GC) +
  geom_point(data = HH_GConly_unique, aes(x = deploy_long, y = deploy_lat)) +
  facet_wrap(~year)+
  geom_text_repel(data = HH_GConly_unique, aes(x = deploy_long, y = deploy_lat, label = rec_group), 
            max.overlaps= Inf, color = "black", size = 3, fontface="bold")  # Adjust the positioning and appearance of the labels

gc

library(ggpubr)
HH_GConly_expanded <- HH_GConly %>%
  group_by(rec_group, date = as.Date(detection_timestamp_utc), animal_id) %>%
  summarize(daily_detections = n(), .groups = "drop") %>%
  group_by(rec_group, date) %>%
  mutate(unique_individuals = n_distinct(animal_id))


as.factor(HH_GConly_expanded$animal_id)
# Plot with independent y-axes
library(ggplot2)

HH_GConly_summary <- HH_GConly %>%
  group_by(rec_group, date = as.Date(detection_timestamp_utc)) %>%
  summarize(
    daily_detections = n(),  # Total number of detections per day per station
    unique_individuals = n_distinct(animal_id)  # Number of unique individuals per day per station
  ) %>%
  ungroup()


p1 <- ggplot(HH_GConly_summary, aes(x = date)) +
  geom_bar(aes(y = daily_detections), stat = "identity", fill = "lightgrey", width = 0.7) +
  facet_wrap(~rec_group, ncol = 2, scales = "free_y") +
  labs(x = "Date", y = "Daily Detections", 
       title="2022") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_blank()  # Remove the right y-axis title
  )

p1
# Overlay points for unique individuals
p2 <- p1 +
  geom_point(data = HH_GConly_expanded, 
             aes(y = unique_individuals * (max(HH_GConly_summary$daily_detections) / 5), 
                 color = factor(animal_id)),  # Convert to factor for discrete colors
             shape = 16, size = 2) +
  scale_y_continuous(sec.axis = sec_axis(~ . / (max(HH_GConly_summary$daily_detections) / 5), name = "Unique Individuals (0-5)")) +
  scale_color_manual(values = scales::hue_pal()(length(unique(HH_GConly$animal_id)))) + 
  guides(color = guide_legend(title = "Animal ID"))  # Add legend title

p2

# Print the combined plot
print(p2)

#this type of plot only gets you the number of individuals total if they occured on teh same day so thats why only 1
#were showing up sometimes 

#plot is showing like 15 individuals right at april 1st so likely that they are staging even before this date. 
#receiver wasnt deployed until then I would assume!


GCmouthonly<-filter(HH_GConly_summary, rec_group=="GC Mouth")


####2023

H_GF_2022Ptagonly$detection_timestamp_utc <- as.POSIXct(HH_GF_2022Ptagonly$detection_timestamp_utc)

#filter to include only detections that occured between april 1 2022 and June 30th 
HH_GF_spring2023 <- HH_GF_2022Ptagonly %>%
  filter(detection_timestamp_utc >= as.POSIXct("2023-03-01") &
         detection_timestamp_utc <= as.POSIXct("2023-06-30"))
#now just filter out to only include the GC receivers

unique(HH_GF_spring2022$station_no)

HH_GF_spring2023 <- HH_GF_spring2023 %>% filter(!animal_id %in% c("A69-9002-1388", 
                                                                                          "A69-9006-9168", 
                                                                                          "A69-9007-13681", "A69-9002-1376", 
                                                                  "A69-9007-13696", "A69-9007-13678"))

#HAM-029, HAM-062, HAM-033, HAM-061, HAM-060, HAM-067, HAM-066, HAM-059

HH_GF_Spring2023_GConly <- HH_GF_spring2023 %>%
  filter(station_no %in% c("29", "62", "33", 
                           "61", "60", "67", 
                           "66", "59", "68"))


unique(HH_GF_Spring2023_GConly$station_no)
#now we need to summarize the number of detections per station_no as well as the number of unique individuals detected per day 
HH_GF_Spring2023_summary <- HH_GF_Spring2023_GConly %>%
  group_by(station_no, date = as.Date(detection_timestamp_utc)) %>%
  summarize(
    daily_detections = n(),  # Total number of detections per day per station
    unique_individuals = n_distinct(animal_id)  # Number of unique individuals per day per station
  ) %>%
  ungroup()

# Plot with independent y-axes
library(ggplot2)

library(ggplot2)
library(ggplot2)

# Calculate max daily detections beforehand
max_detections <- max(HH_GConly_summary$daily_detections, na.rm = TRUE)

HH_GConly_summary <- HH_GConly_summary %>%
  mutate(rec_group = factor(rec_group, levels = c(
    "GC Mouth", "sunfish pond", "second bridge gc", "blackbird pond", 
    "osprey pond", "Plains rd. bridge", "first bend", "ds pond 1", 
    "pond 2", "south pasture swamp bridge"
  )))


p1 <- ggplot(HH_GConly_summary, aes(x = date)) +
  geom_bar(aes(y = daily_detections), stat = "identity", fill = "lightgrey", width = 0.7) +
  facet_wrap(~rec_group, ncol = 2, scales = "free_y") +  # Ordered by the factor levels
  labs(x = "Date", y = "Daily Detections", title = "2023") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_blank()
  )

# Overlay points for unique individuals using precomputed max_detections
p2 <- p1 +
  geom_point(aes(y = unique_individuals * (max_detections / 5)), 
             shape = 1, size = 2, color = "black") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / (max_detections / 5), name = "Unique Individuals (0-5)")
  )

# Print the combined plot
print(p2)
###############################


###to get the plot with animal id on the y and date on the x for the grindstone receivers 

###########################

HH_GConly$rec_group <- case_when(
  HH_GConly$station %in% c("HAM-029") ~ "GC Mouth",
  HH_GConly$station %in% c("HAM-062") ~ "sunfish pond",
  HH_GConly$station %in% c("HAM-068") ~ "second bridge",
  HH_GConly$station %in% c("HAM-060") ~ "blackbird pond",
  HH_GConly$station %in% c("HAM-061") ~ "osprey pond",
  HH_GConly$station %in% c("HAM-033") ~ "Plains rd. bridge",
  HH_GConly$station %in% c("HAM-087") ~ "first bend",
  HH_GConly$station %in% c("HAM-066") ~ "ds pond 1",
  HH_GConly$station %in% c("HAM-067") ~ "pond 2",
  HH_GConly$station %in% c("HAM-059") ~ "SPS bridge",
  HH_GConly$station %in% c("HAM-086") ~ "Unsworth",
  TRUE ~ NA_character_ # Default to NA if no match
)################################



HH_GConly_expanded <- HH_GConly %>%
  group_by(rec_group, date = as.Date(detection_timestamp_utc), animal_id) %>%
  summarize(daily_detections = n(), .groups = "drop") %>%
  group_by(rec_group, date) %>%
  mutate(unique_individuals = n_distinct(animal_id))

unique(HH_GConly_expanded$rec_group)
HH_GConly_expanded <- HH_GConly_expanded %>%
  mutate(rec_group = factor(rec_group, levels = c(
    "GC Mouth", "sunfish pond", "second bridge gc", "blackbird pond", 
    "osprey pond", "Plains rd. bridge", "first bend", "ds pond 1", 
    "pond 2", "south pasture swamp bridge"
  )))


individuals_GC<-ggplot(HH_GConly_expanded, aes(x = date, y = factor(animal_id), fill = rec_group)) +
  geom_tile() +
  facet_wrap(~rec_group, ncol = 2, scales = "free_y") +
  scale_fill_viridis_d() +
  labs(x = "Date", y = "Animal ID", fill = "Receiver Group") +
  theme_minimal()
individuals_GC
