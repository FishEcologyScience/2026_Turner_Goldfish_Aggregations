#load in receiver file 
#see figure code for mapping and receiver location information 

hamrecs<-read.csv("/data/recs_combined222223_forGIS")




################################################################
#goldfish 
#############proportions and depth  
library(dplyr)
library(zoo)
library(tidyr)
library(ggplot2)



#load in data
goldfish204<-readRDS("/HHGOLDFISH_detections_clipped_dead_filtered_firstwoweeksremoved_2015-2025_correct.rds")

#goldfish204<-`HHGOLDFISH_detections_clipped_dead_filtered_firstwoweeksremoved_2015-2025_correct`
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

#write.csv(final_locations_withavgdepth_Proprotions,"dailyprops_avgdepth_droppedTtagsSept25.csv")

##now take the proportions dataframe and add the avg depth to it based on the date and rec group



final_locations_withavgdepth_Proprotions <- proportions %>%
  left_join(summary_df_depth_recgroup, by = c("date", "rec_group_adjusted_new"))

#write.csv(final_locations_withavgdepth_Proprotions, "dailyprops_recgroups_alldata_plusavgdepth_FINAL.csv")
#final_locations_withavgdepth_Proprotions<-read.csv("dailyprops_recgroups_alldata_plusavgdepth_FINAL.csv")


####apply the shallow aggregations filter 

#data filtering 
aggregations_props<-dailyprops_recgroups_alldata_plusavgdepth_FINAL %>%
  filter(proportion >=75)

shallowaggregations <- aggregations_props %>%
  filter(avg_depth <= 2.0)

##see figure plotting code for plotting data


final_locations_withavgdepth_Proprotions$date <- as.Date(final_locations_withavgdepth_Proprotions$date, format = "%Y-%m-%d")  # Adjust format if necessary
final_locations_withavgdepth_Proprotions$rec_group_adjusted_new <- as.factor(final_locations_withavgdepth_Proprotions$rec_group_adjusted_new)


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

####
depth_proportions_lightcat <- detections_updated %>%
  group_by(Lightcat, Sensor.Val) %>% 
  summarise(count = n(), .groups = "drop") %>%
  group_by(Lightcat) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()


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

#write.csv(daily_summary, "spring2022_propat2m_bylightcat.csv")



library(lubridate)

monthly_summary <- daily_summary %>%
  mutate(month = floor_date(date, "month")) %>%  # Extract month
  group_by(month, Lightcat) %>%
  summarise(
    avg_prop_at_2m = mean(prop_at_2m, na.rm = TRUE)  # Average daily proportion
  ) %>%
  ungroup()

print(monthly_summary)



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

library(ggpubr)
HH_GConly_expanded <- HH_GConly %>%
  group_by(rec_group, date = as.Date(detection_timestamp_utc), animal_id) %>%
  summarize(daily_detections = n(), .groups = "drop") %>%
  group_by(rec_group, date) %>%
  mutate(unique_individuals = n_distinct(animal_id))


HH_GConly_summary <- HH_GConly %>%
  group_by(rec_group, date = as.Date(detection_timestamp_utc)) %>%
  summarize(
    daily_detections = n(),  # Total number of detections per day per station
    unique_individuals = n_distinct(animal_id)  # Number of unique individuals per day per station
  ) %>%
  ungroup()




