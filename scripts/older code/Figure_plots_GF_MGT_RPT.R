##figure code for Goldfish management report 


########Figure PT1###############
#goldfish detected across the grindstone creek receivers during pre-spawn stage and spawning time period
library(readxl)

GC_props<-read_excel("GC_props.xlsx")
HH_GConly_unique<-read.csv("GC_reclocs.csv")

#x_limits_GC_new <- c(-79.89, -79.875)  # Define your desired range for x-axis
#y_limits_GC_new <- c(43.285, 43.3)  # Define your desired range for y-axis

library(ggplot2)
library(RColorBrewer)

GC_props$Receiver <- case_when(
  GC_props$Receiver %in% c("GC mouth") ~ "Grindstone Creek Mouth",
  GC_props$Receiver %in% c("sunfish pond") ~ "Sunfish pond",
  GC_props$Receiver %in% c("second bridge") ~ "Grindstone Creek 2",
  GC_props$Receiver %in% c("blackbird pond") ~ "Blackbird pond",
  GC_props$Receiver %in% c("osprey pond") ~ "Osprey pond",
  GC_props$Receiver %in% c("Plains rd. bridge") ~ "Grindstone Creek 3",
  GC_props$Receiver %in% c("pond 2") ~ "Grindstone Creek 4",
  GC_props$Receiver %in% c("SPS bridge") ~ "Grindstone Creek 5",
  TRUE ~ NA_character_ # Default to NA if no match
)################################
unique(GC_props$Receiver)

library(sf)
library(lwgeom)

HH_gcmap <- st_read("C:/Users/TURNERN/Desktop/GIS/HH_Poly_Mar2025/HH_WaterlinesToPoly_21Mar2025.shp")


HH_gcmap <- st_transform(HH_gcmap, crs = 4326)  # Convert to lat/lon if needed
#this is a lingstring need to convert to a polygon feature 
polygons_sf <- st_polygonize(HH_gcmap)
plot(polygons_sf)




HH<-ggplot()+
  geom_sf(data=HH_gcmap, fill="lightblue")+
  theme_minimal()


HH

x_limits_GC <- c(-79.888, -79.875)  # Define your desired range for x-axis
y_limits_GC <- c(43.288, 43.299)  # Define your desired range for y-axis



HH1 <- ggplot() +
  geom_sf(data = HH_gcmap, fill="lightblue") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC, expand = FALSE) +  # Set coordinate limits
  theme_minimal()

HH1  # Plot the map



x_limits_GC <- c(-79.888, -79.875)  # Define your desired range for x-axis
y_limits_GC <- c(43.288, 43.299)  # Define your desired range for y-axis


gc_bubbleplot_1 <- ggplot() + 
   geom_sf(data = HH_gcmap, fill="lightblue") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC)+
  facet_wrap(~year) +
  # Adjusted point size while keeping proportions
  geom_point(data = GC_props, aes(x = deploy_long, y = deploy_lat, size = prop, color = Receiver), alpha = 1) +
  # Increase the size range (adjust as needed)
  scale_size_continuous(range = c(6, 20)) +  
scale_color_manual(values = brewer.pal(n = length(unique(GC_props$Receiver)), name = "Set2"))+
  geom_text(data = GC_props, aes(x = deploy_long, y = deploy_lat, label = num_individuals), 
            vjust = 0, hjust = 0, color = "black", size = 6, fontface="bold") +
  theme_minimal() +
  labs(title = "",
       size = "Percentage",
       color = "Receiver", 
       x="Lon",
       y="Lat") +
  theme(legend.position = "bottom", 
        plot.title = element_text(size = 40, face = "bold"),  # Title size
    axis.title.x = element_text(size = 16),  # X-axis label size
    axis.title.y = element_text(size = 16),  # Y-axis label size
     axis.text.x = element_text(size = 11, angle = 45),  # X-axis tick label size
    axis.text.y = element_text(size = 11),  # Y-axis tick label size
   strip.text = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16))+  # Legend text size )
  guides(color = guide_legend(override.aes = list(size = 16)))  

gc_bubbleplot_1

   

ggsave("./Figures/gc_bubbleplot_1.png", plot = gc_bubbleplot_1, width = 17, height = 12, dpi = 300, units = "in", bg="white")
#########################################################
##Figure PT2
#Abacus plot for each indiviudual goldfish detected at each grindstone creek receiver
#with gray bars denoting the time perionds of spawning window as determined by boston et al 2024
HH_GConly <- goldfish204 %>%
  filter(station_no %in% c("29", "62", "33", 
                           "61", "60", "67", 
                           "66", "59", "68", "59", "64", "86", "87"))

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
unique(HH_GConly$rec_group)
HH_GConly$rec_group <- case_when(
  HH_GConly$station_no %in% c("29") ~ "Grindstone Creek Mouth",
  HH_GConly$station_no %in% c("62") ~ "Sunfish pond",
  HH_GConly$station_no %in% c("68") ~ "Grindstone Creek 2",
  HH_GConly$station_no %in% c("60") ~ "Blackbird pond",
  HH_GConly$station_no %in% c("61") ~ "Osprey pond",
  HH_GConly$station_no %in% c("33") ~ "Grindstone Creek 3",
  HH_GConly$station_no %in% c("67") ~ "Grindstone Creek 4",
  HH_GConly$station_no %in% c("59") ~ "Grindstone Creek 5",
  TRUE ~ NA_character_ # Default to NA if no match
)################################
library(lubridate)
HH_GConly$year<-year(HH_GConly$detection_timestamp_EST)
unique(HH_GConly$rec_group)
colnames(HH_GConly)[colnames(HH_GConly) == "tag_serial_number"] <- "animal_id"

HH_GConly_expanded <- HH_GConly %>%
  group_by(rec_group, date = as.Date(detection_timestamp_utc), animal_id) %>%
  summarize(daily_detections = n(), .groups = "drop") %>%
  group_by(rec_group, date) %>%
  mutate(unique_individuals = n_distinct(animal_id))


as.factor(HH_GConly_expanded$animal_id)


library(ggplot2)


library(ggplot2)

# Plot with independent y-axes


library(ggplot2)
unique(HH_GConly_summary$rec_group)
HH_GConly_summary <- HH_GConly %>%
  group_by(rec_group, date = as.Date(detection_timestamp_utc)) %>%
  summarize(
    daily_detections = n(),  # Total number of detections per day per station
    unique_individuals = n_distinct(animal_id)  # Number of unique individuals per day per station
  ) %>%
  ungroup()

HH_GConly_expanded <- HH_GConly %>%
  group_by(rec_group, date = as.Date(detection_timestamp_utc), animal_id) %>%
  summarize(daily_detections = n(), .groups = "drop") %>%
  group_by(rec_group, date) %>%
  mutate(unique_individuals = n_distinct(animal_id))
unique(HH_GConly_expanded$rec_group)

HH_GConly_expanded <- HH_GConly_expanded %>%
  mutate(rec_group = factor(rec_group, levels = c(
    "Grindstone Creek Mouth", "Sunfish pond", "Grindstone Creek 2", "Blackbird pond", 
    "Osprey pond", "Grindstone Creek 3", 
    "Grindstone Creek 4", "Grindstone Creek 5"
  )))



# Create a dataframe for grey periods
grey_periods <- data.frame(
  xmin = as.Date(c("2022-05-01", "2023-05-01", "2024-05-01")),
  xmax = as.Date(c("2022-07-03", "2023-07-03", "2024-07-03"))
)

# Create the plot
individuals_GC <- ggplot(HH_GConly_expanded, aes(x = date, y = factor(animal_id))) +
  # Add grey bars from the grey_periods dataframe
  geom_rect(data = grey_periods, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            fill = "grey60", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(color = "dodgerblue4",size=3) +  # Now add the main data
  facet_wrap(~rec_group, ncol = 2, scales = "free_y") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +  # Bi-monthly labels
  labs(x = "Date", y = "Animal ID", fill = "Receiver Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 13), 
        axis.text.y = element_text(size = 12))  # Rotate x-axis labels

individuals_GC


##slighlthy reworking the above plot to make it more interprtabel

library(ggplot2)
library(dplyr)
library(patchwork)  # For stacking plots

# Define the groups to isolate
isolated_groups <- c("Grindstone Creek Mouth", "Grindstone Creek 2")

# Split data
isolated_data <- HH_GConly_expanded %>% filter(rec_group %in% isolated_groups)
remaining_data <- HH_GConly_expanded %>% filter(!rec_group %in% isolated_groups)

# Create a dataframe for grey periods
grey_periods <- data.frame(
  xmin = as.Date(c("2022-05-01", "2023-05-01", "2024-05-01")),
  xmax = as.Date(c("2022-07-03", "2023-07-03", "2024-07-03"))
)

# Plot for isolated groups
plot_isolated <- ggplot(isolated_data, aes(x = date, y = factor(animal_id))) +
  geom_rect(data = grey_periods, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            fill = "grey60", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(color = "dodgerblue4", size = 4) +  
  geom_vline(xintercept = as.Date("2022-04-04"), color = "green4", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = as.Date("2022-05-03"), color = "red", linetype = "dashed", size = 1) +  
   geom_vline(xintercept = as.Date("2023-03-29"), color = "green4", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = as.Date("2023-04-13"), color = "red", linetype = "dashed", size = 1) + 
     geom_vline(xintercept = as.Date("2024-03-13"), color = "green4", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = as.Date("2024-04-13"), color = "red", linetype = "dashed", size = 1) +  
  facet_wrap(~rec_group, ncol = 1, scales = "free_y") +  
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = "Date", y = "Animal ID", fill = "Receiver Group", title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15), 
        axis.text.y = element_text(size = 15), 
        plot.title = element_text(size=15),
        strip.text = element_text(size=15))

plot_isolated

plot_remaining <- ggplot(remaining_data, aes(x = date, y = factor(animal_id))) +
  geom_rect(data = grey_periods, 
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), 
            fill = "grey60", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(color = "dodgerblue4", size = 4) +  

  # Green line for April 4th, 2022 in Blackbird Pond
  geom_vline(data = subset(remaining_data, rec_group %in% c("Blackbird pond")), 
             aes(xintercept = as.Date("2022-04-04")), 
             color = "green4", linetype = "dashed", size = 1) +  

  # Red line for May 3rd, 2022 in Blackbird Pond
  geom_vline(data = subset(remaining_data, rec_group %in% c("Blackbird pond")), 
             aes(xintercept = as.Date("2022-05-03")), 
             color = "red", linetype = "dashed", size = 1) +  

  # Green line for March 29th, 2023 in multiple groups
  geom_vline(data = subset(remaining_data, rec_group %in% c("Blackbird pond", "Sunfish pond", "Osprey pond")), 
             aes(xintercept = as.Date("2023-03-29")), 
             color = "green4", linetype = "dashed", size = 1) +  

  # Red line for April 13th, 2023 in multiple groups
  geom_vline(data = subset(remaining_data, rec_group %in% c("Blackbird pond", "Sunfish pond", "Osprey pond")), 
             aes(xintercept = as.Date("2023-04-13")), 
             color = "red", linetype = "dashed", size = 1) +  

  # Green line for March 13th, 2024 in Osprey Pond
  geom_vline(data = subset(remaining_data, rec_group %in% c("Osprey pond")), 
             aes(xintercept = as.Date("2024-03-13")), 
             color = "green4", linetype = "dashed", size = 1) +  

  # Red line for April 13th, 2024 in Osprey Pond
  geom_vline(data = subset(remaining_data, rec_group %in% c("Osprey pond")), 
             aes(xintercept = as.Date("2024-04-13")), 
             color = "red", linetype = "dashed", size = 1) +  

  facet_wrap(~rec_group, ncol = 2, scales = "free_y") +  
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = "Date", y = "Animal ID", fill = "Receiver Group", title = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15), 
        axis.text.y = element_text(size = 15), 
        plot.title = element_text(size=15),
        strip.text = element_text(size=15))

plot_remaining


# Combine plots
plot_isolated / plot_remaining + plot_layout(heights = c(2,1)) # Stacks them vertically using patchwork


ggsave("./Figures/gc_timingdetections.png", plot = plot_isolated, width = 17, height = 12, dpi = 300, units = "in", bg="white")
ggsave("./Figures/gc_timingdetections2.png", plot = plot_remaining, width = 17, height = 12, dpi = 300, units = "in", bg="white")

####################################################

##Fig X. 
#number of daily detections and number of individuals detected daily at th eCPM fishway over thre years of the study 
#check proportions at the fishwya receveivers and average depth, add details into the dissucssion

####fishway only 
cootesfishway <- goldfish204 %>%
  filter(station_no %in% c("43", "32"))
unique(cootesfishway$station_no)

cootesfishway$rec_fishway<-case_when(
  cootesfishway$station %in% c("HAM-043", "HAM-032") ~ "Fishway", 
  TRUE ~ NA_character_ # Default to NA if no match
)

cootesfishway$year<-year(cootesfishway$detection_timestamp_EST)
cootesfishway$doy<- strftime(cootesfishway$detection_timestamp_EST, format = "%j")
cootesfishway$doy<-as.numeric(cootesfishway$doy)
cootesfishway$date<-as.POSIXct(cootesfishway$detection_timestamp_EST, format="%Y-%m-%d")

cootesfishway_summary_doy <- cootesfishway %>%
  mutate(year = year(detection_timestamp_EST)) %>%  # Ensure year is extracted from the 'date' column
  filter(year %in% c(2022, 2023, 2024)) %>%  # Filter to keep only the years 2022, 2023, 2024
  group_by(rec_fishway, doy, year, date) %>%  # Keep 'date' in the grouping to carry it forward
  summarize(
    daily_detections = n(),  # Total number of detections per day per station
    unique_individuals = n_distinct(animal_id)  # Number of unique individuals per day per station
  ) %>%
  ungroup()


cootesfishway_summary <- cootesfishway %>%
  group_by(rec_fishway, date = as.Date(detection_timestamp_utc)) %>%
  summarize(
    daily_detections = n(),  # Total number of detections per day per station
    unique_individuals = n_distinct(animal_id)  # Number of unique individuals per day per station
  ) %>%
  ungroup()

#plot is sho
#change this to be doy
#remove 2021



p1 <- ggplot(cootesfishway_summary_doy, aes(x = doy, fill = factor(year))) +  
  # Add the light grey box first so it's behind the bars
  geom_rect(aes(xmin = 101, xmax = 148, ymin = -Inf, ymax = Inf), 
            fill = "lightskyblue", alpha = 0.1) +  # Adjust the transparency of the box
   geom_rect(aes(xmin = 204, xmax = 261, ymin = -Inf, ymax = Inf), 
            fill = "#74C476", alpha = 0.1) +  # Adjust the transparency of the box

  # Then add the bar plot
  geom_bar(aes(y = daily_detections), stat = "identity", width = 1) +  # Bar plot
  
  labs(x = "Day of Year", y = "Daily Detections", 
       title = "") +
  scale_fill_manual(values = c("2022" = "orchid4", "2023" = "orchid4", "2024" = "orchid4")) +  # Customize the colors
  theme_minimal() +
  facet_wrap(~year) +  # Facet by year
  theme(
    axis.title.y = element_text(color = "black", size=14),
    axis.title.y.right = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size=16),
    axis.text.y = element_text(size=16),
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title
    legend.text = element_text(size = 10),  # Customize legend text size
    strip.text = element_text(size = 14, face="bold")  # Customize facet labels (strip text)
  ) +
  labs(fill = "Year")  # Set the legend title to "Year"



# Display the plot
p1


# Print the plot
print(p1)



p1

p2 <- p1 +
  geom_point(aes(y = unique_individuals * (max(cootesfishway_summary$daily_detections) / 5)), 
             shape = 1, size = 2, color = "black") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / (max(cootesfishway_summary$daily_detections) / 5), name = "Number of Individuals"),  # Add second axis label
    name = "Daily Detections"  # Label for the primary y-axis
  ) +
  theme(
     axis.text.y = element_text(size = 12), 
     axis.text.x=element_text(size=13),
    axis.title.y.right = element_text(color = "black")  # Add color to the right axis title
  )

# Display the plot
p2



########abacus plot of fish 1546510
####abacus plot of fish that moved into the CPM area passed the fishway 
goldfish204_2$rec_group_adjusted_new

unique(goldfish204_2$animal_id)


CPMfish <- dplyr::filter(goldfish204_2, animal_id == "1546510")

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

hamrecs$rec_group_adjusted_new
hamrecs$rec_group_adjusted_new <- case_when(
  hamrecs$station %in% c("HAM-052", "HAM-053", "HAM-054", "HAM-055", "HAM-056", "HAM-057", "HAM-016", "HAM-051","HAM-036",
                               "HAM-082", "HAM-081") ~ "Bayfront area",
  hamrecs$station %in% c("HAM-030", "HAM-031", "HAM-042", "HAM-044", "HAM-065", "HAM-096", "HAM-098", "HAM-095", "HAM-094", "HAM-093", "HAM-088", "HAM-090", "HAM-092", "HAM-091", "HAM-089", "HAM-097") ~ "Cootes Paradise",
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

#want a new figure of the CPM fish, abacus plot with all the CPM including fishway receivers filtered out 
#43 and 32 if fishway inner outter 
CPMfish <- dplyr::filter(rec_props2022_2024_updated, animal_id == "1546510")

 CPMfish_CPMfishwaydetsonly <- CPMfish %>%
  filter(station %in% c("HAM-043", "HAM-032", "HAM-031", "HAM-042", "HAM-044", "HAM-065", "HAM-096",
                        "HAM-098", "HAM-095", "HAM-094", "HAM-093", "HAM-088", "HAM-090", "HAM-092", "HAM-091",
                        "HAM-089", "HAM-097", "HAM-030"))
 
 #go to shapefile code to get the map 
 
 CPM_HHmap<-ggplot() +
  geom_polygon(data = shorelinemap, aes(x = long, y = lat, group = group)) +
  coord_cartesian(xlim = X_CPM, ylim = Y_CPM) +
  theme_minimal()
 
 
 detections_bubble_plottt<-ggplot() +
  geom_polygon(data = shorelinemap, aes(x = long, y = lat, group = group), fill = "light blue") +
   facet_wrap(~year)+
  coord_cartesian(xlim = X_CPM, ylim = Y_CPM) +
  theme_minimal() + 
  geom_point(data=CPMfish_CPMfishwaydetsonly, aes(x=deploy_long, y=deploy_lat, size=2))
 
 
 bubplot_cpm<-detection_bubble_plot(CPMfish_CPMfishwaydetsonly)

 CPMfish_CPMfishwaydetsonly2023 <- CPMfish_CPMfishwaydetsonly %>%
  filter(year == "2023")

 sumdet2023<-summarize_detections(
  CPMfish_CPMfishwaydetsonly2023,
  location_col="station",
  summ_type = "location"
)

  CPMfish_CPMfishwaydetsonly2024<- CPMfish_CPMfishwaydetsonly %>%
  filter(year == "2024")

sumdet2024<-summarize_detections(
  CPMfish_CPMfishwaydetsonly2024,
  location_col="station",
  summ_type = "location"
)

CPMfishsummarized<-rbind(sumdet2023, sumdet2024)
CPMfishsummarized$year<-as.numeric(format(CPMfishsummarized$first_det,"%Y"))

detections_bubble_plottt_new <- ggplot() +
  geom_polygon(data = shorelinemap, aes(x = long, y = lat, group = group), fill = "light blue") +
  facet_wrap(~year) +
  coord_cartesian(xlim = X_CPM, ylim = Y_CPM) +
  theme_minimal() + 
  geom_point(data = CPMfishsummarized, aes(x = mean_lon, y = mean_lat, size = num_dets), alpha = 0.6) +
  scale_size_continuous(name = "Detections", range = c(2, 10))+
  theme(
    axis.title.y = element_text(color = "black", size=14),
    axis.title.y.right = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size=16),
    axis.text.y = element_text(size=16),
    axis.title.x = element_text(size=14),
    legend.title = element_text(size = 12, face = "bold"),  # Customize legend title
    legend.text = element_text(size = 10),  # Customize legend text size
    strip.text = element_text(size = 14, face="bold"))# Customize facet labels (strip text)
  # Set a consistent range

detections_bubble_plottt_new



###figures for the active management section 
#figure of daily proportional time within each of the receiver groupings 

##depth over time byu receiver grouping with the 2m filter on, shoiuld make a second plot with the 75% rpoprtion filter aswell?


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

shallow_data_75<-shallow_data %>%
  filter(proportion >=75)

#clip data to just have 
#clip off  until January 2024
shallowdata_jan2024 <- shallow_data %>%
  filter(date <= as.Date("2023-12-31"))

ggplot(shallowdata_jan2024, aes(x = date, y = avg_depth, color = rec_group_adjusted_new , size = proportion)) +
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
    size = "Percentage"
  ) +
  theme(
  axis.text.x = element_text(angle = 45, hjust = 1, size=14),
  axis.text.y = element_text(size = 14),  # Fix `y.axis.text`
  axis.title.x = element_text(size = 14),  # Fix `x.title.text`
  axis.title.y = element_text(size = 14),   # Fix `y.title.text`
   legend.title = element_text(size = 16, face = "bold"),  # Increases legend title size  
    legend.text = element_text(size = 14)  # Increases legend item text size  
  )


ggplot(shallow_data_75, aes(x = date, y = avg_depth, color = rec_group_adjusted_new , size = proportion)) +
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
    size = "Percentage"
  ) +
theme(
  axis.text.x = element_text(angle = 45, hjust = 1, size=14),
  axis.text.y = element_text(size = 14),  # Fix `y.axis.text`
  axis.title.x = element_text(size = 14),  # Fix `x.title.text`
  axis.title.y = element_text(size = 14),   # Fix `y.title.text`
   legend.title = element_text(size = 16, face = "bold"),  # Increases legend title size  
    legend.text = element_text(size = 14)  # Increases legend item text size  
  )

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















###########depth


library(suncalc)
library(dplyr)

goldfish204_2 <- goldfish204_2 %>%
  mutate(date = as.Date(detection_timestamp_EST))

spring2022_goldfish<-goldfish204_2[goldfish204_2$date >= "2022-03-01" & 
                                               goldfish204_2$date <= "2022-05-31",]

###use getSunlightTimes in suncalc package
spring2022_goldfish$lat<-spring2022_goldfish$deploy_lat
spring2022_goldfish$lon<-spring2022_goldfish$deploy_long
hmmm<-getSunlightTimes(data=spring2022_goldfish,  keep=c("nauticalDawn", "goldenHourEnd", "goldenHour", "nauticalDusk","night","nightEnd"), tz = "America/Toronto")


hmmm<-subset(hmmm, select=c("nauticalDawn", "goldenHourEnd", "goldenHour", "nauticalDusk","night","nightEnd"))
spring2022_goldfish_1<-cbind(spring2022_goldfish,hmmm)

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
library(plotly)

unique(specific_day$Lightcat)
library(dplyr)

library(dplyr)

light_category_times <- specific_day %>%
  group_by(Lightcat) %>%
  summarize(
    start_time = min(detection_timestamp_EST),  # Use the correct timestamp column
    end_time = max(detection_timestamp_EST)
  ) %>%
  ungroup() %>%  # Ensure Lightcat is available for arranging
  arrange(match(Lightcat, c("Night", "Dawn", "Day", "Dusk")))  # Sort in correct order

print(light_category_times)



p <- ggplot(specific_day) +
  geom_point(aes(x = nauticalDawn, y = 1), color = "blue", size = 4) +
   geom_point(aes(x = nauticalDawnEnd, y = 1), color = "blue", size = 4) +
  geom_point(aes(x = goldenHourEnd, y = 2), color = "orange", size = 4) +
  geom_point(aes(x = goldenHour, y = 3), color = "yellow", size = 4) +
  geom_point(aes(x = nauticalDusk, y = 4), color = "purple", size = 4) +
  geom_point(aes(x = night, y = 5), color = "red", size = 4) +
  geom_point(aes(x = nightEnd, y = 6), color = "green", size = 4) +
  scale_y_continuous(breaks = 1:6, labels = c("Nautical Dawn", "Golden Hour End", "Golden Hour",
                                              "Nautical Dusk", "Night", "Night End")) +
  labs(title = "Sunlight Times for March 1st, 2022",
       x = "Time of Day", y = "") +
  theme_minimal()

# Convert to interactive plotly
ggplotly(p)



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






########################################
###heatmap for daily proportion rec group activity 


#U:/Goldfish_Telem_Jan25/final dataframes used in GRM report/rec_props_byGROUPING2022-2024.csv
##make the coooool plot
proportions<-dailyprops_recgroups_alldata_plusavgdepth_FINAL
proportions$date <- as.Date(proportions$date, format = "%Y-%m-%d")  # Adjust format if necessary
proportions$rec_group_adjusted_new <- as.factor(proportions$rec_group_adjusted_new)


unique(proportions$rec_group_adjusted_new)
desired_order <- c("Cootes Paradise","Bayfront area", "Grindstone", "Carrolls Bay", "West End", "Central", "North shore", "East End")  # Replace with your specific group names

proportions$rec_group_adjusted_new    <- factor(proportions$rec_group_adjusted_new   , levels = desired_order)

heatmap2 <- ggplot(proportions, aes(x = date, y = rec_group_adjusted_new   , fill = proportion)) +
  geom_tile() +
  scale_fill_viridis_c() +
  
  # Set bi-monthly breaks with year in label
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  
  # Set the Y-axis as a factor for station numbers
  scale_y_discrete(breaks = unique(proportions$rec_group_adjusted_new   )) +
  
  labs(title = "",
       x = "Date",
       y = "Receiver group",
       fill = "Percentage") +
  
  theme_minimal() +
   theme(
  axis.text.x = element_text(angle = 45, hjust = 1, size=15),
  axis.text.y = element_text(size = 15),  # Fix `y.axis.text`
  axis.title.x = element_text(size = 14),  # Fix `x.title.text`
  axis.title.y = element_text(size = 14),   # Fix `y.title.text`
   legend.title = element_text(size = 16, face = "bold"),  # Increases legend title size  
    legend.text = element_text(size = 14)  # Increases legend item text size  
  )

heatmap2

####make of rec in CP and GC for jen bowman with station_no on the figure 
#2024 receivers 

library(sf)
library(lwgeom)

HH_gcmap <- st_read("C:/Users/TURNERN/Desktop/GIS/HH_Poly_Mar2025/HH_WaterlinesToPoly_21Mar2025.shp")


HH_gcmap <- st_transform(HH_gcmap, crs = 4326)  # Convert to lat/lon if needed
#this is a lingstring need to convert to a polygon feature 
polygons_sf <- st_polygonize(HH_gcmap)
plot(polygons_sf)




HH<-ggplot()+
  geom_sf(data=HH_gcmap, fill="lightblue")+
  theme_minimal()


HH

x_limits_GC <- c(-79.891, -79.865)  # Define your desired range for x-axis
y_limits_GC <- c(43.275, 43.299)  # Define your desired range for y-axis
HH2 <- ggplot() +
  geom_sf(data = HH_gcmap, fill="lightblue") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC, expand = FALSE) +  # Set coordinate limits
  theme_minimal()

HH2
#CP and GC
X_CPM<-c(-79.93, -79.86) 
Y_CPM<-c(43.265, 43.297)


HH1 <- ggplot() +
  geom_sf(data = HH_gcmap, fill="lightblue") +
  coord_sf(xlim = X_CPM, ylim = Y_CPM, expand = FALSE) +  # Set coordinate limits
  theme_minimal()

HH1  # Plot the map



ham_rec_locs_upto2024 <- as.data.frame(ham_rec_locs_upto2024)
CPM_GCrecs <- ham_rec_locs_upto2024 %>%
  dplyr::filter(.data$station %in% c("HAM-030", "HAM-031", "HAM-042", "HAM-044", "HAM-065", "HAM-096", 
                                     "HAM-098", "HAM-095", "HAM-094", "HAM-093", "HAM-088", "HAM-090",
                                     "HAM-092", "HAM-091", "HAM-089", "HAM-097", "HAM-030", "HAM-042",
                                     "HAM-033", "HAM-059", "HAM-060", "HAM-061", "HAM-062", "HAM-064", 
                                     "HAM-066", "HAM-067", "HAM-068", "HAM-086", "HAM-087", "HAM-070",
                                     "HAM-099" ,"HAM-071", "HAM-063", "HAM-032", "HAM-043", "HAM-029"))


CPM_GCrecs <- CPM_GCrecs %>%
  mutate(year = year(deploy_date_time))

recs_2024CPM_GCrecs<-CPM_GCrecs %>%
  dplyr::filter(year == 2024)

gc_bubbleplot_1 <- ggplot() + 
   geom_sf(data = HH_gcmap, fill="lightblue") +
  coord_sf(xlim = x_limits_GC, ylim = y_limits_GC)+
 # facet_wrap(~year)+
  # Adjusted point size while keeping proportions
  geom_point(data = recs_2024CPM_GCrecs, aes(x = deploy_long, y = deploy_lat,), alpha = 1) +
  # Increase the size range (adjust as needed)
  geom_text(data = CPM_GCrecs, aes(x = deploy_long, y = deploy_lat, label = station), 
            vjust = 0, hjust = 0, color = "black", size = 4, fontface="bold") +
  theme_minimal() +
  theme(legend.position = "bottom")

gc_bubbleplot_1
