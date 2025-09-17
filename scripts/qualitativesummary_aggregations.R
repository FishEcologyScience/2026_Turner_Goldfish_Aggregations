#CAPTURED FISH DATA
#direct and targeted field sampling April 2025 


#summarize mean temp of aggregation in each polygon for each time period event
#also maybe subset for just aggregation so >5 individuals 

#data/raw/Aggdata_June52025.xlsx

WQsummary_agg<-summarise((Aggdata_June52025), 
group_by(Date,Polygon), 
mean(Temp, DO))

Aggdata_June52025$Temp<-as.numeric(Aggdata_June52025$Temp)
Aggdata_June52025$DO<-as.numeric(Aggdata_June52025$DO)
Aggdata_June52025$distance_to_river_mouth_m<-as.numeric(Aggdata_June52025$distance_to_river_mouth_m)
Aggdata_June52025$dist_to_shoreline_m<-as.numeric(Aggdata_June52025$dist_to_shoreline_m)
Aggdata_June52025$Depth_m<-as.numeric(Aggdata_June52025$Depth_m)
Aggdata_June52025$Polygon<-as.factor(Aggdata_June52025$Polygon)

summary_allactch <- Aggdata_June52025 %>%
  group_by(Date, Polygon) %>%
  summarise(
    mean_Temp = mean(Temp, na.rm = TRUE),
    mean_DO = mean(DO, na.rm = TRUE),
    mean_depth = mean(Depth_m, na.rm = TRUE),
    mean_disttoRM = mean(distance_to_river_mouth_m, na.rm = TRUE),
    mean_disttoshore = mean(dist_to_shoreline_m, na.rm = TRUE),
    sum_total=sum(Total, na.rm=TRUE), 
    n_observations = n(),
    .groups = "drop"
  )


fiveormoreaggs <- filter(Aggdata_June52025, Total >= 5)

summary_aggdataonly <- fiveormoreaggs %>%
  group_by(Date, Polygon) %>%
  summarise(
    mean_Temp = mean(Temp, na.rm = TRUE),
    mean_DO = mean(DO, na.rm = TRUE),
    mean_depth = mean(Depth_m, na.rm = TRUE),
    mean_disttoRM = mean(distance_to_river_mouth_m, na.rm = TRUE),
    mean_disttoshore = mean(dist_to_shoreline_m, na.rm = TRUE),
    sum_total=sum(Total, na.rm=TRUE), 
    n_observations = n(),
    .groups = "drop"
  )

##also adding min and max values could also get standard error or confidence intervals
 
summary_aggdataonly_simple <- fiveormoreaggs %>%
  group_by(Date, Polygon) %>%
  summarise(
    mean_Temp = mean(Temp, na.rm = TRUE),
    min_Temp = min(Temp, na.rm = TRUE),
    max_Temp = max(Temp, na.rm = TRUE),
    
    mean_DO = mean(DO, na.rm = TRUE),
    min_DO = min(DO, na.rm = TRUE),
    max_DO = max(DO, na.rm = TRUE),
    
    mean_depth = mean(Depth_m, na.rm = TRUE),
    min_depth = min(Depth_m, na.rm = TRUE),
    max_depth = max(Depth_m, na.rm = TRUE),
    
    mean_disttoRM = mean(distance_to_river_mouth_m, na.rm = TRUE),
    min_disttoRM = min(distance_to_river_mouth_m, na.rm = TRUE),
    max_disttoRM = max(distance_to_river_mouth_m, na.rm = TRUE),
    
    mean_disttoshore = mean(dist_to_shoreline_m, na.rm = TRUE),
    min_disttoshore = min(dist_to_shoreline_m, na.rm = TRUE),
    max_disttoshore = max(dist_to_shoreline_m, na.rm = TRUE),
    
        Total_agg_mean = mean(Total, na.rm = TRUE),
    Total_agg_min = min(Total, na.rm = TRUE),
    Total_agg_max = max(Total, na.rm = TRUE),
    
    sum_total = sum(Total, na.rm = TRUE), 
    n_observations = n(),
    .groups = "drop"
  )

#save as a csv we can format a table 
#write.csv(summary_aggdataonly_simple, "agg_data_summary.csv", row.names = FALSE)


#now was a brief summary of the transect sampling data, polygon data
#summarize temp, do, depth, per sampling date


#Transect_samplingdata_cleanJune2025

Transect_samplingdata_cleanJune2025$Temp<-as.numeric(Transect_samplingdata_cleanJune2025$Temp)
Transect_samplingdata_cleanJune2025$DO_mgL<-as.numeric(Transect_samplingdata_cleanJune2025$DO_mgL)
Transect_samplingdata_cleanJune2025$Depth<-as.numeric(Transect_samplingdata_cleanJune2025$Depth)
Transect_samplingdata_cleanJune2025$Polygon<-as.factor(Transect_samplingdata_cleanJune2025$Polygon)

Transect_samplingdata_cleanJune2025$NTU<-as.numeric(Transect_samplingdata_cleanJune2025$NTU)

Polygons_summaryinfo <- Transect_samplingdata_cleanJune2025 %>%
  group_by(Date, Polygon) %>%
  summarise(
    mean_Temp = mean(Temp, na.rm = TRUE),
    min_Temp = min(Temp, na.rm = TRUE),
    max_Temp = max(Temp, na.rm = TRUE),
    
    mean_DO = mean(DO_mgL, na.rm = TRUE),
    min_DO = min(DO_mgL, na.rm = TRUE),
    max_DO = max(DO_mgL, na.rm = TRUE),
    
    mean_depth = mean(Depth, na.rm = TRUE),
    min_depth = min(Depth, na.rm = TRUE),
    max_depth = max(Depth, na.rm = TRUE),
    
      mean_turbidity = mean(NTU, na.rm = TRUE),
    min_turb = min(NTU, na.rm = TRUE),
    max_turb = max(NTU, na.rm = TRUE),
    
    start_Time = min(Time, na.rm = TRUE),
    end_Time = max(Time, na.rm = TRUE),
    .groups = "drop"
  )

#write.csv(Polygons_summaryinfo, "transect_data_summary.csv", row.names = FALSE)


##efishing average settings
Transect_samplingdata_cleanJune2025$Power<-as.numeric(Transect_samplingdata_cleanJune2025$Power)
Transect_samplingdata_cleanJune2025$Amps<-as.numeric(Transect_samplingdata_cleanJune2025$Amps)
Transect_samplingdata_cleanJune2025$Duty_cycle<-as.numeric(Transect_samplingdata_cleanJune2025$Duty_cycle)
Transect_samplingdata_cleanJune2025$Volts<-as.numeric(Transect_samplingdata_cleanJune2025$Volts)
Transect_samplingdata_cleanJune2025$Hertz<-as.numeric(Transect_samplingdata_cleanJune2025$Hertz)


efishingavg_settings<- Transect_samplingdata_cleanJune2025 %>%
  group_by() %>%
  summarise(
    mean_Power = mean(Power, na.rm = TRUE), 
    min_Power = min(Power, na.rm = TRUE),
     max_Power = max(Power, na.rm = TRUE),
    mean_amps = mean(Amps, na.rm = TRUE),
     min_amps = min(Amps, na.rm = TRUE),
     max_amps = max(Amps, na.rm = TRUE),
    mean_duty = mean(Duty_cycle, na.rm = TRUE),
      mean_volts = mean(Volts, na.rm = TRUE),
       min_volts = min(Volts, na.rm = TRUE),
       max_volts = max(Volts, na.rm = TRUE),
    mean_hertz = mean(Hertz, na.rm = TRUE),
  
    .groups = "drop"
  )


####

summary_aggdataonly <- Aggdata_June52025 %>%
  group_by(Date, Polygon) %>%
  summarise(
    total_captured = sum(Number_captured, na.rm = TRUE),
    Total_missed = sum(`Number missed`, na.rm = TRUE),
    sum_total=sum(Total, na.rm=TRUE), 
    n_observations = n(),
      n_observations_missed = sum(!is.na(`Number missed`)),
    .groups = "drop"
  )

fiveormoreaggs <- filter(Aggdata_June52025, Total >= 5)

fiveormoreaggs_capmissed <- fiveormoreaggs %>%
  group_by(Date, Polygon) %>%
  summarise(
    total_captured = sum(Number_captured, na.rm = TRUE),
    Total_missed = sum(`Number missed`, na.rm = TRUE),
    sum_total=sum(Total, na.rm=TRUE), 
    n_observations = n(),
      n_observations_missed = sum(!is.na(`Number missed`)),
    .groups = "drop"
  )

lessthan5 <- filter(Aggdata_June52025, Total < 5)

lessthan5_capmissed <- lessthan5 %>%
  group_by(Date, Polygon) %>%
  summarise(
    total_captured = sum(Number_captured, na.rm = TRUE),
    Total_missed = sum(`Number missed`, na.rm = TRUE),
    sum_total=sum(Total, na.rm=TRUE), 
    n_observations = n(),
      n_observations_missed = sum(!is.na(`Number missed`)),
    .groups = "drop"
  )
