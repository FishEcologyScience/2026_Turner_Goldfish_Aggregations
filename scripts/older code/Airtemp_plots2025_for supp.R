
######################
##airtemp
p<-ggplot(RBG_airtemp_climate_hourly_ON_6153301_04_2025_P1H_in_, aes(x = Date_Time, y = `Temp_°C`)) +
      geom_line() +
      labs(title = "Temperature Over Time",
           x = "Time",
           y = "Temperature (°C)") +
      theme_minimal()

p


#maybe want to do four plots of 2 days plus sample day leading up to samplin g

# Convert Date_Time to POSIXct format
RBG_airtemp_climate_hourly_ON_6153301_04_2025_P1H_in_$Date_Time <- 
  as.POSIXct(RBG_airtemp_climate_hourly_ON_6153301_04_2025_P1H_in_$Date_Time, 
             format = "%m/%d/%Y %H:%M")


#call out sampling dates so we can highlight in the plot

# Define your 4 sampling dates (adjust these to your actual dates)
sampling_dates <- as.POSIXct(c("4/9/2025", "4/14/2025", "4/22/2025", "4/28/2025"), 
                             format = "%m/%d/%Y")

# Create the plot
unique(RBG_airtemp_climate_hourly_ON_6153301_04_2025_P1H_in_$Year)

RBG_airtemp_climate_hourly_ON_6153301_04_2025_P1H_in_$Year <- 
  as.numeric(RBG_airtemp_climate_hourly_ON_6153301_04_2025_P1H_in_$Year)


p <- ggplot(RBG_airtemp_climate_hourly_ON_6153301_04_2025_P1H_in_, 
            aes(x = Date_Time, y = Temp)) +
      geom_hline(yintercept = 9.7, color = "black", linetype = "dashed", linewidth = 0.8) +
      geom_line(aes(color = Temp), linewidth = 1) +
      scale_color_gradient(low = "blue", high = "red", 
                          name = "Temperature (°C)") +
      scale_x_datetime(expand = c(0.02, 0)) +  # Minimal padding
      labs(title = "",
           x = "Date",
           y = "Temperature (°C)") + 
      facet_wrap(~Year, scales = "free_x") +
      theme_minimal()
p


levels(RBG_airtemp_climate_hourly_ON_6153301_04_2025_P1H_in_$Year)

p <- ggplot(RBG_airtemp_climate_hourly_ON_6153301_04_2025_P1H_in_, 
            aes(x = Date_Time, y = Temp)) +
      # Add grey bars for sampling dates (only on 2025 panel)
      geom_rect(data = data.frame(xmin = sampling_dates - 12*3600,
                                  xmax = sampling_dates + 12*3600,
                                  ymin = -Inf, 
                                  ymax = Inf,
                                  Year = 2025),
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = "grey68", alpha = 0.5, inherit.aes = FALSE) +
      geom_hline(yintercept = 9.7, color = "black", linetype = "dashed", linewidth = 0.8) +
      geom_line(aes(color = Temp), linewidth = 1) +
      scale_color_gradient(low = "blue", high = "red", 
                          name = "Temperature (°C)") +
      scale_x_datetime(expand = c(0.02, 0)) +  # Minimal padding
      labs(title = "",
           x = "Date",
           y = "Temperature (°C)") + 
      facet_wrap(~Year, scales = "free_x") +
      theme_minimal()
p













##mean daytime temps
#calculate mean daytime temperatures (9am-6pm) for sampling dates
library(dplyr)
library(lubridate)

# Define your sampling dates
sampling_dates <- as.Date(c("2025-04-05", "2025-04-10", "2025-04-15", "2025-04-20"))

# Calculate daytime means
daytime_means <- RBG_airtemp_climate_hourly_ON_6153301_04_2025_P1H_in_ %>%
  mutate(Date = as.Date(Date_Time),
         Hour = hour(Date_Time)) %>%
  filter(Hour >= 9 & Hour <= 18) %>%  # 9am to 6pm (18:00)
  group_by(Date) %>%
  summarise(Mean_Daytime_Temp = mean(`Temp_°C`, na.rm = TRUE),
            .groups = 'drop')

# Print the results
print("Mean Daytime Temperatures (9am-6pm) for Sampling Dates:")
print(daytime_means)













#think we need to get CGDD and link back to telemetry resutls (dates of aggregations for 2022 and 2023)
#then also get this info for 2025 so we can link it to #s we found in carrolls bay

#thinking about mean daily depth during the timeframe period 
#can we do a simple linear model where we assess the 4 depth time periods across the small 

#month of April
#mean_depth~time period (4 categories) + fish_ID is random
lmer(mean_depth ~ time_period + (1 | fish_ID), data = your_data)
#then run a tukeys post-hoc
#OR can do it where we create a binary column in the depth dataframe for evertyhing above/below 1.5m
#GLMM shallow~time_period+fishid random, family=binomial 
#run a tukeys

###calculate Cumulative growing degree day based on air temps (Boston et al. 2024 based on water temps)

# GDDi=Tmeani-Tbase
#GDD degree day from the day of i year, Tmeani is the mean daily temperature and Tbase (base temp) is set to 5'C

binom.test(18, 49, p = 0.5, alternative = "two.sided")

y2 <- 459
n2 <- 18
y4 <- 341
n4 <- 31

poisson.test(c(y2, y4), T = c(n2, n4), alternative = "two.sided")


#####re-run the light analysis and then do a quick lm model with post-hoc? to see if proportion is in fact sigificantly greater during dusk and night compared to day and dawn

library(glatos)
