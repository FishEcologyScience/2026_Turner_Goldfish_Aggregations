#need to get one dataframetogether with presence and absences for each transect sampled
#with total number of fish captured in each event

#occurances with goldfish need to summarize that down to just be total number captured in each transect

#summarize aggdata_june2025
head(Aggdata_June52025)

Aggdata_summary <- Aggdata_June52025 %>%
  group_by(Date, Transect, StartTime) %>%
  summarize(total_fish = sum(Total, na.rm = TRUE),
            .groups = 'drop')

#now need to fill in the absences
Aggdata_June52025 <- read_excel("data/raw/Aggdata_June52025.xlsx")
#load in transect sampling data
Transectdata_June52025 <- read_excel("data/raw/Transect samplingdata_cleanJune2025.xlsx")

#to keep it simple to start we are going to have one entry per transect
#date, time start, transect, total captured (zeros and non-zeros), WQ info taken for each transect and depth 


#now need to add TOTAL fish captured column in agg dataframe to the transect dataframe 

#first summarize the agg data to have one total per transect (excluding waypoint information)
#load in transect sampling data

library(dplyr)
summarized_data <- Aggdata_June52025 %>%
  group_by(Date, Transect) %>%
  summarise(total_goldfish = sum(Total, na.rm = TRUE))


#now merge the above dataframe with transect sampling dataframe plus add zeros where not catch was found

transects_withtotalGF <- Transectdata_June52025 %>%
  left_join(summarized_data, by = c("Transect", "Date")) %>%
  mutate(total_goldfish = replace_na(total_goldfish, 0))


#take a look at data, run a model 

mean(transects_withtotalGF$total_goldfish)
var(transects_withtotalGF$total_goldfish)
hist(transects_withtotalGF$total_goldfish)
table(transects_withtotalGF$total_goldfish)

#if variance is much greater than the mean = over disperesed data, negative binomial model
library(lme4)
library(glmmTMB)

#scale the temperature value
transects_withtotalGF$Temp<-as.numeric(transects_withtotalGF$Temp)
transects_withtotalGF$Temp_scaled <- scale(transects_withtotalGF$Temp)[,1]



model1 <- glmer(total_goldfish ~ Temp + (1|Transect), 
                   family = poisson, 
                   data = transects_withtotalGF)

model2 <- glmer(total_goldfish ~ Temp + On_offshore + (1|Transect), 
                   family = poisson, 
                   data = transects_withtotalGF)

anova(model1, model2)
summary(model2)
#temp significant effect, increases in temp=increases in abundance
#on offshore highly significant effect


#check model 
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel = model2)
plot(simulationOutput)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
testUniformity(simulationOutput)
#poisson model assumption is met, 

summarized_data1 <- Aggdata_June52025 %>%
  group_by(Date) %>%
  summarise(total_goldfish = sum(Total, na.rm = TRUE))


#############################################################

