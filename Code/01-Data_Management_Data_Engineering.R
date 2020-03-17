##### CODE FILE FOR COAL MINING THESIS PROJECT ######
## CODE FILE 1 - READING IN DATA AND DATA MANAGEMENT

#Set up required packages
library(tidyverse)

#Creating functions for transformation
#preparing standardizing function. 
gel_std <- function(x){
  #Observations are centered and then standardized by 2 Standard Deviations
  #This technique is based on Gelman (2008)
  (x - mean(x)) / (2 * sd(x))
}

#Function for mutate all call
perc_multpl <- function(x) round((x * 100), 3)


#Set Working Directory
#setwd(file.path("...", "Data"))

#Data Management ####
#Reading in Data
coal_data <- read_csv("Full_Data.csv")

#joining in health indicator variables from county health ranking
coal_data <- read_csv(file.path("Health_Indicators_County_level.csv"))%>% # Reading in Data
  filter(FIPS %in% coal_data$FIPS)%>% # Filtering out data that is not part of the coal mining data due to sampling
  mutate_at(vars(everything(), -FIPS, -starts_with("PopTo")), perc_multpl)%>% #turn into percentages
  pivot_longer(cols = -FIPS, names_to = "year_type", values_to = "value")%>% # transform county data into long format
  separate(year_type, into=c("type","year"), sep = "_")%>% # Seperating name columns into year and type
  pivot_wider(id_cols = c(FIPS, year, type), names_from = type, values_from = value)%>% # transform back to wide format
  mutate(year = as.numeric(year))%>% 
  inner_join(coal_data)%>% #Inner join with coal mining data to ensure overlap
  group_by(State, year)%>% #Imputating missing values on county level with state level values
  mutate(smoking_m = ifelse(is.na(smoking), mean(smoking, na.rm=T), smoking), #Replacing missing values 
         drinking_m = ifelse(is.na(drinking), mean(drinking, na.rm=T), drinking), #with state/year means
         obesity_m = ifelse(is.na(obesity), mean(obesity, na.rm=T), obesity),
         uninsured_m = ifelse(is.na(uninsured), mean(uninsured, na.rm=T), uninsured),
         PopToPCP_m = ifelse((is.na(PopToPCP)|PopToPCP<=0), mean(PopToPCP, na.rm=T), PopToPCP))%>%
  dplyr::select(everything(), -smoking, -drinking, -obesity, -uninsured, -PopToPCP)# reducing variables to imputed vars

#Data Engineering
#rescaling time as counter variable for multilevel modeling
coal_data$time <- coal_data$year-min(coal_data$year)

#Measurement of Independent variable: coal variables
#dichotomous coal_mining: 1 for presence of any coal mining on county level
coal_data$coal_mining <- as.numeric(coal_data$coal_prod>0)

#dichotomoud median_mining: 1 for counties with coal production above median level
#Note that the median level is the median of coal producing counties *NOT* of all counties
coal_data$median_mining <- as.numeric(coal_data$coal_prod>median(coal_data$coal_prod[coal_data$coal_mining>0]))

#creating south: Indicator variable for southern states
south <- c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky", "Louisiana", "Mississippi",
           "North Carolina", "South Carolina", "Tennessee", "Texas", "Virginia")
coal_data$southern <- as.numeric(coal_data$State %in% south)

#Applying standardization to continuous variables
coal_data <- cbind(coal_data, setNames(lapply(coal_data[7:27], gel_std),
                   paste0(names(coal_data)[7:27], "_std")))%>% #creating new Variable Names
  dplyr::select(FIPS, year, State, County, rural, Appalachia, southern, median_mining, #selecting and ordering variables
         coal_mining, time, dplyr::ends_with("_std")) 