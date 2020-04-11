##### CODE FILE FOR COAL MINING THESIS PROJECT ######
## CODE FILE 2 - Descriptive Analysis and Visualization
#Run code from Code 1 first.

#Set up required packages
library(tidyverse)
library(gridExtra)

#Set Directory
#setwd(file.path("...", "Coal_Mining"))


#Figure 1 - Mortality Rate Distribution and Mortality Rates vs. Time - by State #######
q1 <- coal_data%>%
  ggplot(aes(x=mortality))+
  geom_histogram(aes(y=..density..),bins = 100, colour="black", fill="white")+
  geom_density(alpha=.2, fill="darkgrey")+
  geom_vline(xintercept = mean(coal_data$mortality), size=1.0, color="blaCK", linetype=1)+
  geom_text(aes(x= mean(coal_data$mortality), label="Mean Value",
                y=0.00275), colour="blaCK", hjust = -0.05)+
  theme_bw()+
  labs(title = "Density Plot Mortality Rates on County Level",
       subtitle = "Counties in United States, 2010-2017",
       x="Age-Adjusted Mortality in Deaths per 100,000 Population",
       y="Density")

q2 <- coal_data%>%
  group_by(State, year)%>%
  summarize(stae_average = mean(mortality),
            coal_mining = max(coal_mining))%>%
  ggplot(aes(x = year, y = stae_average, group = State), show.legend = F) +  
  geom_line(show.legend = F, alpha = 0.6) + 
  theme_bw() + 
  coord_cartesian(xlim = c(2010, 2017))+
  labs(x="Time", y="Mortality Rate",
       title = "State Mortality Rate",
       subtitle = "All State in US, 2010-2017")

png("Visualizations/FIGURE1.png", width = 8.66, height=5.75, units = "in", res = 600)
gridExtra::grid.arrange(q1, q2, nrow=2)
dev.off()

#Figure 2 - Distribution of Coal Mining on County Level #####
png("Visualizations/FIGURE2.png", width = 8.66, height=5.75, units = "in", res = 600)

coal_data %>%
  filter(coal_prod>0)%>%
  ggplot(aes(x=coal_prod))+
  scale_x_log10(labels = scales::comma)+
  geom_histogram(aes(y=..density..),bins = 100, colour="black", fill="white")+
  geom_density(alpha=.2, fill="gray")+
  theme_bw()+
  labs(title = "Density Distribution of Coal Mining on County Level",
       subtitle = "Coal Mining Counties in United States",
       x="Short Tons Mined per Year, logged",
       y="Frequency")+
  geom_vline(xintercept = mean(coal_data$coal_prod[coal_data$coal_prod>0]), size=1.0, color="black", linetype=1)+
  geom_vline(xintercept = median(coal_data$coal_prod[coal_data$coal_prod>0]), size=1.0, color="black", linetype=2)+
  geom_text(aes(x= mean(coal_data$coal_prod[coal_data$coal_prod>0]), label="Mean Value", y=0.7),
            colour="black", hjust = -0.1)+
  geom_text(aes(x=median(coal_data$coal_prod[coal_data$coal_prod>0]), label="Median Value", y=0.7),
            colour="black", hjust = 1.1)

dev.off()


#Figure 3, Coal Mining and Mortality Rates ######
#State level mortality rate, coal vs. non-coal
#specifying colors for plot
p1 <- coal_data%>%
  group_by(State, year)%>%
  summarize(stae_average = mean(mortality),
            coal_mining = max(coal_mining))%>%
  ggplot(aes(x = year, y = stae_average, group=interaction(State, coal_mining), color=as.factor(coal_mining)),
         show.legend = F) +  
  geom_line(alpha=0.5, show.legend = F) + 
  geom_smooth(aes(group=coal_mining, linetype=as.factor(coal_mining)), 
              method="lm", se = FALSE, size = 2, show.legend = F) + 
  theme_bw() + 
  coord_cartesian(xlim = c(2010, 2017))+
  labs(x="Time", y="Mortality Rate",
       title = "State Mortality Rate",
       subtitle = "Coal Mining vs. Non-Mining States",
       caption = "Coal Mining States are represented in blue, non-mining states in red.")+
  scale_color_grey(start=0, end=0.5)


#state level mortality vs. coal production
p2 <- coal_data%>%
  filter(coal_prod>0 & coal_prod<1000000)%>%
  ggplot(aes(x=coal_prod, y=mortality, group=State))+
  stat_smooth(method=lm, fullrange=TRUE, se=F, color="black") +
  geom_point(alpha=0.5)+
  labs(title = "State-level Relationship between Coal Production and Mortality Rates",
       subtitle = "Coal Mining States with fewer than 1,000,000 tons of coal mined per year",
       x="Coal Prodution",
       y="Age-Adjusted Mortality Rate")+
  theme_bw()

png("Visualizations/FIGURE3.png", width = 8.66, height=5.75, units = "in", res = 600)
p2
dev.off()

#Table 2 - Descriptive Analysis of continuous variables, Table Quantities #####

#Reading in intial data set for descriptive analysis
read_csv(file.path("Data/Health_Indicators_County_level.csv"))%>% 
  filter(FIPS %in% read_csv("Data/Full_Data.csv")$FIPS)%>%
  mutate_at(vars(everything(), -FIPS, -starts_with("PopTo")), perc_multpl)%>%
  pivot_longer(cols = -FIPS, names_to = "year_type", values_to = "value")%>% 
  separate(year_type, into=c("type","year"), sep = "_")%>% 
  pivot_wider(id_cols = c(FIPS, year, type), names_from = type, values_from = value)%>% 
  mutate(year = as.numeric(year))%>% 
  inner_join(read_csv("Data/Full_Data.csv"))%>% 
  group_by(State, year)%>% 
  mutate(smoking_m = ifelse(is.na(smoking), mean(smoking, na.rm=T), smoking), 
         drinking_m = ifelse(is.na(drinking), mean(drinking, na.rm=T), drinking), 
         obesity_m = ifelse(is.na(obesity), mean(obesity, na.rm=T), obesity),
         uninsured_m = ifelse(is.na(uninsured), mean(uninsured, na.rm=T), uninsured),
         PopToPCP_m = ifelse((is.na(PopToPCP)|PopToPCP<=0), mean(PopToPCP, na.rm=T), PopToPCP))%>%
  ungroup()%>%
  dplyr::select(coal_prod, land_area, mortality, unemployment,                 #Selecting varibles for desc
                median_income, poverty_rate, median_age, hs_grad_rate, ba_higher_rate,
                perc_male, perc_black, perc_amerin, perc_hisp, smoking_m, drinking_m,
                obesity_m, uninsured_m, PopToPCP_m)%>%
  dplyr::summarize_all(list(desc_min=min, desc_max=max, desc_mean=mean, desc_median=median, desc_std_dev = sd))%>%
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_desc_")%>%
  mutate(val = round(val, 5))%>%
  spread(stat, val) %>%
  write_csv("Data/Tables/table_2-quant.csv")


#Footnote on missing values
read_csv(file.path("Data/Health_Indicators_County_level.csv"))%>% 
  filter(FIPS %in% read_csv("Data/Full_Data.csv")$FIPS)%>%
  dplyr::select(everything(), -FIPS)%>%
  sapply(function(x) sum(is.na(x)))%>%
  as.data.frame()%>%
  summarise(mean_na = mean(.),
            dist = median(.),
            max= max(.),
            min= min(.))
  

#Table Appendix - Descriptives of non-continuous variables Variables by State ####
descr_ind <- coal_data%>%
  ungroup()%>%
  dplyr::select(State, rural, median_mining, coal_mining, Appalachia)%>%
  group_by(State)%>%
  summarize_all(.funs=list(mean=mean))

coal_data%>%
  ungroup()%>%
  dplyr::select(State, rural, median_mining, coal_mining, Appalachia)%>%
  group_by(State)%>%
  summarize(n_county=length(coal_mining))%>%
  inner_join(descr_ind)%>%
  write_csv("Data/Tables/table_app-descr_county.csv")


# Figure 4 Population Distribution ####

#setwd(file.path("...", "Visualizations"))


png("Visualizations/FIGURE4.png", width = 8.66, height=5.75, units = "in", res = 600)
coal_data%>%
  ggplot()+
  geom_density(aes(x=perc_hisp_std), alpha=.2 , fill="blue")+
  geom_density(aes(x=perc_black_std), alpha=.2, fill="darkgrey")+
  geom_density(aes(x=perc_amerin_std), alpha=.2, fill="green")+
  labs(title = "Distribution of Population Variables",
       subtitle = "Centered and Standardized by Two Standard Deviations",
       x="Percentage of Total Population",
       y="Density",
       caption = "Grey = Percentage Black, Blue = Percentage Hipsanic, Green = Percentage American Indian")+
  theme_bw()
dev.off()
