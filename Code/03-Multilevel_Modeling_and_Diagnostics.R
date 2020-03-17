##### CODE FILE FOR COAL MINING THESIS PROJECT ######
## CODE FILE 3 - Multilevel Modeling and Diagnostics

#Set up required packages
library(lme4)
library(lmerTest)
library(AICcmodavg)
library(broom.mixed)
library(tidyr)
library(AICcmodavg)
library(reghelper)
 
#unconditional mean model####
uncond_mean_mod <- lmer(data=coal_data, mortality ~ 1 + (1|State))
#summary(uncond_mean_mod)
#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(uncond_mean_mod)%>%
  write_csv(path="Data/Tables/Model_Summary/01-uncond_mean_mod_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=ICC(uncond_mean_mod), BIC=BIC(uncond_mean_mod), AICc=AICc(uncond_mean_mod))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/01-uncond_mean_mod_stats.csv")


#unconditional growth model####
#Fixed Slope but random intercept
uncond_growth_mod <- lmer(data=coal_data, formula = mortality ~ coal_mining + (1|State))
#summary(uncond_growth_mod)
tidy(uncond_growth_mod)%>%
  write_csv(path="Data/Tables/Model_Summary/02-uncond_growth_mod_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=ICC(uncond_growth_mod), BIC=BIC(uncond_growth_mod), AICc=AICc(uncond_growth_mod))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/02-uncond_growth_mod_stats.csv")

#Random Slope
uncond_growth_mod_rs <- lmer(data=coal_data, formula = mortality ~ coal_mining + (coal_mining|State))
#summary(uncond_growth_mod_rs)

tidy(uncond_growth_mod_rs)%>%
  write_csv(path="Data/Tables/Model_Summary/02a-uncond_growth_mod_rs_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=ICC(uncond_growth_mod_rs), BIC=BIC(uncond_growth_mod_rs), AICc=AICc(uncond_growth_mod_rs))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/02a-uncond_growth_mod_rs_stats.csv")



#Full Model #####
full_model <- lmerTest::lmer(data=coal_data, formula = mortality ~ time + I(time^2) + coal_mining + median_mining + 
                               Appalachia + rural + unemployment_std + median_income_std + poverty_rate_std +
                               median_age_std + hs_grad_rate_std + ba_higher_rate_std + perc_male_std + 
                               perc_black_std + perc_amerin_std + perc_hisp_std + PopToPCP_m_std + drinking_m_std +
                               obesity_m_std + smoking_m_std + land_area_std + southern + uninsured_m_std +
                               coal_mining:Appalachia + median_mining:Appalachia + perc_hisp_std:perc_male_std +
                               hs_grad_rate_std:coal_mining + ba_higher_rate_std:coal_mining +
                               (coal_mining|State), control=lmerControl(optCtrl=list(maxfun=200000)))

summary(full_model)
#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(full_model)%>%
  write_csv(path="Data/Tables/Model_Summary/full_model_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=ICC(full_model), BIC=BIC(full_model), AICc=AICc(full_model))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/full_model_stats.csv")

#Figure 5 - Coefficient Visualizations #####




#Figure X - Correlation between Ran Slope and Ran Intercept ####
png("Visualizations/FIGURE-Corr.png", width = 8.66, height=5.75, units = "in", res = 600)

ranef(full_model)$State%>%
  rename(ran_intercept=1, ran_slope=2)%>%
  ggplot(aes(x=ran_intercept, y=ran_slope))+
  geom_point()+
  labs(title = "Full Multilevel Regression Model, All Groups",
       subtitle = "Random Intercept Values vs. Random Slope Values",
       x="Random Intercept", y="Random Slope")+
  theme_bw()

dev.off()

#Fig X (App) Correlation Ran_Slop, Ran_Int - full model vs. uncond growth#####
png("Visualizations/FIGURE-App-Corr_Comp.png", width = 8.66, height=5.75, units = "in", res = 600)

ranef(full_model)$State%>%
  rename(ran_intercept_full=1, ran_slope_full=2)%>%
  mutate(ran_intercept_uncond = ranef(uncond_growth_mod_rs)$State[,1],
         ran_slope_uncond = ranef(uncond_growth_mod_rs)$State[,2])%>%
  ggplot()+
  geom_point(aes(x=ran_intercept_full, y=ran_slope_full), color="gray")+
  geom_point(aes(x=ran_intercept_uncond, y=ran_slope_uncond), color="black")+
  labs(title = "Multilevel Regression Model, Full Model vs. Unconditional Growth Model",
       subtitle = "Random Intercept Values vs. Random Slope Values",
       x="Random Intercept", y="Random Slope",
       caption = "Black= Unconditional Growth Model, Gray = Full Model")+
  theme_bw()

dev.off()
