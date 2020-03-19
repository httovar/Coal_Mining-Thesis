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

# Outlier Analysis - These calculations take a long time ####

#Cook's Distance for group level.
#Rather than each observation, each group is excluded
#Includes a counter variable
alt_est_a <- influence(full_model, group = "State", count = T)

influence_state <- data.frame(cooks_d = cooks.distance(alt_est_a),
                              pc_change = pchange(alt_est_a),
                              sig_test = sigtest(alt_est_a),
                              df_betas = dfbetas(alt_est_a))

#The function calculates a separate model for every observation in the data set.
#i.e. 23,952 multilevel models. It took about 18h to run.
#Rather read in the csv file that contains observation level measures in the repository

#influence_obs <- read_csv("Data/obs_outlier_diag.csv")

alt_est_b <- influence(full_model, obs=T, count = T)

influence_obs <- data.frame(cooks_d = cooks.distance(alt_est_b),
                            pc_change = pchange(alt_est_b),
                            sig_test = sigtest(alt_est_b),
                            df_betas = dfbetas(alt_est_b))

#Creating dataset with obs-level cook's d and indicator for observations above cut-off

coal_data_mod <- coal_data%>%
  ungroup()%>%
  mutate(cooks_d = cooks.distance(alt_est_b),
         cook_d_out = as.numeric(cooks_d>3*mean(cooks_d)))

#Replacing mortality values for outliers with NA
coal_data_mod$mortality[coal_data_mod$cook_d_out==1] <- NA

#create model without outliers
full_model_no <- lmerTest::lmer(data=coal_data_mod, formula = mortality ~ time + I(time^2) + coal_mining + median_mining + 
                                   Appalachia + rural + unemployment_std + median_income_std + poverty_rate_std +
                                   median_age_std + hs_grad_rate_std + ba_higher_rate_std + perc_male_std + 
                                   perc_black_std + perc_amerin_std + perc_hisp_std + PopToPCP_m_std + drinking_m_std +
                                   obesity_m_std + smoking_m_std + land_area_std + southern + uninsured_m_std +
                                   coal_mining:Appalachia + median_mining:Appalachia + perc_hisp_std:perc_male_std +
                                   hs_grad_rate_std:coal_mining + ba_higher_rate_std:coal_mining +
                                   (coal_mining|State), control=lmerControl(optCtrl=list(maxfun=200000)))
#summary(full_model_not)

#influence measure for model w/o outliers
alt_est_c <- influence(full_model_not, group = "State", count = T)

#Influence measure dataframe 
influence_state_noout <- data.frame(cooks_d = cooks.distance(alt_est_c),
                                    pc_change = pchange(alt_est_c),
                                    sig_test = sigtest(alt_est_c),
                                    df_betas = dfbetas(alt_est_c))

#Figure 5 - Cook's D Group-level before and after outlier treatment####

p1 <- influence_state%>%
  mutate(State = row.names(influence_state))%>%
  ggplot(aes(y=cooks_d, x= as.factor(State)))+ 
  geom_col()+
  theme_bw()+
  geom_hline(yintercept = mean(influence_state$cooks_d))+
  geom_hline(yintercept = 3*mean(influence_state$cooks_d), linetype=2)+
  coord_flip()+
  labs(title = "Cook's Distance on Group-Level",
       subtitle = "Prior to Outlier Treatment",
       y="Cook's Distance",
       x="State")

p2 <- influence_state_noout%>%
  mutate(State = row.names(influence_state_noout))%>%
  ggplot(aes(y=cooks_d, x= as.factor(State)))+ 
  geom_col()+
  geom_hline(yintercept = mean(influence_state$cooks_d))+
  geom_hline(yintercept = 3*mean(influence_state$cooks_d), linetype=2)+
  coord_flip()+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title = "",
       subtitle = "Post Outlier Treatment",
       y="Cook's Distance",
       x="State")

png("Visualizations/Outlier-Group.png", width = 8.66, height=5.75, units = "in", res = 600)
gridExtra::grid.arrange(p1, p2, nrow=1)
dev.off()

# More Plots for Outlier Treatment - Appendix ####
q1 <- influence_state%>%
  mutate(State = row.names(influence_state))%>%
  ggplot(aes(y=df_betas.perc_hisp_std, x= as.factor(State)))+ 
  geom_col()+
  theme_bw()+
  coord_flip()+
  theme_bw()+
  labs(title = "Influence on Hispanic Coefficient",
       subtitle = "Prior Outlier Treatment",
       y="Difference Beta-Coefficient",
       x="State")

q2 <- influence_state%>%
  mutate(State = row.names(influence_state))%>%
  ggplot(aes(y=df_betas.land_area_std, x= as.factor(State)))+ 
  geom_col()+
  theme_bw()+
  coord_flip()+
  theme_bw()+
  labs(title = "Influence on Land Area Coefficient",
       subtitle = "Prior Outlier Treatment",
       y="Difference Beta-Coefficient",
       x="State")


q3 <- influence_state_noout%>%
  mutate(State = row.names(influence_state_noout))%>%
  ggplot(aes(y=df_betas.perc_hisp_std, x= as.factor(State)))+ 
  geom_col()+
  theme_bw()+
  coord_flip()+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme_bw()+
  labs(title = "",
       subtitle = "Post Outlier Treatment",
       y="Difference Beta-Coefficient",
       x="State")


q4 <- influence_state_noout%>%
  mutate(State = row.names(influence_state_noout))%>%
  ggplot(aes(y=df_betas.land_area_std, x= as.factor(State)))+ 
  geom_col()+
  theme_bw()+
  coord_flip()+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme_bw()+
  labs(title = "",
       subtitle = "Post Outlier Treatment",
       y="Difference Beta-Coefficient",
       x="State")


q5 <- influence_state%>%
  mutate(State = row.names(influence_state))%>%
  ggplot(aes(y=df_betas.coal_mining.Appalachia, x= as.factor(State)))+ 
  geom_col()+
  theme_bw()+
  coord_flip()+
  theme_bw()+
  labs(title = "Influence on Coal:Appalachia",
       subtitle = "Prior Outlier Treatment",
       y="Difference Beta-Coefficient",
       x="State")


q6 <- influence_state_noout%>%
  mutate(State = row.names(influence_state_noout))%>%
  ggplot(aes(y=df_betas.coal_mining.Appalachia, x= as.factor(State)))+ 
  geom_col()+
  theme_bw()+
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme_bw()+
  labs(title = "",
       subtitle = "Post Outlier Treatment",
       y="Difference Beta-Coefficient",
       x="State")


png("Visualizations/Appendix-Outlier-Hisp.png", width = 8.66, height=5.75, units = "in", res = 600)
gridExtra::grid.arrange(q1, q3, nrow=1)
dev.off()

png("Visualizations/Appendix-Outlier-Area.png", width = 8.66, height=5.75, units = "in", res = 600)
gridExtra::grid.arrange(q2, q4, nrow=1)
dev.off()

png("Visualizations/Appendix-Outlier-coal_int.png", width = 8.66, height=5.75, units = "in", res = 600)
gridExtra::grid.arrange(q5, q6, nrow=1)
dev.off()

#Figure 6 - Cook's D Observation level




#Figure 6 - Coefficient Visualizations Full Model/Full Data#####
png("Visualizations/Full-Model-Vis.png", width = 8.66, height=5.75, units = "in", res = 600)

full_model%>%
  tidy(conf.int = TRUE)%>%
  filter(effect =="fixed" & term != "(Intercept)")%>%
  mutate(term = factor(term, levels = term[order(estimate)]),
         significance = as.factor(as.numeric(p.value <= 0.05)))%>%
  ggplot(aes(x = term, y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  theme_minimal() +
  geom_hline(yintercept = 0.0, color = 'red', size = 1.0) +
  geom_point(aes(color=significance), show.legend = F) +
  geom_linerange() + coord_flip() + 
  #including new labels and order of labels
  scale_x_discrete(labels=c("coal_mining:Appalachia"="Interaction Coal Mining/Appalachia",
                            "median_mining:Appalachia"="Interaction Median Mining/Appalachia",
                            "perc_male_std:perc_hisp_std"="Interaction Percent Hispanic/Percent Male",
                            "coal_mining:hs_grad_rate_std"="Interaction HS Grad Rate/Coal Mining",
                            "coal_mining:ba_higher_rate_std"="Interaction BA & Higher/Coal Mining",
                            "smoking_std" =  "Smoking Rate", "area_std" = "County Size",
                            "alcohol_std" = "Alcohol Consumption", "obesity_std" = "Obesity Rate",
                            "perc_hisp_std" = "Percent Hispanic", "phys_access_std" = "Physician Access",
                            "perc_amerin_std" = "Percent Native American", "perc_asian_std" = "Percent Asian" ,
                            "perc_male_std" = "Percent Male", "perc_black_std" = "Percent Black",
                            "ba_higher_rate_std" = "BA or Higher", "total_population_std" ="Total Population",
                            "median_age_std" = "Median Age", "hs_grad_rate_std" = "High School Grad Rate",
                            "median_income_std" = "Median Income" , "poverty_rate_std" = "Poverty Rate",
                            "Appalachia" = "Appalachia" , "rural" =" Rural", "unemployment_std" = "Unemployment Rate", 
                            "coal_mining"="Coal Mining" , "median_mining"="Above Median Mining",
                            "time" = "Time", "I(time^2)" = "Time, squared", "southern"="Southern State",
                            "total_population_std"="Total Population")) +
  labs(y="Estimate", x="",
       title = "Multilevel Regression Model Predicting Countylevel Mortality Rates",
       subtitle="Regression Coefficients and Confidence Intervals, Correlated Model")

dev.off()

#Figure 7 - Coefficient Visualizations - No Outliers #####
png("Visualizations/Full-Model-No-Out-Vis.png", width = 8.66, height=5.75, units = "in", res = 600)

full_model%>%
  tidy(conf.int = TRUE)%>%
  filter(effect =="fixed" & term != "(Intercept)")%>%
  mutate(term = factor(term, levels = term[order(estimate)]),
         significance = as.factor(as.numeric(p.value <= 0.05)))%>%
  ggplot(aes(x = term, y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  theme_minimal() +
  geom_hline(yintercept = 0.0, color = 'red', size = 1.0) +
  geom_point(aes(color=significance), show.legend = F) +
  geom_linerange() + coord_flip() + 
  #including new labels and order of labels
  scale_x_discrete(labels=c("coal_mining:Appalachia"="Interaction Coal Mining/Appalachia",
                            "median_mining:Appalachia"="Interaction Median Mining/Appalachia",
                            "perc_male_std:perc_hisp_std"="Interaction Percent Hispanic/Percent Male",
                            "coal_mining:hs_grad_rate_std"="Interaction HS Grad Rate/Coal Mining",
                            "coal_mining:ba_higher_rate_std"="Interaction BA & Higher/Coal Mining",
                            "smoking_std" =  "Smoking Rate", "area_std" = "County Size",
                            "alcohol_std" = "Alcohol Consumption", "obesity_std" = "Obesity Rate",
                            "perc_hisp_std" = "Percent Hispanic", "phys_access_std" = "Physician Access",
                            "perc_amerin_std" = "Percent Native American", "perc_asian_std" = "Percent Asian" ,
                            "perc_male_std" = "Percent Male", "perc_black_std" = "Percent Black",
                            "ba_higher_rate_std" = "BA or Higher", "total_population_std" ="Total Population",
                            "median_age_std" = "Median Age", "hs_grad_rate_std" = "High School Grad Rate",
                            "median_income_std" = "Median Income" , "poverty_rate_std" = "Poverty Rate",
                            "Appalachia" = "Appalachia" , "rural" =" Rural", "unemployment_std" = "Unemployment Rate", 
                            "coal_mining"="Coal Mining" , "median_mining"="Above Median Mining",
                            "time" = "Time", "I(time^2)" = "Time, squared", "southern"="Southern State",
                            "total_population_std"="Total Population")) +
  labs(y="Estimate", x="",
       title = "Multilevel Regression Model Predicting Countylevel Mortality Rates",
       subtitle="Regression Coefficients and Confidence Intervals, Correlated Model")

dev.off()

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
