##### CODE FILE FOR COAL MINING THESIS PROJECT ######
## CODE FILE 3 - Multilevel Modeling and Diagnostics
#The code in this file reflects the level selection process rather than the sequence of tables and figures in the thesis.

#Set up required packages####
library(lme4)
library(lmerTest)
library(AICcmodavg)
library(broom.mixed)
library(tidyr)
library(AICcmodavg)
library(reghelper)
library(influence.ME)


#Table 11 Model Selection Process####
#unconditional mean model
uncond_mean_mod <- lmer(data=coal_data, mortality ~ 1 + (1|State))
#summary(uncond_mean_mod)
#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(uncond_mean_mod)%>%
  write_csv(path="Data/Tables/Model_Summary/01-uncond_mean_mod_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(uncond_mean_mod), BIC=BIC(uncond_mean_mod), AICc=AICc(uncond_mean_mod))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/01-uncond_mean_mod_stats.csv")


#unconditional growth model
#Fixed Slope but random intercept
uncond_growth_mod <- lmer(data=coal_data, formula = mortality ~ coal_mining + (1|State))
#summary(uncond_growth_mod)
tidy(uncond_growth_mod)%>%
  write_csv(path="Data/Tables/Model_Summary/02-uncond_growth_mod_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(uncond_growth_mod), BIC=BIC(uncond_growth_mod), AICc=AICc(uncond_growth_mod))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/02-uncond_growth_mod_stats.csv")

#Random Slope
uncond_growth_mod_rs <- lmer(data=coal_data, formula = mortality ~ coal_mining + (coal_mining|State))
#summary(uncond_growth_mod_rs)

tidy(uncond_growth_mod_rs)%>%
  write_csv(path="Data/Tables/Model_Summary/02a-uncond_growth_mod_rs_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(uncond_growth_mod_rs), BIC=BIC(uncond_growth_mod_rs), AICc=AICc(uncond_growth_mod_rs))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/02a-uncond_growth_mod_rs_stats.csv")



#Full Model
full_model <- lmerTest::lmer(data=coal_data, formula = mortality ~ time + I(time^2) + coal_mining + median_mining + 
                               Appalachia + rural + unemployment_std + median_income_std + poverty_rate_std +
                               median_age_std + hs_grad_rate_std + ba_higher_rate_std + perc_male_std + 
                               perc_black_std + perc_amerin_std + perc_hisp_std + PopToPCP_m_std + drinking_m_std +
                               obesity_m_std + smoking_m_std + land_area_std + southern + uninsured_m_std +
                               coal_mining:Appalachia + median_mining:Appalachia + perc_hisp_std:perc_male_std +
                               hs_grad_rate_std:coal_mining + ba_higher_rate_std:coal_mining +
                               (coal_mining|State), control=lmerControl(optCtrl=list(maxfun=200000)))

#summary(full_model)
#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(full_model)%>%
  write_csv(path="Data/Tables/Model_Summary/full_model_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(full_model), BIC=BIC(full_model), AICc=AICc(full_model))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/full_model_stats.csv")

#Footnote Table 11 - Other models of model selection process####
#mortality = f(coal mining, Appalachia)
cm_app_model <- lmerTest::lmer(data=coal_data, formula = mortality ~ coal_mining + Appalachia + 
                                  (coal_mining|State), control=lmerControl(optCtrl=list(maxfun=200000)))

#summary(cm_app_model)
#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(cm_app_model)%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/cm_app_model_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(cm_app_model), AICc=AICc(cm_app_model), BIC=BIC(cm_app_model))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/cm_app_model_stats.csv")


#mortality = f(above median mining)
amm_model <- lmerTest::lmer(data=coal_data, formula = mortality ~ median_mining + 
                               (coal_mining|State), control=lmerControl(optCtrl=list(maxfun=200000)))

#summary(amm_model)
#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(amm_model)%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/amm_model_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(amm_model), AICc=AICc(amm_model), BIC=BIC(amm_model))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/amm_model_stats.csv")

#mortality = f(above median mining, Appalachia)
amm_app_model <- lmerTest::lmer(data=coal_data, formula = mortality ~ median_mining + Appalachia + 
                              (coal_mining|State), control=lmerControl(optCtrl=list(maxfun=200000)))

#summary(amm_app_model)
#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(amm_app_model)%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/amm_app_model_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(amm_app_model), AICc=AICc(amm_app_model), BIC=BIC(amm_app_model))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/amm_app_model_stats.csv")

#mortality = f(coal mining, above median mining, Appalachia)
cm_amm_app_model <- lmerTest::lmer(data=coal_data, formula = mortality ~ coal_mining + median_mining + Appalachia + 
                                 (coal_mining|State), control=lmerControl(optCtrl=list(maxfun=200000)))

#summary(cm_amm_app_model)
#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(cm_amm_app_model)%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/cm_amm_app_model_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(cm_amm_app_model), AICc=AICc(cm_amm_app_model), BIC=BIC(cm_amm_app_model))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/cm_amm_app_model_stats.csv")

#mortality = f(coal mining, above median mining, Appalachia, coal mining * Appalachia, above median mining * Appalachia)
cm_amm_app_int_model <- lmerTest::lmer(data=coal_data, formula = mortality ~ coal_mining + median_mining + Appalachia +
                                     coal_mining:Appalachia + median_mining:Appalachia +
                                     (coal_mining|State), control=lmerControl(optCtrl=list(maxfun=200000)))

#summary(cm_amm_app_int_model)
#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(cm_amm_app_int_model)%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/cm_amm_app_int_model_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(cm_amm_app_int_model), AICc=AICc(cm_amm_app_int_model), BIC=BIC(cm_amm_app_int_model))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/cm_amm_app_int_model_stats.csv")

#mortality = f(coal mining, above median mining, Appalachia, coal mining * Appalachia,
#above median mining * Appalachia, demographics)
cm_amm_app_int_dems_model <- lmerTest::lmer(data=coal_data, formula = mortality ~ coal_mining + median_mining + Appalachia +
                                         unemployment_std + median_income_std + poverty_rate_std + median_age_std + 
                                         hs_grad_rate_std + ba_higher_rate_std + perc_male_std + 
                                         perc_black_std + perc_amerin_std + perc_hisp_std +
                                         coal_mining:Appalachia + median_mining:Appalachia +
                                         (coal_mining|State), control=lmerControl(optCtrl=list(maxfun=200000)))



#summary(cm_amm_app_int_dems_model)
#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(cm_amm_app_int_dems_model)%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/cm_amm_app_int_dems_model_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(cm_amm_app_int_dems_model), AICc=AICc(cm_amm_app_int_dems_model), BIC=BIC(cm_amm_app_int_dems_model))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/Table_11-FN/cm_amm_app_int_dems_model_stats.csv")


#Outlier Analysis after full model is specified ####
#These calculations take a long time

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

#alt_est_b <- influence(full_model, obs=T, count = T)
#influence_obs <- data.frame(cooks_d = cooks.distance(alt_est_b),
#                            pc_change = pchange(alt_est_b),
#                            sig_test = sigtest(alt_est_b),
#                            df_betas = dfbetas(alt_est_b))

#Rather read in the csv file that contains observation level measures in the repository
influence_obs <- read_csv("Data/obs_outlier_diag.csv")

#Creating dataset with obs-level cook's d and indicator for observations above cut-off
coal_data <- coal_data%>%
  ungroup()%>%
  mutate(cooks_d = influence_obs$cooks_d,
         cook_d_out = as.numeric(cooks_d>3*mean(cooks_d)))

#Replacing mortality values for outliers with NA
coal_data_mod <- coal_data
coal_data_mod$mortality[coal_data_mod$cook_d_out==1] <- NA

#Full model without outliers (Table 4)#####
full_model_no <- lmerTest::lmer(data=coal_data_mod, formula = mortality ~ time + I(time^2) + coal_mining + median_mining + 
                                   Appalachia + rural + unemployment_std + median_income_std + poverty_rate_std +
                                   median_age_std + hs_grad_rate_std + ba_higher_rate_std + perc_male_std + 
                                   perc_black_std + perc_amerin_std + perc_hisp_std + PopToPCP_m_std + drinking_m_std +
                                   obesity_m_std + smoking_m_std + land_area_std + southern + uninsured_m_std +
                                   coal_mining:Appalachia + median_mining:Appalachia + perc_hisp_std:perc_male_std +
                                   hs_grad_rate_std:coal_mining + ba_higher_rate_std:coal_mining  +
                                   (coal_mining|State), control=lmerControl(optCtrl=list(maxfun=200000)))
summary(full_model_no)

#Creating CSV file with model coefficients for Full Summary in Appendix
tidy(full_model_no)%>%
  write_csv(path="Data/Tables/Model_Summary/full_model_no_coef.csv")

#Additional Measures
#Intra Class Correlation Coefficient, BIC, AICc
data.frame(ICC=reghelper::ICC(full_model_no), BIC=BIC(full_model_no), AICc=AICc(full_model_no))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/full_model_no_stats.csv")


#influence measure for model w/o outliers####
alt_est_c <- influence(full_model_no, group = "State", count = T)

#Influence measure data frame 
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

#Table 3 - Summary Statistics, Observation-Level Influence Measures####
influence_obs%>%
  summarize(max_cook_d = max(cooks_d),
            min_cook_d = min(cooks_d),
            mean_cook_d = mean(cooks_d),
            median_cook_d = median(cooks_d),
            sd_cook = sd(cooks_d),
            outliers_sum = sum(cooks_d>3*mean(cooks_d)))%>%
  write_csv("Data/Tables/table_3-outlier.csv")

#Table 10, Appendix - State-level Summary Statistics for Excluded Influential Observations ####
#Calculating number of counties that are excluded per state
county_n <- coal_data%>%
  filter(year==2010)%>%
  group_by(State)%>%
  summarise(n_one = length(County),
            n_total= n*8)

#summary statistics for states with excluded counties. 
coal_data%>%
  filter(cook_d_out ==1)%>%
  group_by(State)%>%
  summarise(n_county = length(County),
            mean_mort = mean(mortality),
            prob_mining = mean(coal_mining),
            mean_perc_hisp_std = mean(perc_hisp_std),
            mean_area = mean(land_area_std))%>%
  inner_join(county_n, by = "State")%>%
  mutate(reduc_n_county = n_county/n_total)%>%
  write_csv("Data/Tables/table_App-outlier-State_sum.csv")


#Table 11, Appendix - Summary statistics of excluded observations by year ####
coal_data%>%
  filter(cook_d_out ==1)%>%
  group_by(year)%>%
  summarise(n_county = length(County),
            reduc_n_county =length(County)/2994, 
            mean_mort = mean(mortality),
            prob_mining = mean(coal_mining),
            mean_perc_hisp_std = mean(perc_hisp_std),
            mean_area = mean(land_area_std))%>%
  write_csv("Data/Tables/table_App-outlier-year_sum.csv")

# Figure 13-15, Appendix - influence measurement beta-coefficients ####
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



#Figure NOT INCLUDED - Coefficient Visualizations Full Model/Full Data with outliers #####
png("Visualizations/Full-Model-Vis.png", width = 8.66, height=5.75, units = "in", res = 600)

full_model%>%
  tidy(conf.int = TRUE)%>%
  filter(effect =="fixed" & term != "(Intercept)")%>%
  mutate(term = factor(term, levels = term[order(estimate)]),
         significance = as.factor(as.numeric(p.value <= 0.05)))%>%
  ggplot(aes(x = term, y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  theme_bw() +
  geom_hline(yintercept = 0.0, color = 'red', size = 1.0) +
  geom_point(aes(color=significance), show.legend = F) +
  geom_linerange() + coord_flip() + 
  #including new labels and order of labels
  scale_x_discrete(labels=c("coal_mining:Appalachia"="Interaction Coal Mining/Appalachia",
                            "median_mining:Appalachia"="Interaction Median Mining/Appalachia",
                            "perc_male_std:perc_hisp_std"="Interaction Percent Hispanic/Percent Male",
                            "coal_mining:hs_grad_rate_std"="Interaction HS Grad Rate/Coal Mining",
                            "coal_mining:ba_higher_rate_std"="Interaction BA & Higher/Coal Mining",
                            "smoking_m_std" =  "Smoking Rate", "land_area_std" = "County Size",
                            "drinking_m_std" = "Alcohol Consumption", "obesity_m_std" = "Obesity Rate",
                            "perc_hisp_std" = "Percent Hispanic", "PopToPCP_m_std" = "Physician Access",
                            "perc_amerin_std" = "Percent Native American", "perc_asian_std" = "Percent Asian" ,
                            "perc_male_std" = "Percent Male", "perc_black_std" = "Percent Black",
                            "ba_higher_rate_std" = "BA or Higher", "total_population_std" ="Total Population",
                            "median_age_std" = "Median Age", "hs_grad_rate_std" = "High School Grad Rate",
                            "median_income_std" = "Median Income" , "poverty_rate_std" = "Poverty Rate",
                            "Appalachia" = "Appalachia" , "rural" =" Rural", "unemployment_std" = "Unemployment Rate", 
                            "coal_mining"="Coal Mining" , "median_mining"="Above Median Mining",
                            "time" = "Time", "I(time^2)" = "Time, squared", "southern"="Southern State",
                            "uninsured_m_std"="Percent Uninsured")) +
  labs(y="Estimate", x="",
       title = "Multilevel Regression Model Predicting Countylevel Mortality Rates",
       subtitle="Regression Coefficients and Confidence Intervals, Full Model")

dev.off()

#Figure 6 - Coefficient Visualizations - No Outliers #####
png("Visualizations/Full-Model-No-Out-Vis.png", width = 8.66, height=5.75, units = "in", res = 600)

full_model_no%>%
  tidy(conf.int = TRUE)%>%
  filter(effect =="fixed" & term != "(Intercept)")%>%
  mutate(term = factor(term, levels = term[order(estimate)]),
         significance = as.factor(as.numeric(p.value <= 0.05)))%>%
  ggplot(aes(x = term, y = estimate,
             ymin = conf.low,
             ymax = conf.high)) +
  theme_bw() +
  geom_hline(yintercept = 0.0, color = 'red', size = 1.0) +
  geom_point(aes(color=significance), show.legend = F) +
  geom_linerange() + coord_flip() + 
  #including new labels and order of labels
  scale_x_discrete(labels=c("coal_mining:Appalachia"="Interaction Coal Mining/Appalachia",
                            "median_mining:Appalachia"="Interaction Median Mining/Appalachia",
                            "perc_male_std:perc_hisp_std"="Interaction Percent Hispanic/Percent Male",
                            "coal_mining:hs_grad_rate_std"="Interaction HS Grad Rate/Coal Mining",
                            "coal_mining:ba_higher_rate_std"="Interaction BA & Higher/Coal Mining",
                            "smoking_m_std" =  "Smoking Rate", "land_area_std" = "County Size",
                            "drinking_m_std" = "Alcohol Consumption", "obesity_m_std" = "Obesity Rate",
                            "perc_hisp_std" = "Percent Hispanic", "PopToPCP_m_std" = "Physician Access",
                            "perc_amerin_std" = "Percent Native American", "perc_asian_std" = "Percent Asian" ,
                            "perc_male_std" = "Percent Male", "perc_black_std" = "Percent Black",
                            "ba_higher_rate_std" = "BA or Higher", "total_population_std" ="Total Population",
                            "median_age_std" = "Median Age", "hs_grad_rate_std" = "High School Grad Rate",
                            "median_income_std" = "Median Income" , "poverty_rate_std" = "Poverty Rate",
                            "Appalachia" = "Appalachia" , "rural" =" Rural", "unemployment_std" = "Unemployment Rate", 
                            "coal_mining"="Coal Mining" , "median_mining"="Above Median Mining",
                            "time" = "Time", "I(time^2)" = "Time, squared", "southern"="Southern State",
                            "uninsured_m_std"="Percent Uninsured")) +
  labs(y="Estimate", x="",
       title = "Multilevel Regression Model Predicting County-level Mortality Rates",
       subtitle="Regression Coefficients and Confidence Intervals, No Outlier Data")

dev.off()

# Figure 7 - Correlation slope intercept, no outliers ####
png("Visualizations/FIGURE-Corr-no_out.png", width = 8.66, height=5.75, units = "in", res = 600)

ranef(full_model_no)$State%>%
  rename(ran_intercept=1, ran_slope=2)%>%
  ggplot(aes(x=ran_intercept, y=ran_slope))+
  geom_point()+
  labs(title = "Full Multilevel Regression Model, All Groups",
       subtitle = "Random Intercept Values vs. Random Slope Values",
       x="Random Intercept", y="Random Slope")+
  theme_bw()

dev.off()

#Figure 7 - Correlation between Ran Slope and Ran Intercept ####
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

#Fig 16, Appendix - Correlation Ran_Slop, Ran_Int - full model vs. uncond growth#####
png("Visualizations/FIGURE-App-Corr_Comp.png", width = 8.66, height=5.75, units = "in", res = 600)

ranef(full_model_no)$State%>%
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

#Diagnostics ####
#Add Diagnostics to data set
coal_data_diag <- coal_data%>%
  filter(cook_d_out == 0)%>%
  ungroup()%>%
  mutate(resid_value = residuals(full_model_no),
         fit_value = fitted(full_model_no))
#Figure 8 - Fitted vs. y-Values  and total distribution####

q1<-coal_data_diag%>%
  ggplot(aes(x=mortality, y=fit_value))+
  geom_point()+
  theme_bw()+
  labs(title = "Regression Diagnostics Plot",
       subtitle = "Fitted Values vs. Actual Values",
       y= "Fitted Values", x="Mortality")

q2 <- coal_data_diag%>%
ggplot(aes(x = resid_value)) + 
  geom_density() +
  geom_vline(xintercept = mean(coal_data_diag$resid_value), size=0.5, color="blaCK", linetype=2)+
  geom_text(aes(x= mean(resid_value), label="Mean Value", y=0.0025), colour="black", hjust = -0.1)+
  theme_bw()+
  labs(title = "Multilevel Regression Model Predicting Countylevel Mortality Rates",
       subtitle="Total Residual Distribution",
       x="Residual Value",
       y="Density")

png("Visualizations/Diag-Fit_vs_Mort.png", width = 8.66, height=5.75, units = "in", res = 600)
grid.arrange(q1, q2, nrow=2)
dev.off()

# Figure 9 - Fitted vs. Residuals ####
png("Visualizations/Diag-Fit_vs_Resid.png", width = 8.66, height=5.75, units = "in", res = 600)
coal_data_diag%>%
  ggplot(aes(x=resid_value, y=fit_value))+
  geom_point()+
  theme_bw()+
  labs(title = "Regression Diagnostics Plot",
       subtitle = "Fitted Values vs. Residual Values",
       y= "Fitted Values", x="Residual Values")

dev.off()

#Figure 10 Residuals Faceted by year ####
png("Visualizations/Diag-by-Year.png", width = 8.66, height=5.75, units = "in", res = 600)

coal_data_diag%>%
  ggplot(aes(y=fit_value, x=resid_value))+
  geom_point()+
  facet_wrap(~year, scales = "free")+
  theme_bw()+
  labs(title = "Regression Model Diagnostics",
       subtitle = "Residuals vs. Fitted Values, Facetted by Year",
       y="Fitted Value",
       x="Residual Value")
dev.off()

#Figure 12 - Model Comparison####
#Creating pooled OLS model
pool_model <- lm(data=coal_data,  formula = mortality ~ coal_mining + median_mining + 
                   Appalachia + rural + unemployment_std + median_income_std + poverty_rate_std +
                   median_age_std + hs_grad_rate_std + ba_higher_rate_std + perc_male_std + 
                   perc_black_std + perc_amerin_std + perc_hisp_std + PopToPCP_m_std + drinking_m_std +
                   obesity_m_std + smoking_m_std + land_area_std + southern + uninsured_m_std +
                   coal_mining:Appalachia + median_mining:Appalachia + perc_hisp_std:perc_male_std +
                   hs_grad_rate_std:coal_mining + ba_higher_rate_std:coal_mining)
#summary(pool_model)
#Regression Output
tidy(pool_model)%>%
  write_csv(path="Data/Tables/Model_Summary/pool_model_coef.csv")
#Additional Measures
#BIC, AICc (There is no ICC for pooled data)
data.frame(AICc=AICc(pool_model), BIC=BIC(pool_model))%>%
  pivot_longer(cols=everything(), names_to = "Measure", values_to = "Statistic")%>%
  write_csv(path="Data/Tables/Model_Summary/pool_model_stats.csv")
#Create diagnostic dataset
pool_diag <- coal_data%>%
  ungroup()%>%
  mutate(resid_value = residuals(pool_model),
         fit_value = fitted(pool_model))

#Create figure panels
p1 <- pool_diag%>%
  ggplot(aes(x=mortality, y=fit_value))+
  geom_point()+
  theme_bw()+
  labs(title = "Pooled Data OLS",
       subtitle = "Fitted Values vs. Actual Values",
       y= "Fitted Values", x="Mortality")

p2 <- coal_data_diag%>%
  ggplot(aes(x=mortality, y=fit_value))+
  geom_point()+
  theme_bw()+
  labs(title = "Mulitlevel Regression Model",
       subtitle = "Fitted Values vs. Actual Values",
       y= "Fitted Values", x="Mortality")

p3 <- pool_diag%>%
  ggplot(aes(x=resid_value, y=fit_value))+
  geom_point()+
  theme_bw()+
  labs(title = "Pooled Data OLS",
       subtitle = "Fitted Values vs. Residual Values",
       y= "Fitted Values", x="Residual Values")

p4 <- coal_data_diag%>%
  ggplot(aes(x=resid_value, y=fit_value))+
  geom_point()+
  theme_bw()+
  labs(title = "Mulitlevel Regression Model",
       subtitle = "Fitted Values vs. Residual Values",
       y= "Fitted Values", x="Residual Values")

#create figure
png("Visualizations/Pool_vs_MLM-Diag.png", width = 8.66, height=5.75, units = "in", res = 600)
grid.arrange(p1, p2, p3, p4, nrow=2)
dev.off()

#Appendix E - Residuals by state ####
#Creating list of all states
state_list <- as.data.frame(table(coal_data_diag$State))[,1]
#creating sequence list
interval_sq <- seq(from=1, to=51, by=9)

#residual plots - Fitted vs. Residual
for(i in 1:6){

  coal_data_diag%>%
    filter(State %in% state_list[interval_sq[i]:(interval_sq[i]+8)])%>% 
    ggplot(aes(x=fit_value, y=resid_value))+
    geom_point()+
    facet_wrap(~State, scales = "free", nrow=3)+
    theme_bw()+
    labs(title = "Regression Model Diagnostics",
         subtitle = "Residuals vs. Fitted Values, Facetted by Year",
         x="Fitted Value",
         y="Residual Value")
  
  ggsave(paste("Visualizations/Residual_states/State", interval_sq[i],"-",(interval_sq[i]+8),".png" ,sep = ""),
         width = 8.66, height=5.75, units = "in", dpi = 600)
}

#Residual plots - Residual Density
for(i in 1:6){
  
  coal_data_diag%>%
    filter(State %in% state_list[interval_sq[i]:(interval_sq[i]+8)])%>% 
    ggplot(aes(x=resid_value))+
    geom_density()+
    facet_wrap(~State, scales = "free", nrow=3)+
    theme_bw()+
    labs(title = "Regression Model Diagnostics",
         subtitle = "Residuals Density",
         x="Residual Value",
         y="Density")
  
  ggsave(paste("Visualizations/Residual_states/Residual_states_Density", interval_sq[i],"-",(interval_sq[i]+8),
               ".png" ,sep = ""), width = 8.66, height=5.75, units = "in", dpi = 600)
  
}

#Figure 11 - Random Effect Diagnostics ####
png("Visualizations/RanEf-Diag.png", width = 8.66, height=5.75, units = "in", res = 600)
plotREsim(REsim(full_model_no), stat = "median", level = 0.95)  # plot the interval estimates
dev.off()
