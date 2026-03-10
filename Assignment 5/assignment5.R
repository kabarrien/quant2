#Karolina Barrientos
#Assignment 5 

##Part 1: In Class Assignment====================================================

#1.1 Setup and Data exploration

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 5")

library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(tidyverse)

df = read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")

view(df)

length(unique(df$State))

#50

length(unique(df$Year))

#32

table(table(df$State))

#The panel is unbalanced as there is not the same number of observations per year-state)

#b) Summary Stats for the key variables/ exploring the data

summary(df$PresApprov)

summary(df$UnemPct)

df_sub = df %>%
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_sub, aes(x = Year, y = PresApprov, color = State)) +
  geom_line() + theme_minimal() + labs(x = "Year", y = "Presidential Approval (%)", color = "State")

#The 3 states move closely together over time, tracking the same large swings in approval. The movement can suggest that common national factors are a large driver of approval, while state- level differences are stable.


#c) 
ggplot(raw, aes(x = UnemPct, y = PresApprov, color = State))+
  geom_point(alpha = 0.4)+
  theme_minimal()+
  theme(legend.position="none")+
  labs(x="Unemployment rate (%)", y="Presidential approval (%)")

#Across state-year observations, higher unemployment is associated with lower presidential approval. However, this cross-sectional pattern pools observations across states and years, so it reflects both with in state variation over time and permanent between0state differences in unemployment levels and approval- making it difficult to draw causal conclusions.

#1.2) Pooled OLS

#a) pooled OLS regressing approval on unemployment: 

m_pooled = lm(PresApprov ~ UnemPct, data = df)

summary(m_pooled)

#The coeff on UnemPct is negative: one percentage point increase in the unemployment rate is associated with a decrease of 0.14 percentage points of magnitude in the presidential approval rating. The relationship is not statistically significant as the pvalue on UnemPct is very large. 

#b) Adding south as a control:
m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df)
summary(m_pooled2)

modelsummary(list(m_pooled, m_pooled2), stars = TRUE)

#When adding a control for southern states, the coefficient on UnemPct changes slightly suggesting that the OLS estimate was not strongly confounded by the North-South divide. The model does sow that southern states do differ systematically from the rest in their approval levels, but this difference is not correlated with the unemployment-approval association in this pooled specification. 

#c) The pooled OLS is problematic for panel data because it ignores unobserved, time-invariant differences across states that may be correlated with unemployment. Examples are, states with historically weaker economies may have structurally higher un employment and different political cultures that shape baseline approval, states in particular regions may have persistent partisan leanings that affect how residents evaluate that president independently of economic conditions, and states with large unionized labor forces may have higher unemployment sensitivity and different approval baselines.  

#1.3 Entity fixed effects

#a) state fixed effects

m_fe = feols(PresApprov ~ UnemPct | State,data = df)

modelsummary(list("Pooled OLS" = m_pooled,"State FE" = m_fe),
             vcov = ~State, stars = TRUE, gof_map = c("r.squared", "nobs"))
#b) State effects absorb all time-variant differences across states- including geography, political culture, long-run economic structure, and regional identity. This is why state drops from the model as being in the South does not vary within a state over time, so its effect is indistinguishable from the state-specific intercept (fixed effect)

#c) The coefficient on UnemPct in the state FE model is looking at internal changes rather than comparing different states to one another. The state fixed method isolates temporal shifts within states but remains susceptible to time-varying omitted variable bias. 

#1.4 Two-way fixed effects

#a-b) Adding year fixed effects to control for common time shocks:

m_twefe = feols(PresApprov ~ UnemPct | State + Year, data = df)

modelsummary(list("Pooled" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twefe), vcov = ~State, stars = TRUE, gof_map = c("r.squared", "nobs"))

#c) Year-fixed effects absorb the common time shocks such as national economic cycles and presidential scandals. The movement of unemployment rates and presidential approval will be together if national unemployment rises. This would happen because of experiences at the macro level not at the state-level effect. Adding year dummies removes the source of the confounding and identifies the effect of a state  unemployment relative to the national average in each year. The coefficients change in a noticeable manner from -0.451 to -1.409 suggesting that the without the years fixed-effects is clouding the data. 

##2 Take-Home- Teaching Evaluations========================================

teachevals = haven::read_dta("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/teaching_evals/teaching_evals.dta")


#2.1 Data Exploration---------------------------------------------------

#a) How many unique instructors and courses are in the data? What is the average number of observations (course-year pairs) per instructor? In a comment, note whether this looks like a short or long panel?

view(teachevals)

n_distinct(teachevals$InstrID) #There are 48 unique instructors in the data. 

n_distinct(teachevals$CourseID)#There are 254 unique courses in the data. 

teachevals %>% group_by(InstrID) %>% summarize(n = n()) %>%summarize(avg_obs = mean(n))

#The average number of observations is 17.5 per instructor. 

#this looks like a short panel because there are multiple observations per individual instructor. It would be considered a long panel if there were more instructors and did not teach more than one class. 

#b) Create a scatter plot of Eval (y-axis) against Apct (x-axis). Add a regression line. In a comment, describe the cross-sectional relationship between grading generosity and evaluations: is it positive or negative? Does this pattern surprise you? 

plot1 <- ggplot( teachevals, aes(x= Apct, y = Eval)) + geom_point() + geom_smooth(method = "lm") + theme_minimal() + labs(x = "Percent of Students recieving a A/A-", y = "Average Course Evaluation", title = "Average Course Evaluation per Percent of Students Recieving an A")

plot1
ggsave

#The scatter plot shows a concentration of the points between 3 and 5 evaluation point. There is a positive relationship the average course evaluation and the percent of students receiving an A or A-. The points are fairly scattered across the regression line so that suggests a weak relationship.

#2.2 Pooled OLS Baseline-------------------------------------

#a) estimate a pooled OLS model with all three regressors

m1 = lm(Eval ~ Apct + Enrollment + Required, data = teachevals)

modelsummary(m1)

#A one percentage point increase in the percentage of students receiving an A is associated with a 0.359 increase in evaluation scores. 

#b) explain why the OLS estimate of Apct might be biased. What unobserved characteristics of instructs could simultaneously drive both grading generosity and evaluation scores? Is the expected bias upward or downward? 

#The OLS estimator may be biased because of unobserved characteristics such as the race/gender of the instructor. If the instructor is in a socially acceptable body they could receive higher evaluations than an instructor who is not. Another possibility is if the professor is teaching a difficult course, a professor teaching an easier course is more likely to give higher grades and thus receive generous evaluations. A third example would be teaching styles, if a professor is more engaging they could receive higher evaluation regardless of the grade the student is given. 

#2.3 Fixed Effects models------------------------------------

#a) Estimate a model with instructor fixed effects, and a two-model adding year fixed effects: 

m_instr <- feols(Eval ~ Apct + Enrollment + Required | InstrID, data = teachevals) 

m_twfe <- feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = teachevals)

#b) Compare all three models (m1, m_instr, m_r
modelsummary(list("Pooled OLS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe), vcov = ~ InstrID, stars = TRUE, gof_map = c("r.squared", "nobs")) 

#c) 
#The coef on the fixed effect model is 0.306, meaning that when using fixed effects, a one unit increase in the percentage of A grades is associated with a 0.306 increase in course evaluation scored. In this model, we are holding the enrollment and whether the course is required constant. In the Two fixed effects model, we control for time variant, characteristics, comparing evaluation scores for the same instructor across different courses of years, giving a coef of 0.318. Both two-way FE and FE models, have smaller coefs than the OLS, suggesting that the OLS model was in fact upward biased.


#2.4 Random effects and the Hausman Test

#a) estimate a random effects model using plm

library(plm)

pdata <- pdata.frame(teachevals, index = c("InstrID", "CourseID", "Year"))
                     
m_re = plm(Eval ~ Apct + Enrollment + Required, 
           data = pdata, model = "random")
#b)
m_fe_plm = plm(Eval ~ Apct + Enrollment + Required,
               data = pdata, model = "within")
phtest(m_fe_plm, m_re)

#c) 
#The null hypothesis is that the random effects estimator is consistent. This means that the unobserved effects that are instructor-specific are not correlated with regressors. The pvale of the test is 0.178 suggesting we fail to reject the null hypothesis. This suggests that the random effects model could be appropriate.

                     
            