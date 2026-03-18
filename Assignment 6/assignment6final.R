#Karolina Barrientos
#Assignment #6 Panel Data II 


# In class (Card- Kruegeer Minimum Wage)

install.packages("did")
install.packages("fixest")


#1.1 Data Set Up and exploration 


setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 6")

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(fixest)
library(modelsummary)
library(did)

df = read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/other/minwage.csv")

View(df)
summary(df)

#a) create an NJ dummy variable and summarize wages by state

df = df %>% mutate(NJ = ifelse(location == "PA", 0, 1))

table(df$NJ)

#There are 291 restaurants in Jersey and 67 in PA. 

#mean min. wage in PA and NJ Before and after
df %>% 
  group_by(NJ) %>%
  summarise( mean_wage_before = mean(wageBefore, na.rm = TRUE), mean_wage_after = mean(wageAfter, na.rm= TRUE))

#Before the policy change, average starting wages in NJ and PA were nearly identical (both close to the federal minimum of $4.25.
#After the policy was set in place,NJ minimum wage rose to $5.05 while that PA remained the same.

#b) Compute the simple DiD estimate manually 

#Create a new object that is only the means of the NJ and PA

means = df %>% 
  group_by(NJ) %>%
  summarise(before = mean(fullBefore, na.rm=TRUE), after = mean(fullAfter, na.rm = TRUE), change = after - before)

means 

#use means to do differences

nj_change = means$change[means$NJ == 1]

pa_change = means$change[means$NJ == 0]

did_est = nj_change - pa_change #2.97245

did_est

cat("DiD Estimate:", round(did_est, 3), "\n") #<- used to give a sort of label to the output and round it the nearest third digit
#2.927

#The DiD estimate is the difference in with-in group changes. A positive value means full-time employment grew more (or fell less) in NJ than in PA after the min wage increase, which contradicts the standard prediction that higher min wage reduces employment. 

#c) run regressions, shape the data to long format (one row per restaurant per period)

df_long = df %>%
  mutate(id = row_number()) %>%
  pivot_longer( cols= c(fullBefore, fullAfter), names_to = "period", values_to = "full_emp") %>%
  mutate ( post = ifelse(period == "fullAfter", 1, 0), NJ = ifelse(location != "PA", 1,0))

df_long

nrow(df_long)
nrow(df)
358 * 2

#The long format is needed because we need one row per observation, before and after the treatement, in this case the policy implementation for each resistant location. 

#1.2 DiD Regression 

#a) estimate the DiD regression 

m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)

modelsummary(m_did, stars = TRUE, gof_map = c("nobs", "r-squared"), output = "markdown")

#The coefficient on post*NJ matches the Did estimator as they both reflect a coeff of 2.97. 

#b) adding chain fixed effects

m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)

modelsummary(list( "DiD"= m_did, "DiD + chain FE"= m_did_fe), stars = TRUE, gof_map = c("nobs", "r.squared"), output = "markdown")             

#the DiD estimate does not change when chain fixed effects are added. Chain FEs absorb baese line differences in staffing levels across fast-food chains (ex: wendys may have structurally different employment levels than KFC) but since chain type is roughly balanced across states, controlling for it has little impact on the DiD coefficient. 

#c) The parallel trend assumption requires that the NJ minimum wage increase,employment trends in NJ and PA fast-food restaurants would have been the same from Feb- Nov 1992. This makes sense because PA and NJ share a similar economic environment and with the surveys administered at similar times, any divergence is limited. A violation of this assumption would be if PA experienced an independent economic shock such as the closing of a major employer closed a factory between the 2 survey waves, this would change NJ employment independent from min wage. 

#1.3 Wages as a validation check 

#a) Did for wages (first-stage check)

df_long_wage = df %>% 
  mutate(id = row_number()) %>% 
  pivot_longer( cols = c(wageBefore, wageAfter), names_to = "period", values_to = "wage") %>% 
  mutate ( post = ifelse(period == "wageAfter", 1, 0), NJ = ifelse(location == "PA", 1, 0))

m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)

modelsummary(m_wage, stars = TRUE, gof_map = c("nobs", "r.squared"), output = "markdown")

#The interaction coefficient post:NJ is positive and statistically significant: wages rose substantially in NJ relative to PA after the policy change, and the magnitude is consistent with the $0.80 min wage increase ($5.05=$4.25) This the sign and magnitude we would expect if the law was binding. 

#b) comment on why the wage result is important for interpreting the employment DiD. 

#Wage result is important for interpreting the employment DiD because it in our case, serces as a "first stage" or manipulation check. If wages had not risen in NJ after the min wage increase, it would be unclear whether the study is truly estimating the effect of a min wage change at all- the law might not have been vinding or the firsm had already paid folks above the wage. The fact that the wages did rise in NJ gives us confience that the treatment actually occured as intended, so the employmement DiD can be credibly interpreted as a casual response to the min wage increase rather than a spurious or null comparison. 

#2. Take Home- Staggered DiD==========================

#2.1- Data Cleaning
#a)

library(did)
data(mpdta)

length(unique(mpdta$countyreal))

#There are 500 counties in this dataset. 

table(mpdta$first.treat)

#There are 5 treatment cohorts in this dataset.

#0 (before treatment--> 1545), 2004 (100 countries adapted treatment), 2006 (200 countries adapted treatment), 2007 (655 countries adapted treatment)

#in a staggered treatment adoption in this case means that we have different waves of treatment that were adopted a different times. 
#for example in 2004, during the first wave of treatment, 100 countries received the treatment. It is problematic to simply compare treated vs untreated counties because some counties were exposed to the treatment earlier than other counties, meaning they were exposed longer by 2007. 

#b) plot average log teen employment (lemp) over years, separately for each treatment cohort.  

mpdta_avg = mpdta %>% 
  mutate(cohort = factor(first.treat, levels = c(0, 2004, 2006, 2007), labels = c("Never Treated", "Adopted 2004", "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))

avglempplot = ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) + geom_line()+ 
  geom_point() + theme_minimal()+ labs(x = "Year", y= "Log teen employment", color = "Treatment cohort")

avglempplot

ggsave("avglemp.png")

#In the graph we can see that 2006 and 2007 cohorts have similar patterns for teen employment in where this is a slight increase but then it flattens slightly. For 2004, the pattern is decreasing till the group gets to 2006 where there is an increase. The only pre-trends that look a bit problematic is that of the 2007 cohort as it the time period 2003-2004 looks different from that of the other two cohorts. 

#2.2 Naive TWFE vs Callaway-Santanna estimator

#a) estimate a naive TWFE model treating all treated countries as a single group.

mpdta = mpdta %>%
  mutate(treated_post = ifelse(first.treat > 0 & year >= first.treat, 1, 0))

naive_twfe = feols(lemp ~ treated_post|countyreal+year, data = mpdta)

naive_twfe #-0.0365

#The coefficient on treated_post is -0.036549 represents a decline of about 3.65% on employment and is statistically significant. 
#The implicit assumption being made here is that the treatment effect is identical across all cohorts of treatment (the 2004 cohort reacts the same as the 2007 group), and are constant over time (the effect in the 1st year of treatment is the same as the effect in the last year). 
#This is a naive assumption because we know that the treatment is staggered, meaning it is being administered in a different time periods where the context of the cohort is different. For example in this case, the economic landscape in 2004 could be different than the economic context in 2007. The problem here is that this model compares newer treated units to units that were treated years ago.

#b) Use the Callaway-Santanna (2021) estimator (estimates group-time average treatment effects separately for each cohort and time period), report the overall ATT estimate.
#Is it similar to or different from the naive TWFE estimate? 

out <- att_gt(yname = "lemp", gname = "first.treat", idname = "countyreal", tname = "year", xformla = ~1, data = mpdta, est_method = "reg")

summary(out)

#for overall ATT estimate:

overall_att<- aggte(out, type = "simple")
summary(overall_att) #-0.04

#OR??

group_effects <- aggte(out, type = "group")
summary(group_effects)
#-0.031 

#The overall ATT is -0.04 OR -0.031. This math is averaging the ATT for all groups in their different post-treatment periods and is weighing the effects by the size of each group. It avoids bias by not using already treated unites as controls for later-treated units. It is only comparing treated units to the "Never Treated" group. 
#The  TWFE from the earlier steps is about the same as the overall ATT we got a coef of -0.0365. 

#c) Event-Study version 

#using event study in DiD package. 

es <- aggte(out, type = "dynamic")

summary(es)

#to make the event study plot 

esplot <- ggdid(es)

esplot

ggsave("esplot.png")

# Are the pre-treatment estimates (leads, i.e.periods before treatment) statistically distinguishable from zero? What does this tell us about the parallel trends assumption? What do the post-treatment estimates (lags)show about the dynamic effects of treatment?

#The plot shows that COMMENT ON SOMETHING ELSEEEE 

#2.3 Pre-Testing the parallel trends assumption

#a) We run CS but atdd bstrap and cband to end to control for further uncertainty??? 

out_bstrap <- att_gt(yname = "lemp", gname = "first.treat", idname = "countyreal", tname = "year", xformla = ~1, data = mpdta, est_method = "reg", bstrap = TRUE, cband = TRUE)   # Enable uniform confidence bands and bstrap

summary(out_bstrap)

#This test evaluates the parallel trends assumption, before treatment occurs. The Null Hypo: All pre-treatment ATT values are equal to zero. This tells that before any policy was set in place, the treated and control groups were progressing the same way. However the large p-value means we FAIL to reject the null hypothesis. 

#b) 
plotbstrap <- ggdid(out_bstrap)
plotbstrap

ggsave("plobstrap.png")

#In this visualization we see that the pre-treatment ATT estimates are very close to 0 and are statistically indistinguishable from zero across all cohorts. We know this because the uniform confidence bands for the years overlap with the horizontal zero line. This supports the parallel trends assumption-meaning there were no significant "placebo" effects or divergent trends. 

#c) In a comment (2–3 sentences), reflect on the limitations of pre-testing. Even if we cannot reject parallel trends in the pre-period, can we be certain the assumption holds during the post-treatment period? What is the pre-test actually telling us, and what is it not telling us?

#Pretesting allows us to make the assumption that are research-design should work by showing that in the control and treated groups have moved in the same direction in the past are in fact comparable groups. However, the pretesting can not confirm or give the ability to assume that in the future the two groups will move together. By not rejecting the null hypothesis does not automatically mean that we accept the hypothesis. The pretest allows to increase our confidence in the model.

#2.4 Comparing control group specifications 

#a) Re-estimate the CS model using not yet treated counties as the control group. Report the overall ATT. 

out_nyt <- att_gt(yname = "lemp", gname = "first.treat",idname = "countyreal", tname = "year", xformla = ~1, data = mpdta,
  est_method = "reg",
  control_group = "notyettreated")

summary(out_nyt)

overallatt_cs=aggte(out_nyt,type = "simple")

overallatt_cs #-0.0398

#The estimate of -0.0398 is about the same as the estimate from the 2.2b. They have same sign and magnitude. 

#b) Produce and save an event-study plot for this specification.

es_nyt <- aggte(out_nyt, type = "dynamic")

summary(es_nyt)

esnytplot <- ggdid(es_nyt)

ggsave("esnytplot.png")

#The trends in both plots, the event study using not-yet treated as the control and the event study using never treated show similar patterns. I don't believe using the broader control group changes the conclusions. 


#c) Comment Discuss the trade-off between the two control group choices. Under what conditions would you prefer never treated as the control

#In this part we are considering two cases, one where we use not yet treated groups as the control and the other where we are considering the specification of an events study.By considering the not yet treated group as a control we are maximizing the sample size but is riskier as it does not assume anticipation effects, in where units can change behavior before their actual treatment date. Never treated as the control is "cleaner and has no risk of treatment contamination at all. We would prefer to use never treated as the control when we have a large enough sample size. 

#2.5 Discussion: Why does TWFE fail in staggered settings? 

#a) In a comment (3–5 sentences), explain intuitively why the naive TWFE estimator can produce misleading results in staggered DiD settings. What is the “forbidden comparison” problem? Which units get used as the control group in a way that is problematic, and why is that a problem if treatment effects are heterogeneous across cohorts or over time?

#The native TWFE estimator can produce miss leading results in staggered settings because it uses a weighted average in the calculations for overall treatment effect for all possible 2X2 DiD combinations. This is a forbidden comparison because units treated later in the sample are used as the control group for those treated later.If treatment effects are heterogeneous across cohorts, meaning they change over time or vary by cohort, the early-treated units are no longer a stable baseline as they ar still experiencing their own evolving treatment effect. This can result in a biased estimate during the regression.


#b) Compare the TWFE estimate from question 2.2a to the Callaway-Santanna estimate from question 2.2b. Are they similar or different? In a comment, based on the eventstudy pre-trends from question 2.2c, which estimate do you find more credible and why?

#The TWFE estimate from 2.2a is -0.0365 and the estimate from the Callaway-Santanna aggregate estimate was -0.04. These two estimates are fairly close and differ only slightly but they were found using different mathematical methods, where the CS aggregate estimate has less noise as it uses the never treated group as the control. 




