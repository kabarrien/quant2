#Karolina Barrientos
#AQMSS - Assignment 3

#1 In Class Assignment ----------------------------------------------------------------------------------------------

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 3")


library(dplyr)
library(reader)
library(tidyverse)
library(broom)
library(ggplot2)
library(modelsummary)


library(marginaleffects)

raw  = read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")


df = raw%>%
  mutate(
    voted = ifelse(V202109x < 0, NA, V202109x), 
    age = ifelse(V201507x < 0, NA, V201507x),
    female = case_when(V201600 == 2 ~ 1, V201600 == 1 ~ 0, TRUE ~ NA_real_),
education = case_when(
  V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14, V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_),
income = ifelse(V201617x< 0, NA, V201617x),
party_id = ifelse(V201231x < 0, NA, V201231x))


#b)
summary (df)
nrow(df) #8280

#c)
mean(df$voted, na.rm = TRUE)
summary(df)

#2. Exploratory visulatization
#a) 
turnout_by_edu = df %>%
  group_by(education) %>%
  summarise(turnout = mean(voted, na.rm = TRUE))

turnout_by_edu

ggplot(turnout_by_edu, aes(x = factor(education), y = turnout)) +
  geom_col() +
  labs(x = "Years of education", y = "Turnout rate")

#3 Linear probability model

#a-b)
lpm = lm(voted~age+education+income+female,data=df)
tidy(lpm)

#c) Tbe coefficient on education represnts the esitmated change the in the probablity of voting for each additional year or education, holding the other variables constant. 

#d) check predicated probablities
preds_lpm = predict(lpm)

sum(preds_lpm < 0)

sum(preds_lpm > 1)

range(preds_lpm)

#4 Logistic regression

#a-b 
logit = glm(voted ~ age + education + income + female, family = binomial, data = df)
tidy(logit)

#c odds ratio
exp(coef(logit))
#The odds ratio for education indicated the multiplicative change in the odds of voting for each additional year of education. An odds ratio above 1 means more education is associated with high odds of voting. 

#d)
preds_logit = predict(logit, type = "response")
range(preds_logit)

#5 Comparing LPM and logit

#a)

library(marginaleffects)

avg_slopes(logit)

#b) The AMEs from the logit model are similar to the LPM coeficcients. We expected this since predicted probabilities are mostly in a moderate range. Both appraiced tell a similar story about the relationship between each predictor and voter turnout. 

#c) 

library(modelsummary)

modelsummary(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL), output = "markdown")

#Predicted probabilities

#a) Predicted probablity across education:

p1 = plot_predictions(logit, condition = "education")
p1

#b) 

p2 = plot_predictions(logit, condition = c("age", "female"))
p2

ggsave("pred_prob_age_gender.png", p2, width = 6, height = 4)

#7) Coefficient Plot

p3 = modelplot(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL))

p3

ggsave("coefplot_lpm_logit.png", p3, width = 6, height = 4)

#c)
#In this dataset, the LPM and logit lead to similar conclusions. Essentially that age, education, and income are all positively associated with turnout, and gender has a modest or negligible effect. 
#The differences between lpm and logit matter more when predicted probabilites are close to the boundary (0 or 1). In this simple, turnout is relatively common, so the linnear approximation works reasonably well. 

#2 Part 2: Take-Home Exercises(STAR- High School Graduation)----------------------------------------------------------------------------------

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 3")

#2.1 data prep
#a) loading star.csv and creating factor variables
star = read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/star/star.csv")

star$classtype = factor(star$classtype, levels = c(1, 2, 3), labels = c("small", "regular", "regular+aide"))

star$race = factor(star$race, levels = c(1, 2, 3, 4, 5, 6), labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))

#b)creating binary variable for small

star$small <- ifelse(star$classtype== "small", 1, 0 )

summary(star)
view(star)

#c) dropping missing values on hsgrad

star1 = star %>%
  drop_na(hsgrad)

summary(star1)

#3047 variables remain in the data set after removing observations with missing values on hsgrad.

#d computing high school graduation rate overall and by class type

hsgradratclass = star1 %>%
  group_by(classtype)%>%
  summarise(turnout = mean(hsgrad))

#The graduation rates are similar, for small its 0.835, regular at 0.825 and regular + aide at 0.839.

#2.2 LPM and logit

#a) estimate an lpm predicting hsgrad from small:

lpm1 = lm(hsgrad ~ small, data = star1)

modelsummary(lpm1)
#Students in small classes have a 0.004 higher probability than non-small classes. 


#b) estimate a logit model with the same predictor: 

logit1 = glm(hsgrad ~ small, family = binomial, data = star1)

modelsummary(logit1) #small- 0.0271 intercept- 1.60

coef(lpm1) -> #small = 0.00375

#c) The LPM coefficient on small is 0.00375, meaning students that are in small class re estimated to be about 0.4 percentage points more likely to gradaute than students in non-small classes. A very small percentage. 

#d) AME from logit1

avg_slopes(logit1) 

#The average marginal effects of small from the logit model is very similar to the LPM coefficients. 

#2.3 Adding controls

#estimate both LPM and logit with controls:

#a)

lpm2 = lm(hsgrad ~ small + race + yearssmall, data = star1)

coef(lpm2) #small -0.0756


logit2 = glm(hsgrad ~ small + race + yearssmall, family = binomial, data = star1)

coef(logit2)

#b) compare the coefficient on small between the bivariate and controlled models

coef(lpm1) #small= 0.00375
coef(lpm2) #small= - 0.0756

#when taking controls into account the coeff for small changes dramatically, from a postive insignificant effect to a negative and significant effect. It is possible that this change happened because of the inclusion of yearssmall, which entails the treatment intensity by looking at how long students were exposed to small classes.
#For randomization, this could mean that it worked for the initial assignment but not for the intensity of exposure. Resulting in different coeffs. 


#c) interpret the coefficient on yearsmall from the logit mode.

library(marginaleffects)

avg_slopes(logit2, variables = "yearssmall")

#estimate = 0.0283

#The coeff is 0.0283 for on yearssmall. This means that for each additional year a student is in a small class, the probability of them graduating high school increases by 2.83 percent. 
#2.4

#a) using controlled logit model, prediction graduation probabilities

pred <- predictions(logit2, data.frame(small = c(1,0), yearssmall = c(3,0), race = factor(c("White", "Black"), levels = levels(logit2$model$race))))
coef(pred)
summary(pred)

#Student 1(White student) coef:  0.869 At 95% CI: 0.8452 - 0.8896. 
#Student 2 (Black student) coef:0.729, At 95% CI: 0.6947 - 0.7615.


#b) plot predicted graduation probabilities

#need to fix "Hispanic variable"

star1 <-star1 %>% mutate(race= factor(race))
race_levels<- levels(star1$race)
table(star1$race)
race_levels <- levels(logit2$model$race)


logitplot<- plot_predictions(logit2, condition = c("yearssmall", "small"))

logitplot
ggsave("logitplot.png")

#2.5 Interactions

#a) Does the small class effect on graduation differ by race?
logit3 = glm(hsgrad ~ small * race + yearssmall, family = binomial, data = star1)

logit3

#The small class effect differ slightly by race for other students and Black students. However the data is a bit sparse when it comes to Asian and native American students. 

#b) compute marginal effect of small separately for each racial group.
avg_slopes(logit3, variable = "small", by = "race")

#c) The small class effect does in fact vary by racial groups. The effect is the strongest for black students, moderate for white students, and insignficant for Native Students, and inconclusive for Asian students.

#2.6 presenting the results and discussions 

#a)
create a table with modelsummary()

table<- modelsummary(list("LPM bivariate" = lpm1, "LPM controlled" = lpm2, "Logit bivariate" = logit1, "Logit controlled" = logit2), vcov = list("robust", "robust", NULL, NULL))

table

ggsave("table.png")

#b) coefficient plot with modelplot()

modelplot <- modelplot(list(lpm1, lpm2, logit1, logit2))

modelplot
ggsave("modelplot.png")

#c) discuss the following questions:
# • What does the STAR data suggest about the effect of small class sizes on high
# school graduation?
# • How do the LPM and logit results compare? Do they tell a similar or different story?
# • Why is this experimental evidence more credible than an observational study?

#The STAR data suggests that are small differences in graduation probabilities across small, regular and regular + aid classes based on raw averages. 
#However the overall descriptive graduation rate was about 83%. There was minimal difference between students in differing classes on their graduation rates. 
#Small classes had a rate of about 83.6%, regular class at 82.5% and the regular+aide class at 83.9%.
#The bivariate LPM shows a very small positive effect of being an small class, approximately, 0.0038, though it is not statistically significant. 
#The logit model mirrors this result, as the AME was around 0.4 percentage point. Meaning that when taking the treatment indicator into account, the LMM and logit models are similar 
#When controls are taken into account, the results change, showing a negative coefficient on small class assignments as shown in the controlled LPM (-0.0756). 
#However, the yearssmall shows a positive effect in the logit model. This suggests that each additional year in small class increases the probability of graduating by about 2.8 percentage points. 
#The design this experiment, using random assignment to class types has a greater credibility to an observational study because it reduces confounding bias. 
#Although researchers should have taken into account randomization for the number of years a student is in a small calss. 

