Karolina Barrientos
AQMSS - Assignment 3

#1 In Class Assignment 

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
In this dataset, the LPM and logit lead to similar conclusions. Essentially that age, education, and income are all postively associated with turnout, and gender has a modest or negligible effect. The differences between lpm and logit matter more when predicted probabilites are close to the boundary (0 or 1). In this simple, turnout is relatively common, so the linnear approximation works reasonably well. 

#2 Part 2: Take-Home Exercises(STAR- High School Graduation)

#2.1 data prep
#a) loading star.csv and creating factor variables
star = read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/star/star.csv")

star$classtype = factor(star$classtype, levels = c(1, 2, 3), labels = c("small", "regular", "regular+aide"))

star$race = factor(star$race, levels = c(1, 2, 3, 4, 5, 6), labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))

#b)creating binary variable for small

star$small <- ifelse(star$classtype== "small", 1, 0 )

summary(star)
view(star)

#c) droping missing values on hsgrad

star1 = star %>%
  drop_na(hsgrad)

view(star1)

#3047 variables remain in the data set after removing observations with missing values on hsgrad.

#d computing high school graduation reate overall and by class type

hsgradratclass = star1 %>%
  group_by(classtype)%>%
  summarise(turnout = mean(hsgrad))

#2.2 LPM and logit

#a) estimate an lpm predicting hsgrad from small:

lpm1 = lm(hsgrad ~ small, data = star1)

modelsummary(lpm1)

#b) estimate a logit model with the same predictor: 

logit1 = glm(hsgrad ~ small, family = binomial, data = star1)

modelsummary(logit1)

#c) SKIPPPP

#d) AME from logit1

avg_slopes(logit1)

#2.3 Adding controls

#estimate both LPM and logit with controls:

lpm2 = lm(hsgrad ~ small + race + yearssmall, data = star1)

logit2 = glm(hsgrad ~ small + race + yearssmall), family = binomial, data = star1)
