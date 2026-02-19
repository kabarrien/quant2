Karolina Barrientos
AQMSS 2 Spring 2026
Assignment 2 

##1 In Class Assignment QoG Dataset

#1.1 Setup and data preparation

library(dplyr)
library(reader)
library(tidyverse)
library(broom)
library(ggplot2)

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 2")
qog = read.csv("~/Applied quant 2/quant2/Assignment 2/qog_std_cs_jan26 (1).csv")
summary(qog)

df = qog %>%
  select(
    country = cname,
    epi = epi_epi,
    women_parl = wdi_wip,
    gov_eff = wbgi_gee,
    green_seats = cpds_lg
  )
qog%>%
summary(df)
head(df)
#1.2 exploratory visualization 
women_parl_epi<- ggplot(df, aes(x = women_parl, y = epi)) + geom_point() + geom_smooth() + geom_smooth(method = "lm")

women_parl_epi

#There is a positive relationship in where countries with more women in parliament tend to have higher EPI scores. This most likely indicates that both variables could be associated with development and governance quality. 

#1.3 Bivariate regression
bivar<- lm(epi ~ women_parl, data = df)
broom::tidy(bivar)

tidy(bivar)

#coef- 0.308

#interpretation: The coefficient 0.308 represents the predicted change in EPI score for each individual addition percentage point of women in parliament. 
# to get the predicted difference between the 25th and 75th percentile I multiply the coefficient by the IQR:
p25 = as.numeric(quantile(df$women_parl, 0.25, na.rm = TRUE))
p75 = as.numeric(quantile(df$women_parl, 0.75, na.rm = TRUE))

coef(bivar)["women_parl"] * (p75- p25)
#5.638584 is the difference between the 25th and 75th percentile. 

#1.4 multiple regression by adding the government effectiveness"

multiregress<- lm(epi ~ women_parl+ gov_eff, data = df)
tidy(multiregress)

#coef- women_parl- 0.0979 and gov_eff- 8.71

#Interpretation: After adding the the gov_eff the coefficient on women_parl decreases a by a large amount. This suggests that the bivariate association is impacted by government effectiveness and is correlated with women in parliament and environmental performance. 

#1.5 Demonstrating OVB

#pulling out the relevant coeffs
beta1_biva = tidy (bivar) %>% filter(term == "women_parl") %>% pull(estimate)
beta1_mult = tidy(multiregress) %>% filter(term == "women_parl") %>% pull(estimate)
beta2_mult = tidy(multiregress) %>% filter(term == "gov_eff") %>% pull(estimate)

#auxiliary regression 
aux = lm(gov_eff ~ women_parl, data = df)
delta = tidy(aux) %>% filter(term == "women_parl") %>% pull(estimate)

delta #0.02677 

#verify the OVB formula
#right hand side 
round(beta1_mult + beta2_mult * delta, 4)
#0.3307

round(beta1_biva,4)
#0.3078

#The values match the and confirm the OVB formula. The bias is positive because gov_eff is positively correlated with women_parl and with epi. So this inflated the bivariate estimate. 

#1.6 Robust standard errors

#classical SEs:
install.packages("modelsummary")
library(modelsummary)
modelsummary(multiregress, output =  "markdown")

#robust SEs:
modelsummary(multiregress, vcov= "robust", output = "markdown")


#SEs don't differ by much. 

#1.7 Presenting results 

modelsummary(list("Bivariate"= bivar, "Multiple"= multiregress), vcov = "robust", output = "markdown")

coefplot = modelplot(list("Bivariate" = bivar, "Multiple"= multiregress), vcoc = "robust")

ggsave("coefplot.png")

##2 Home Assignment: STAR Dataset

#2.1 Data prep
setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 2")


#a
STAR = read_csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/star/star.csv")
#b
STAR$classtype = factor(STAR$classtype, levels = c(1, 2, 3), labels = c("small", "regular", "regular+aide"))
#c
STAR$race = factor(STAR$race, levels = c(1, 2, 3, 4, 5, 6), labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other"))
#d
STAR$small <- ifelse(STAR$classtype== "small", 1, 0 )
#e
summary(STAR)

#g4math:
6325-3930
#greading:
6325-3972
# There are 6325 total observations in the dataset. There are 2395 non-missing observations in g4math and 2353 non-missing variables in g4reading.

#2.2 Comparing Groups

#a
STAR %>%
  group_by(classtype) %>%
  summarize(mean_value = mean(g4reading, na.rm = TRUE))

#Answer: The Small class has the highest mean score at 723, with regular at 720 and regular_aide at 721. 

#the means of small vs nonsmall grouping
STAR %>%
  group_by(small) %>%
  summarize(mean_value = mean(g4reading, na.rm = TRUE))
#b
m1<- lm(g4reading ~ small, data = STAR)
broom::tidy(m1)

#The coef is 3.10, representing the predicted change of reading test scores for each individual percentage point of a student in a small class would increase by 3.10 percentage points. 

#c 
#small-regular+aide
723-721 #2
#small-regular
723-720 #3
#small-notsmall 
723-720

#d 
#means of small and non small grouping's math test scores
STAR %>%
  group_by(small) %>%
  summarize(mean_value = mean(g4math, na.rm = TRUE))

m2<- lm(g4math ~ small, data = STAR)
broom::tidy(m2)
#coef 0.591
#The effect of a small class for math test scores is positive but much smaller when compared to reading scores. 


#2.3 Adding Controls 

#a 
m3<- lm(g4reading ~ small + race + yearssmall, data = STAR)
tidy(m3)
modelsummary(list(m1,m3))

#b comparing the coefficient on small with bivratiate model 
#coef in m3 = -4.00 and coef in m1 is 3.10. 
#When controlling for race and number of years in a small class, the coefficient changes drastically and depicts a negative relationship.  This most likely tells us that their was insufficient randomization of the treatment and control groups based on race and years in a small class.

#c Interpret the coeff on years small 
#Looking only at the number of years in a small class, we see a coefficient of 2.17, suggesting a positive relationship between yearssmall and reading test scores, meaning that when a student spends more time in a smaller class, their test scores should have a 2.170 increase per year.

#2.4 Interactions 

#a effect of being in small class differing by race

m4 <- lm(g4reading ~ small * race +yearssmall, data = STAR)

#b
broom:: tidy(m4)

#The interactions between small and race does differ by race, thus impacting the impact of small on test scores. 

#c 
#estimated effect of small class for white students

coef(m4)["small"]
#-5.3175

#The reference group, white students, is associated with 5.32 decrease on reading test scores in small classes. 

#estimated effect of small class for black students 
coef(m4)["small"] + coef(m4)["small:raceBlack"]
#1.656

#For Black students, the effect of small classes on reading test scores is about a 1.7 unit increase. 

#d 
#The interaction terms depicts variations on the effect of small classes on different races. However the values for most of the interaction coeffs are not statistically significant as they have large standard errors. This tells look at the results with caution as it provides little evidence of the effect of small classes on test scores. 

#2.5 

#a

modelsummary(list("Bivariate" = m1,"Multivariate" = m3, "Interaction" = m4), vcov = "robust", output = "reading_models.html", data = STAR)
             
#b

modelplot(list("Bivariate"= m1, "Multivariate" = m3, "Interaction"= m4), vcov = "robust")

library(tidyverse)
ggsave("reading_models.png", scale = 2)

#2.6
##a) What does the STAR data suggest about the effect of small class sizes on student achievement?
##b) Why is this evidence more credible than a typical observational study of class size?
##c) Are there any limitations or caveats based on what you observed in the data?

#The STAR data offers mixed evidence in support of smaller classes to boost reading test scores. On it's own, in a bivariate regression, the variable of small class size shows a 3 point increase in test scores however, when controlling for other variables the coefficients change and offer less significant impacts of small classes. For instance, when controlling for race and the years in small classes, the coefficients show a smaller positive relationship or in other cases a negative relationship. This suggests that the bivariate regression was capturing omitted variables like the racial makeup of the classes and shows that the number of years in a small class is more relevant. 
#This evidence is more credible because the research is a randomized experience, limiting selection bias and is able to provide statistical evidence for the conclusions. While observational studies do not provide the same level of analysis. 
#From what is observed the limitations of the data include, not randomizing the race of students or the number of years each student is in a small class. There is also the main caveat that much of the data is statistically insignificant, with large standard errors.

##The entire code won't post on github- making this edit to try to push and commit again
