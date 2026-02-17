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

#The values match the and confirm the OVB formula. The bias is postive because gov_eff is positively correlated with women_parl and with epi. So this inflated the bivariate estimate. 

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

STAR = read_csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/star/star.csv")
