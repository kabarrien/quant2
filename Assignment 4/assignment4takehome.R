#Karolina Barrientos
#Assignment 4- Homework

#2 Wealth and Infant Mortality 

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 4")

library(dplyr)
library(reader)
library(tidyverse)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(readstata13)

#2.1 Data exploration 

#a)load dataset and print summary stats 
raw = haven::read_dta("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/other/infantmortality.dta")

view(raw)

summary(raw)

nrow(raw)

#101 countries are in the data

#b) Create a histogram of infant and a histogram of income 

hist(raw$infant)

hist(raw$income)

#Both histagrams are righ-skwed. 

#c) create a scatter plot of inant (yaxis) agianst (xaxis), coloring points by region

plot1 = ggplot(raw, aes(x = infant, y = income, col = region)) +
  geom_point() +
  labs(x = "Infant Mortality (per 1,000)", y = "Income Per Capita ($)", title = "Infant Mortality & Income")

plot1

#The points on the plot are skewed to the right similar to the histograms.

#d) create the same scatter plot by using log(income) and the x-axis and log(infant)

plot2 = ggplot(raw, aes(x = log(infant), y = log(income), col = region)) + geom_point() + labs(x = "Infant Mortality(per 1,000)", y = "Income Per Capita ($)", title = "Infant Mortality & Income")

plot2 

#Plot 2 is displays a more linear relationship between Infant Mortality and Income Per Capita. 

#2.2 Comparing specifications 

#a)  Estimate a level-level model

m1 = lm(infant ~ income, data = raw)

#b) estimate a log-log model

m2 = lm(log(infant) ~ log(income), data = raw)

#c) interpret the coefficients on income in each model

summary(m1)

#$1,000 increase in income per capita is associated with a DECREASE of about 20 deaths per 1,000 births.
#A 10% increase in income is associated with a 5.118% DECREASE in infant mortality.


summary(m2)



#d) create a residual vs fitted values plot for both models

broom::augment(m1)

broom::augment(m2)

plot(m1, which=1)

plot(m2, which=1)

#The log-log model displays a better residual pattern.

#2.3 Multiple regressions with controls 

#a) estimate a log-log model with controls for region and oil-exporting status

m3 = lm(log(infant) ~ log(income) + region + oil, data = raw)


#b) print the results and comment on coefficient on log(income)

m3

#c) interpret the coeff on the Africa region indicator


#When controlling for income the infant mortality rate the coeff for African countries is negative while it is postive for other countries.

#d) computer average marginal effects 

marginaleffects:: avg_slopes(m3)

#2.4 Interaction: Oil status and Income 

#a)

m4 = lm(log(infant) ~ log(income) * oil + region, data = raw)

m4

#b) 

marginaleffects::avg_slopes(m4, variables = "income", by="oil")

#c) comment

#The relationship between income and infant mortanlity does in fact change between oil and non-oil counrties.
#In non-oil countries, there is about 2.1 LESS deaths per 1000 births associated with $1000 increases in come. 
#In oil countries, a $1000 increase is associated with 1.1 more deaths per 1000 births.

#d) 
m4plot = marginaleffects::plot_slopes(m4, variables = "income", condition = "oil")

m4plot

ggsave("m4plot.png")

#2.5 Predicted values for specfic scenarios 

#a-c)

marginaleffects::predictions(m3,
                             newdata = marginaleffects::datagrid(
                               income = c(1000, 20000, 10000),
                               region = c("Africa", "Europe", "Americas"),
                               oil = c("no", "no", "yes")))

#d comment, discuss the predicted values, are they plausible? How large is the gap between the African and European scenarios? 

#Non-oil African, income =1000 --> 66.99 infant deaths per 1000 births
#non oil Euro income 20000 --> 8.61 deaths per 1000 deaths
#oil exporting American 10000 ---> 33.53 deaths per 10000

#Predicted vales represents a gap of about 58 deaths per 1000 births, they are plausible and reflect income and regional disparities. Wealth and geography could explain the large gaps between African and European countries. 

#2.6 Publication quality visualization 

#a) 

plot4 = marginaleffects::plot_predictions(m3, condition = c("income", "region")) +
  labs(x="Income", y="Mortality", title="Predicting Infant Mortality with Income")+
  theme_minimal()

plot4

#had to fix some error related to my plots
dev.off()

ggsave("plot4.png")

#b)

#The plot shows a strong negative relationship across all regions. Essentially as income increases, predicted infant mortality declines. This is especially seen at lower income levels.
#However across regions, the slopes differ, for example, in African countries are predicted to have higher infant mortality than other countries, especially in Europe.
#This suggests that income is not the only factor that must be taken into account, geography and regional characteristics also play a role. 

#A main limitation is that we cannot make a strong causal claims due to the fact it is an observational and cross-sectional analysis. 
%
#2.7 Diagnostics and robust inference 

#a) Create a residuals vs fitted values for plot 3

m3_aug = broom::augment(m3)

ggplot(m3_aug, aes(x= .fitted, y= .resid)) + geom_point() + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x= "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log(m2)")

plot(m3, which=1)

#There appears to be 3 clusters with a few outliers. 

#b) 
modelsummary( list("Level" = m1, "Log-Log" = m2,"Controls" = m3, "Interaction" = m4),
  vcov = "robust",stars = TRUE, gof_map = c("r.squared", "nobs"))

#c)

modelsummary(m3)
modelsummary(m3, vcov = "robust")

#Comparing the standard errors of both the orginal and robust standard errors we see that the estimates remain the same. However, the robust standard errors are larger, thus reducing the statistical signficant for some variables. 
#Robust standard errors are  preferred because of the assumption that homoskedasticity is unlikely to hold.