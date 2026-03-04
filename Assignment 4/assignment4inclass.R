#Karolina Barrientos
#AQMSS II 

#In Class Assignment 

setwd(("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 4")

library(dplyr)
library(reader)
library(tidyverse)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)

library(readstata13)

#1.1  Setup and data exploration

#a)

raw= haven:: read_dta("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/other/corruption.dta")

view(raw)

#b)
raw = raw%>%
  drop_na(ti_cpi)

raw= raw%>%
  drop_na(undp_gdp)

nrow(raw)

#170 countries remain 

#c) 
summary(raw$ti_cpi)
sd(raw$ti_cpi)

summary(raw$undp_gdp)
sd(raw$undp_gdp)

#right-skewness: mean > 

#1.2
#a)
plot1 <- ggplot(raw, aes(x = undp_gdp, y = ti_cpi)) + geom_point() + geom_smooth(method = "lm") + labs ( x = "GDP per capita (ppp)", y = "Corruption Perceptions Index")

plot1
#b)
#the data is concentrated at the bottom left of the plot
#c)
plot2 <- ggplot(raw, aes (x = log(undp_gdp), y = ti_cpi)) + geom_point() + geom_smooth(method = "lm") + labs ( x = "log (GDP per capita)", y = "Corruption Perceptions Index") + theme_bw()

plot2
# now during the log the data points are less skewed and more linear

#1.3 Bivariate regression

#a)
m1 = lm(ti_cpi ~ undp_gdp, data = raw)
#b)
summary(m1) 

coef(m1)["undp_gdp"] * 1000  #0.1729782
#c)
q25 = quantile(raw$undp_gdp, 0.25) #1974.25

q75 = quantile(raw$undp_gdp, 0.75)

c(q25, q75)

predictions(m1, newdata = datagrid(undp_gdp = c(q25, q75)))

predictions(m1, newdata = datagrid(undp_gdp = 0))

#the predicted values are...

#4 non-linear specfications 

#a)
m2 = lm(ti_cpi ~ log(undp_gdp), data = raw)

#b) 
plot_predictions(m2, condition = "undp_gdp")

#c) 
m3 = lm(ti_cpi ~ undp_gdp + I(undp_gdp ^2), data = raw)

tidy(m3)

plot_predictions(m3, condition = "undp_gdp")

#d)
r2 = c( "Level-Level" = summary(m1)$r.squared, 
        "Level-Log" = summary(m2)$r.squared,
        "Quadratic" = summary(m3)$r.squared)
r2

#The quadratic model best fits the data. A non-linear specfication is more appripraite for this relationship because.....


#1.5 Marginal Effects 

#a) logmodel AME 

avg_slopes(m2, variables = "undp_gdp")

#b) explain why the AME differs from the raw coefficent on log(undp_gdp)

#

#c) quadratic
slopes(m3, variables = "undp_gdp", newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))
##

#1.6 Prediction plots

#a) create a prediction plot for the log model:

predplotm2 = plot_predictions(m2, condition = "undp_gdp")

predplotm2

ggsave("predplot2.png", p2, width = 6)

predplotm3 = plot_predictions(m3, condition = "undp_gdp")

ggsave("predplotm3.png", p3, width = 6)


#1.7 Residual diagnostics

#a) get residuals and fitted values from the level-level model

m1_augment = broom::augment(m1)

ggplot(m1_augment, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(x = "Fitted values", y = "Residuals", title = "Residiuals vs Fitted Level-Level (m1)")

#The residual plot shows a clear curved pattern, indicating that the linear specification misses the non-linear relationship. The spread of residuals also appears to increase with fitted values, suggesting heterskedasticity. 

#b) Residuals vs fitted for the log model

m2_aug = broom::augment(m2)

ggplot(m2_aug, aes(x= .fitted, y= .resid)) + geom_point() + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x= "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log(m2)")
#The log transformation substantially improves the residual pattern. The curvature is reduced, though some heterskedasticity may remain.

#c) Cook's distance for influential observations:

n = nrow(raw)
threshold = 4/n

cooks_d = cooks.distance(m2)

influential = which(cooks_d > threshold)
raw$cname[influential]

plot(m2, which = 4)

#d) Influential observations should not be removed automatically. They may represent genuine cases 
#( very wealthy or very corrupt countries) rather than data errors. 
#A recommended robustness check would be to re-estimate the model excluding these observations and compare the coefficients. If the results are similar, the original estimates are robust.

#1.8 publication-quality table 

#a) regression table comparing all three models: 

finallist = modelsummary(list ("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3), vcov = "robust", stars = TRUE, gof_map = c("r.squared", "nobs"), output = "markdown")

ggsave("finallist.png", width = 6)
       
#b)
#The quadratic model (m3) is the preferred specification. It has the highest R^2, produces the best residual diagnostics, and its the functional form has clear substantive interpretation: the relationship between wealth and corruption is one of diminishing returns. 
  