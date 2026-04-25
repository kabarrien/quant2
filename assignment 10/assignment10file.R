#Karolina Barrientos
#Assignment 10

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 10")

library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(tidyverse)
library(broom)

teachevals = haven::read_dta("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/teaching_evals/teaching_evals.dta")

#simple regression depicting relationship between Eval and Apct

lm1 = lm(Eval ~ Apct, data = teachevals)

summary(lm1)

#scatter plot of Evaluation (y-axis) against Apct (x-axis)

plot1 <- ggplot( teachevals, aes(x= Apct, y = Eval)) + geom_point() + geom_smooth(method = "lm") + theme_minimal() + labs(x = "Percent of Students recieving a A/A-", y = "Average Course Evaluation", title = "Average Course Evaluation per Percent of Students Recieving an A")

ggsave("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 10/plot1.pdf", plot = plot1, width = 8, height = 6)


#multiple linear regression, adding Enrollment and Required as predictors

lm2 = lm(Eval ~ Apct + Enrollment + Required, data = teachevals)

#model with fixed effects 

m_instr <- feols(Eval ~ Apct + Enrollment + Required | InstrID, data = teachevals)

summary(lm2)

modelsummary(lm2)

#coef plot

plot2 = tidy(m_instr, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(x = "Predictor", y = "Coefficient Estimate", 
       title = "Regression Coefficients with 95% CI")
plot2

ggsave("plot2.pdf")

list = modelsummary(list ("Simple Regression" = lm1, "Pooled OLS" = lm2, "Instructor FE" = m_instr), vcov = ~InstrID, stars = TRUE, gof_map = c("r.squared", "nobs"), output = "table.tex")

ggsave("list.tex")
