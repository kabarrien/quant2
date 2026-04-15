#Karolina Barrientos 
#Assignment 9


#In-Class Portion(Ordinal, Multinominal, and Count Outcomes)

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 9")


library(carData)
library(MASS)
library(nnet)
library(marginaleffects)
library(tidyverse)
library(ggplot2)
library(AER)
data(BEPS)

#1.1 Ordered logit: perceptions of the national economy

#a) explore the outcome and convert to an ordered factor:

table(BEPS$economic.cond.national)

#1  2    3   4   5
#37 257 607  542  82

BEPS$econ_ord = factor (BEPS$economic.cond.national, ordered = TRUE)

summary(BEPS$econ_ord)

#The distribution is cocentrated in the middle categories (2,3,4), with category 3 being the modal response. Not many respondents chose the extreme ends. 

#b) Fit the ordered logit model

m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge, data = BEPS, Hess = TRUE)

summary(m_ologit)

#c) compute average marginal effects across all response categories

avg_slopes(m_ologit)

#d) Predicted probabilities by gender

predictions(m_ologit, newdata = datagrid(gender = c("female","male")))


library(dplyr)
pred = tidy(predictions)

#1.2 Multinomial logit: Vote choice

#a) set the reference category and fir the multinomial logit:

BEPS$vote = relevel(BEPS$vote, ref = "Conservative")

m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague + Kennedy + Europe, data = BEPS, trace = FALSE)

summary(m_mlogit)

#b) compute average marginal effects across all predictors and outcome categories
avg_slopes(m_mlogit)

predictions(m_mlogit, by = "economic.cond.national")

#1.3 Poisson regression: publication counts

install.packages("pscl")
library("pscl")

data(bioChemists)

#a) explore the outcome variable:

summary(bioChemists$art)

var(bioChemists$art) #3.7097

pdf("art_histogram.pdf", width = 6, height = 4)
hist(bioChemists$art, breaks = 20, main = "Distribution of articles",
     xlab = "Number of articles", col = "gray80")
dev.off()

#The distribution of art is right skewed, with a mode at zero and a long upper tail. The mean is around 1.69 while the variance is aprox 3.71- roughly twice the mean. Under the poisson assumption, the variance should equal the mean, a ratio substantially above the 1 indicates over-dispersion.

#b) fit the poisson regression 

m_pois = glm(art ~ fem + mar + kid5 + phd + ment, data = bioChemists, family = poisson)

summary(m_pois)

exp(coef(m_pois)["ment"])
#1.02872

#The incidence rate ratio for ment is 1.026: each additional article published by the mentor is associated with multiplicative increase in expected student articles by that factor, holding all else constant.There is a slight positive effect suggesting that more productive mentors slightly boost student output. The residual deviance is substantially larger than the residual degrees of freedom (ratio well above 2). This is another clear sign of over dispersion- the poisson model does not adequately capture the variation in publication counts. 

#c) Formal overdsispersion test: 

dispersiontest(m_pois) #1.82454

#The overdispersion test strongly rejects the null hypo of equidisperson. the estimated dispersion paramter is well above 1, confirming that the variable in art substantially exceeds its mean.This means the poisson standard errors are too small: the model underestimates uncertainty, inflates test statsics, and produces p-vales that are misleadingly small. A model that explicitly accounts for overdispersion- such as the negative binomial is needed. 


#1.4 Negative binomial regression

#a) fit the negative binomial model:

m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment, data = bioChemists)

summary(m_nb)

#The coef on ment is similar to the poisson estimate, indicating that the point estimate is reasonably stable. the key difference in the standard errors is that the negative binomial model produces larger, more honest uncertainty estimates. 

#b) compare model fit by AIC

AIC(m_pois) #3314.113

AIC(m_nb) #3135.917

#the negative binomial AIC is much lower than the Poisson AIC, despite the first mpdel having one additional parameter, that is theta. Under AIC, the improvement in fit more than compensates for the added complexity. This confirms that overdispersion is a genuine feature of the data, not noise, and that the negative binomial is te more appripriate model for these publication counts. 

#c) Predicted article counts by gender, holding other variables at sample means: 

predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))

#The predicted number of articles for men exceeds that for women, when holding martial status, number of young children, PhD prestige and mentor productivity constant at their sample means. The confidence intervals provide information on whether this gender gap is statistically distinguishable: if the intervals do not overlap, the difference is significant at conventional levels. The gap reflects a persistent within-group gender difference in publication productivity that is not simply an artefact of other observable characteristics.

#d) summary of findings:

#The possion model is not adequate for this dataset. The variance to mean ratio of art is roughly double, the residual deviance for exceeds the degrees of freedom, and the formal dispersiontest() rejects equidipersion with a p-value of below 0.001. The negative binomial model, which adds a dispersion parameter to accommodate this extra variation, achieves a substantially lower AIC and produces more reliable (wider) standard errors. On substantive findings: the mentor's productive mentor confers a real, if small, boost. Gender (fem) and number of young children are both negative and statistically significant effect, with an IRR slightly above 1 - each additional child under age 5 is associated with reduced output.Phd program prestige (phd) and marital status (mar) are not statistically significant in th negative binomial model. Together, the results point to early-career productivity being shaped by mentor environment, gender, and family demands- patterns consistent with broader literature on PhD student outcomes in STEM fields.
#2 Part 2: Take-Home (Survival Analysis) ================


library(survival)
library(broom)
library(ggplot2)
library(marginaleffects)
library(dplyr)

lung = survival::lung

head(lung)
dim(lung)
summary(lung) 

lung$dead <- lung$status - 1

#Total number of observations: 288 patients. 

nrow(lung) #228
sum(lung$dead == 1) #165
sum(lung$dead == 0) #63

63/228 #0.277

#There is about at 28% of patients censored in the data. With 63 patients exceeding the observation period as they sirvived the cancer treatment. 

#b) overall Kaplan-Meir Survival Curve

fit_overall <- survfit(Surv(time, dead) ~ 1, data = lung)

summary(fit_overall)

median_survival <- summary(fit_overall)$table["median"]

median_survival
#310 

#Based on the Kaplan-Meir survival survival, the median survival time is #310 days after enrollment, a little under a year. This represents about the time where about 50% of patients are expected to experience death.

#c) Kaplan-meier curves by sex with visualization 

#stratify by sex
fit_sex <- survfit(Surv(time,dead) ~ sex, data = lung)

summary(fit_sex)

#convert object to data frame 

km_data <- tidy(fit_sex)

#labeling sex in data frame

km_data$sex_label <- ifelse(km_data$strata == "sex=1", "Male", "Female")

#create the kaplan-meier plot 

km_plot <- ggplot(km_data, aes(x = time, y = estimate, color = sex_label, fill = sex_label)) +
  geom_step(size = 1, direction = "hv") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    title = "Kaplan-Meier Survival Curves by Sex",
    subtitle = "Lung Cancer Dataset (N=228)",
    x = "Time (days)",
    y = "Survival Probability",
    color = "Sex",
    fill = "Sex"

km_plot

ggsave("kaplan_meir_by_sex.pdf", plot = km_plot)

#log-rank test

logrank_test <- survdiff(Surv(time, dead) ~ sex, data = lung)
logrank_test

#pvalue = 0.001,

#In the K-M plot, we see that the female curve (red) is well above the male curve (blue). This indicated that women have have a better chance at survival compared to men. When looking at the confidence interval we see little overlap. The log-rank p-vale is 0.001 and is highly significant. This test is a nonparametric test of the null hypothesis that the two survival curves are identical. The small p-value provides strong evidence against the null hypothesis. There is a statistically significant difference in survival between males and females in the lung cancer cohort of interest. 

#2.2 Cox Proportional hazards model 

#a) fit cox proportional hazards model 

cox_model <- coxph(Surv(time, dead) ~ age+ sex + ph.ecog, data = lung)

print(cox_model)

cox_summary <- tidy(cox_model, exponentiate = TRUE)

cox_summary

#coef estimate 0.575,
#coef pvalue - 0.000986 

confint(cox_model)
exp(confint(cox_model))
#2.5%- 0.41 97.5%- 0.8%- exponent 
#The 95% confidence interval for the hazard ratio has a bound of 0.41-0.8, making it statistically significant since it does not include one. This underscores the idea that women have a significantly lower hazard of death compared to men.

#The hazard ratio is 0.575, meaning women have a 42.5% lower risk of death (1-0.575). With an extremely small p-value, 0.000987 is statistically significant. 

#b) Interpret the hazard ratio for ph.ecog. 

#For ph.ecog, the hazard ratio is 1.59, indicating a one-unit increase in ECOG performance is associated with with a 59% (1.59-1) higher hazard of death. This basically means that patients with worse health status have much shorter  survival times. Ph.ecog has a confidence interval bound of 1.27 and 1.98.

#c) proportional hazards

ph_test <- cox.zph(cox_model)
print(ph_test)
#pvales for age (0.66), sex (0.13), ph.ecog (0.15) and GLOBAL (0.22). These p-values are not statistically significant. Indicating no evidence of a violation of te proportional hazards assumption and the effects of the covariates can be interpreted as constant over time. 

#d) summary paragraph 
#The Kaplan-Meier survival curves suggest that women tend to have higher survival probabilities then men. This indicates a survival advantage for women. The cox model confirms this pattern as sex and ph.ecog are statistically significant predictors of survival. Though when looking at age is not. Women also have a lower hazard of death compared to men while lower ECOG performance status is associated with a substantially higher hazard of death. Looking at the proportional hazards, the assumption is not violates, suggesting that the effects of the covariates remain consistent over time.  In conclusion the current physical health status of a patient is a key predictor of survival because worse health is strongly associated with shorter survival times. 
