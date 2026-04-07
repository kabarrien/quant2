#Karolina Barrientos
#Assignment 8 
#In Class

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 8")

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(fixest)
library(modelsummary)
library(sf)
library(spData)
data(world)



library(spdep)
library(spatialreg)

#1.1 Setup and OLS baseline:

#a) Filter the data 

world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp),]

world = world[world$continent != "Antarctica",]

world$log_gdp = log(world$gdpPercap)

nrow(world)

#160 countries remain in the set, we log-transform GDP per capita bc the raw variable is skewed right to where a handful of rich countries have values far above the bulk of the distribution. The log transformation compresses the upper tail and makes the relationship between GDP and life expectancy more linear, which is an assumption of OLS. 

#b) OLS regression of life expectancy on log GDP per capita: 

ols_fit = lm(lifeExp ~ log_gdp, data = world)

summary(ols_fit)

#The coef on log_gdp is 5.5403 and is positive as well as statstically sig since the pvalue is super small. Meaning one unit increase in logGDP per capita is associated with higher life expectancy by the approximately that many years on average. The model explains a substantial share of cross-country variation in life expectancy, as reflected by the R^2. 

#c) Save OLS residuals and map them:

world$ols_resid = residuals(ols_fit)

ggplot(world) + geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) + scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d", midpoint = 0, name = "OLS residual") + theme_void() + labs(title = "OLS residuals: life expectancy ~ log GDP per capita")

ggsave("ols_residuals_map.pdf", width = 10, height = 5)                                                                                                       
#The residual map revelas clear geographic clustering. Sub Saharan Africa shows a concentration of negative residuals- countrie with lower life expectancy than the model predicts given their income level, likely due to high HIV /AIDS prevalence and disease burden. Western Eruo and parts of East Asia display postive residuals, indicatin that these regions achieve higher life expectancy than income alone predicts. 
#1.2 Spatial weights matrix

#a) build queen contiguity neighborhoods and row-standardized weights

nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)

summary(nb)

#Some countries have 0 countries in the contiguity matrix. These are island nations like New Zeland and Caribbean states, that share no land boundary or common border point with any other polygon in the dataset. Queen contiguity requires at least one shared point, islands surronded by nation ocean have non some they are isolated nodes in the weights graph. The zero.policy = TRUE argument allows thee units to remin the analysis despite having no neighbors. 


#b) Moral I test on OLS residuals 

moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)

#The Moran's I stat is positive and the p-value is well below 0.05 and indicates a stat significant positive spatial autocorrelation in the OLS residuals. Countries close to each other tend to have similar residuals either both are overestimated or underestimated. this violates the OLS assumption of independent. So by ignoring this pattern yields inefficient estimates and invalid standard errors. 

#1.3 lagrange multiplier tests

lm_tests = lm.RStests(ols_fit, listw = listw, test = c("LMerr", "LMlag", "RLMerr","RLMlag"))

summary(lm_tests)

#RSerr- 52.17, #RSlang- 0.06157, #adjRSerr 54.305760, #adjRSlanf #2.197282 

#RSerr tests whether there is a spatial dependence in the error term. RMlaf tests whether a spatially lagged  dependent variable belongs in the model. Both tests are significant thus both types of spatial dependence appear to be present in some form when tested individually. Both both standard LM tests are significant, we turn to robust versions to discriminate.

#b) The robust tests each control for the presence of the other type of spatial dependence. Comparing them: if adjRSerr is more significant than adjuRSlag, the evidence favors the SEM, if adjRSlag dominates, the SLM is preferred. Based on the decision rule from class- select the model whose robust test is more sig, so the LM error was sig.


#1.4 Spatial error Model (SEM)

#fit the SEM using errorsarlm():

sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world, listw = listw, zero.policy = TRUE)

summary(sem_fit)                    

#a) The coefs on log_gdp from the SEM and the OLS estimate are both reported above. The SEM coef may shift somewahat from OLS bc the error-structure  correction absorbs spatial confounding. The lambda parameter captures spatial autocorrelation in the errors, is it is positive and statistically sig, the SEM has identified genuine spatial dependence in the residual variation.

#b) In the SEM, lambda governs the spatial autoregressive process in the disturbance. A positive and sig lambda means that the unmeasured factors driving life expectancy are spatially correlated- omitted variables such as regional disease environments, cultural practices around healthcare, or cross border health infrastructure are themselves geographically clustered. The SEM filters this spatial correlation out of the residuals without positing that life expectancy itself directly diffuses across borders.

#c) Check Moran's I on SEM residuals"

world$sem_resid = residuals(sem_fit)

moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)

#comparing the Moran's I to the one from  question 2b, the SEM substantially reduces the spatial autocorrelation in the residuals. The test statistic is now much closer to zero and the p-value is no longer significant (or much less so), indicating that the spatial error correction has absorbed most of the geographic clustering that the OLS left behind in its residuals. 


#Part 2: Take- Home (Spatial Log Model and Model Comparison)

#2.1 Spatial Log Model (SLM)

#a)report the estimated rho parameter and its pvalue and report the coef on log_gdp. Is the p statistically sig?

slm_fit = lagsarlm(lifeExp~ log_gdp, data = world, listw = listw, zero.policy = TRUE)

summary(slm_fit)

#The estimated rho is -0.0004251 and the p-value is 0.806. The coef for log_gdp is about 5.5. The pvalue is NOT statistically significant. 

#b) interpret rho, If p>0 what does this mean about the relationship between a country's life expectancy and its neighbors life expectancy?

#-0.0004251, the rho, represents the spatial spillover effect, essentially how much the life expectancy of one country is influenced by the life expectancy of its neighborhoods. With a very low rho and a statistically insignificant p value, there is not significant spatial lag effect. 


#c) explain why the coeff on log_gdp in the SLM output is NOT the marginal effect of GDP on life expectancy. What does this equilibrium matrix imply for how a change in xi propagates through the network? 

#In the SLM, the presence of a spatial lag means tat the y appears on both sides of equation. The coeff on log_gdp only is the "pre-spatial" or direct internal impact. It does NOT account for an spatial spillovers. Bc y is determined simultaneously across the map, a change in x causes a chain reaction though the spatial weights matrix The (I-rhoW)^ -1, matrix represents a spatial multiplies effect in which a a change in one country's GDP propagates through tr weights matrix W, affecting neighbors, neighbors-of-neighbors and so on. Additional steps need to be taken in order to account for marginal effects. 

#2.2 Direct and Indirect Effects:

#a) compute the equilibrium direct and indirect effects using the impacts()function, passing the SLM fit and spatial weights. Use R=500 for simulation based standard erros. In a comment, report the direct effect, the indirect effect, and its total effect of log_gdp. How does the direct effect compare to the raw log_gdp coef from the SLM output and to the OLS coef?

set.seed(123)


imp = impacts(slm_fit, listw = listw, R = 500)

print(imp$res)

#direct- 5.5
#indirect -0.0235
#Total effect" 5.5247

#The Direct effect is almost the same as the raw SLM coef, and the OLS coef, with it being close to 5.5 and the indirect effect being super small. 


#b)In a comment (2–3 sentences), explain the substantive meaning of the indirect effect. Recall from class: the indirect effect captures the spillover from unit i’s x to all other units’ y, after the spatial feedback loop reaches equilibrium. If log GDP per capita in Country A increases by 1 unit, what does the indirect effect say about life expectancy in neighboring countries? 

#The indirect effect depicts the spatial spilllover. It suggests that an increase in log GDP per capita of country A by 1 unit, will indirectly decrease the life expectancy of a neighboring country by 0.02 units. However because this the pvalue is so insignificant, in this dataset, country's wealth does not have a meaningfull spillpover impact on other countries. 

#c) The total effect is larger than the direct effect. In a comment, explain whether this is an expected feature of the SLM. Under what conditions would the indirect effect be larger or smaller? (Hint: think about what happens to the spillover term as ρ approaches 0 versus as ρ grows larger.)

#Typically  in a SLM, the direct effect is largerly in SLM but in our case its the opposite becaues of the negative indirect effect. When rho appraoches 0, the indirect effect vanishes while when it increases to 1, the indirect effect grows exponentially bc of the spatial multiplies. 

#2.3 Model Comparasion 

#a) Compare OLS, SEM, and SLM using AIC(). Lower AIC indicates better fit, penalized for model complexity. In a comment, report the three AIC values. Which model has the lowest AIC? Does this agree with your LM-test-based model choice in question 1.3b?

AIC(ols_fit, sem_fit, slm_fit)

#       df      AIC
#ols_fit  3 965.9880
#sem_fit  4 894.7021
#slm_fit  4 967.9270

#Here the SEM has the lowest AIC and would be the best fit for our case. When looking at 1.3b, the tests pointed to the EM movile bc of the significance of the Robust LM error. 

#b)  Write a short summary paragraph as a comment in your R script (5–8 sentences). Include all of the following: (1) whether spatial autocorrelation was present in the OLS residuals and how strong it was; (2) which spatial model you selected based on the LM tests and why; (3) how the key coefficient estimate on log gdp differs across OLS, SEM,and SLM; (4) what the SLM implies about life expectancy spillovers across borders; (5) one limitation of using queen contiguity weights for country-level data (think about what the matrix misses).


#In the OLS residuals there was in fact spatial autocorrelation given the a highly significant Moran's I test. This indicates that countries with similar life expectancy are clustered geographically. Based on the LM tests, the best spatial model is the Spatial Error Model because of the robust LM error statistic was significant while the Robust LM-Lag was not. This suggests that spatial dependence lies in the omitted variables rather than a direct lag of the dependent variable. Because the coefficient for log GDP stayed fairly stable across the three models, we can suggest that wealth is a robust predictor of life expectancy regardless of spatial specification. The SLM results showed an insignificant rho and a near-zero indirect effect. This implies there is little evidence of life expectancy spill over across borders after a country's own GDP is accounted for. The major limitation when using the queen contiguity weights for country level data is that ignores geographic closeness between countries separated by small bodies of water. It treats them as having no neighbors thus also from the spatial influence network.
