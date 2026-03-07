#Karolina Barrientos
#Assignment 5 

#Part 1: In Class Assignment 

#1.1 Setup and Data exploration

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment 5")

library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(tidyverse)

df = read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")

view(df)

length(unique(df$State))

#50

length(unique(df$Year))

#32

table(table(df$State))

The panel is unbalanced as there is not the same number of observations per year-state)

#b) Summary Stats for the key variables/ exploring the data

summary(df$PresApprov)

summary(df$UnemPct)

df_sub = df %>%
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_sub, aes(x = Year, y = PresApprov, color = State)) +
  geom_line() + theme_minimal() + labs(x = "Year", y = "Presidential Approval (%)", color = "State")

#The 3 states move closely together over time, tracking the smae large swings in approbal. The movement can suggest that common national factors are a large driver of aproval, while state- level differences are stable.


#c) 
ggplot(raw, aes(x=UnemPct, y=PresApprov, color=State))+
  geom_point(alpha = 0.4)+
  theme_minimal()+
  theme(legend.position="none")+
  labs(x="Unemployment rate (%)", y="Presidential approval (%)")

#2) Pooled OLS

m_pooled = lm(PresApprov ~ UnemPct, data = df)

summary(m_pooled)
#The coeff on UnemPct is negative: one percentage point increase in the unemploymenet rate is associated with a decrease
#of t hatagnitude in the presidential approval rating. This relationship is statistically sig, but it conflates variation across states with cariation within states overtime.

#b) Adding south as a control:
m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df)
summary(m_pooled2)

modelsummary(list(m_pooled, m_pooled2), stars = TRUE)


#3Entity fixed effects

#a) state fixed effects

m_fe = feols(PresApprov ~ UnemPct | State,data = df)

ols = lm(PresApprov = PresApprov - mean(PresApprov), UnemPct = EnemPct - mean(UnemPct)) %>% ungroup

modelsummary(list("Pooled OLS" = m_pooled,"State FE" = m_fe),
             vcov = ~State, stars = TRUE, gof_map = c("r.squared", "nobs"))
#b)
#c)

#4 Two-way fixed effects

#a-b) Adding year fixed effects to control for common time shocks:

m_twefe = feols(PresApprov ~ UnemPct | State + Year, data = df)

modelsummary(list("Pooled" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twefe), vcov = ~State, stars = TRUE, gof_map = c("r.squared", "nobs"))
            
             