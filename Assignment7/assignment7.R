#Karolina Barrientos
#Assignment 7- Spatial Data I 

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment7")

##In-Class Assignment============================================================

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(fixest)
library(modelsummary)

install.packages("sf")
install.packages("spData")

library(sf)
library(spData)
#1.1 Inspecting an sf object

data(world)

class(world)

names(world)

nrow(world) #177

#class(world) returns both "sf" and "data.frame": an sd object is a refulat data frame augmented with an extra geometry column (of class sfc) that stores the spatial shapes(polygons, points, lines). The geom column is "sticky" standard dyply operations (filter,mutate, select) retain it automatically, so spatial attributes travel with the data without any extra effort. 

#b) check the coordinate reference system

st_crs(world)

#The dataset uses EPSG:4326 (WGS84- world geodetic system 1984). WGS884 is the global standard coordinate system used by GPS and most mapping tools online. Coordinates are expressed in decimal degrees of longitude (east-west) and latiutde (north-south), making it suitable for global datasets where a common datum is needed across all regions.

#c) Inspect geometry types:

unique(st_geometry_type(world))

#18 levels: Geometry Point Linestring Plygon Multipoint... Triangle 

#The geometry type is MulTIPOLYGON. A MULTIPLYGON is a collection of one or more polygons treated a single geographic feature. Countries require multiple polygons when their terriort is not a single contiguous land mass. For example te US includes Alaska and Hawaii as separate polyons , and Grance includes overseas terriotires such as Martinique and Guadelope in the Carribbean.

#d) Quick base-R map of GDP per capita: 

pdf("world_gdp_base.pdf")

plot(world["gdpPercap"])

dev.off()
##pdf 
##null device is 1 

#Display inline as well:

gdpPercapmap<- plot(world["gdpPercap"], main = "GDP per cappita by country")

ggsave("gdpPercapmap.png")

#The map reveals a global inequality pattern. Western and North Euro, North America and Austrualia/New Zealand appea as the wealthiest regions (dark end of the scale). Sub-Saharan Africa and parts of South and Southeast Asia occupy the lowest end. East Asia shows intermediate to high values, reflecting rapid economic growth in countries such as South Kore and Japan.

#1.2 Attribute Operations=========================================================

#a) Filter to African countries 

africa = filter(world, continent == "Africa")

nrow(africa) #51 countries in the continent of Africa

plot(africa["gdpPercap"], main = "GDP per capita -- Africa")

#The data set contains 51 African countries. The UN recognizes 54 sovereign African states, so this count is slightly below expectations and likely reflects missing data or the exclusion of very small territories from the spData world polygon dataset. 

#b) Add pop_millions and summarise GDP per capita by continent: 

world = world %>%
  mutate(pop_millions = pop/1e6)

gdp_by_continent = world %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm= TRUE))

print(st_drop_geometry(gdp_by_continent))

 #when summarise() is called on grouped sf object, it unions the geometrices within eahc group and retains the resulting geometry column. To obtain a plain data frame without spaitial information, use st_deop_geometry() before or after the summary step. This avoids carrying unneded geometry through putely rabular analyses. 

#c) Top 5 African countries by GDP per capita:

africa_sorted = africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)
print(head(st_drop_geometry(africa_sorted),5))

#The five African countries with the highest GDP per capita in this dataset are Equatorial Guinea, Gabon, Libya, Botswana, Algeria. 


#1.3 Simple Visualization with ggplot2=======

#a) Choropleth map of world GDP per capita: 

ggplot(world) + geom_sf(aes(fill = gdpPercap)) + scale_fill_viridis_c(option = "plasma", na.value = "grey80", name = "GDP per capita") + theme_void() + labs(title = "GDP per capita by country") 

ggsave("world_gdp.pdf", width = 10, height = 5)

#The geographic pattern mirrors what the base-R map showed. Western Euro, North America, and Oceania stand out as the wealthiest cluster. East Asia shows a gradient from high (Japan, South Korea) to middle (china). Sub-Saharan Africa, and South Asia concentrate the lowest values, with a few exceptions (eg. Equatorial Guinea's oil wealth). 

#b) Africa map with magma palette:

ggplot(africa) + geom_sf(aes(fill = gdpPercap)) + scale_fill_viridis_c(option = "magma", na.value = "grey80", name = "GDP per capita")+ theme_void() + labs(title = "GDP per capita-- Africa")

ggsave("africa_gdp.pdf", width = 7, height = 6)
#within Africa, there is substantial variation. A cluster of relatively wealthier countries appears in North Africa (Egypt, Libya, Tunisia) and in Southern Africa (Botswana, South Africa, Namibia). Central and West Africa (with the exception of oil-rich Equatorial Guinea and Gabon) display the lowest values, reflecting low diversification and persistent structural poverty. 

#c) Africa map with country borders 

ggplot(africa) + geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) + scale_fill_viridis_c(option = "magma", na.value = "grey80", name = "GDP per capita") + theme_void() + labs(title = "GDP per capita == Africa (with borders)")

#adding white country borders significantly improves readability, especially for smaller countries where adjacent fill colors alone make it hard to distinguish units. The thin white line demarcate each country without competing visually with the fill scale, making it easier to identify specific countries of interest and to compare neighbors. 



