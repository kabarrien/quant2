#Karolina Barrientos
#AQMSII
#Take Home Assignment 

setwd("C:/Users/Karolina/OneDrive/Documents/Applied quant 2/quant2/Assignment7")

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(fixest)
library(modelsummary)
library(sf)
library(spData)


data(world)

df= read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/spatial/conflict_events.csv")

view(df)

#2.1 Converting tabular data to sf============================

#a) cover the events data frame to an sf object 

events <- st_as_sf (df, coords = c("longitude", "latitude"), crs = 4326)

class(events)

st_crs(events)

#st_as_sf() is converting the data frame into an sf object. We use the coords argument to tell the function which columns contain location data nd then the function packages these into spatial points so that R can plot and analyze the data. CRS= 4326 attaches metafata to the data frame, this tells r where these points are located on the earth's surface.

#b) How many events are in the dataset?

nrow(events)

#There are 68354 total events in the dataset.

table(events$event_type)

#There are 10418 non-state events, 24449 one-sided events, and 33487 state-based events. 

data(world)

#map of conflict events over world polygon

cemap = ggplot()+ geom_sf(data = world, fill = "grey90", color = "white") + geom_sf(data = events, aes(color = event_type), alpha = 0.5, size = 0.5) + theme_minimal() + labs(title = "Global Conflict Events", color = "Event Type")

print(cemap)

ggsave("cemap.png")

#The conflicts in this data set are concentrated in Africa.

#2.2 Spatial Join: Events to countries=======================

#a) Use st_join to assign country attributes from the world polygon to each conflict

#need to make sure that the CRS matches
st_crs(world) == st_crs(events)

#use st_join() to assign each conflict the country attributes 
events_joined <- st_join(events, world)

nrow(events_joined) == nrow(events)

#st_join, merges two datasets based on their gerographic relationship, it checks the geometric coordinates of each point (events data) against the boundaties of the country polygons (world data). When a match is found st_join() takes the the cooresponding info from the world dataset and adds them to the row for each corresponding event. Checking the CRS is important because we need to make sure that both datasets are using the same mathematical instructions for how coordinates are mapped onto the earth's surgace.

#b) checking matching the polygons 

sum(is.na(events_joined$name_long)) #1576

sum(is.na(events_joined$name_long))/nrow(events_joined) #0.023, #about 2.3% of the conflict events did not match any country polyhon during the spatial join. This could happen because small islands may not be included in the polygons of the world dataset. There could also be issues if a conflict was on the border or slighly outside a defined border. 

#c) counting the number of events and total fatalities per country 

#summary table of events and fatalities per country
country_summary <- events_joined %>%
  filter(!is.na(name_long))%>%
  st_drop_geometry() %>%
  group_by(name_long)%>%
  summarise(n_events = n(), total_fatalities = sum(fatalities, na.rm = TRUE)) %>%
  arrange(desc(n_events))

#print top 10 
head(country_summary,10)

#Top 10 countries with highest number of events, Democratic Republic of Congo, Nigeria, Somalia, Ethopia, Algeria, Sudan, Burundi, Mali, Rwanda, South Africa.

#The Top 10 countries with the highest number of events in Africa is consistent with geographic patterns of contemporary armed conflict. For example the Congo and Rwanda are facing persistent insurgencies and civil unrest.

#2.3 Choropleth of conflict intensity 

#a) joining event counts back to the world polygon data

event_counts <- events_joined %>%
  st_drop_geometry()%>%
  filter(!is.na(name_long))%>%
  group_by(name_long)%>%
  summarise(n_events = n())

#^ this drops spatial features to treat as a standard table and removes events that didnt match a country 

#merge the counts into the world map object 

world_counts <- world%>%
  left_join(event_counts, by = "name_long") %>%
  mutate(n_events = replace_na(n_events, 0)) 

nrow(world_counts) == nrow(world) #TRUE

#b) Make a choropeth map

choroplethmap = ggplot(world_counts) + 
  geom_sf(aes(fill = n_events)) +
  scale_fill_distiller(palette = "Reds", direction = 1) + theme_minimal() + 
  labs(title = "Conflict Event Counts by Country", fill = "Number of Events")

ggsave("choroplethmap.png")

#Both maps share a geographic pattern and show similar global hotspots. The plots differ in their visual representation, in 2.1c individual points are used while here points become national totals. 

#c) log transformed map

#make map with log-transformed counts to visualize variation less skewed 

logmap = ggplot(world_counts) + geom_sf(aes(fill = log1p(n_events))) + scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Log(events+1") + theme_minimal() + labs(title = "Log Transformed Conflict Intensity")

ggsave("logmap.png")

#The log transformation accounts for countries with 0 events and is helpful because of the skewing in the data. The skewing allows to see the low end of the data.

#2.5 Discussion =========================

#a) A limitation of the spatial joint approach we used is the how it can miscalculate and get confused when events occur along a border. For an event that falls just outside a polygon because of small issues in the coordinate, I could add "join = st_within" the code for the points to fall inside the polygon.

#b) 
#The difference between st_join and left_join is that in st, the code uses geographic location while in left, its based on a specified common variable. I would prefer st_join when  we need to link two different spatial layers and left_join when merging non-spatial data.
