library(raster)
library(sp)
library(spData)
library(tidyverse)
library(sf)

setwd("/Users/zolalzzz/Documents/Main/PhD/Fall2019/SpatialDataScience/Git/geo511-tasks-zolal-ayazpour/week_06/case_study")

# load the data
data(world)  #load 'world' data from spData package
tmax_monthly <- getData(name = "worldclim", var="tmax", res=10)

# remove Antarctica from the data and convert the new data to Spatial
world_data <- world %>% 
  filter(continent != "Antarctica") %>% 
  as("Spatial")

# temerature modification
gain(tmax_monthly) <- 0.1

# find the max temperature in 12 months
tmax_annual <- max(tmax_monthly)

# change the name of the layer
names(tmax_annual) <- "tmax"

# calculate the maximum temperature observed in each country.
tmax_annual_country <- raster::extract(tmax_annual, world_data, fun=max, na.rm=T, small=T, sp=T)

# convert the data to sf
tmax_annual_country_sf <- st_as_sf(tmax_annual_country)
  
# plot
ggplot() +
  geom_sf(data=tmax_annual_country_sf,aes(fill=tmax))+
  scale_fill_viridis_c(name="Annual\nMaximum\nTemperature (C)")+
  theme(legend.position = 'bottom')+
  ggsave("annual_max_temp.png")

# find the hottest country in each continent
tmax_table <- tmax_annual_country_sf %>% 
  group_by(continent) %>%
  st_set_geometry(NULL) %>% 
  top_n(1) %>% 
  arrange(continent) %>% 
  select(name_long,continent,tmax)
  
  

