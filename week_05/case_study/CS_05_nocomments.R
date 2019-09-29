library(spData)
library(sf)
library(tidyverse)
library(units)
# library(units) #this one is optional, but can help with unit conversions.

#load 'world' data from spData package
data(world)
# load 'states' boundaries from spData package
data(us_states)
# plot(world[1])  #plot if desired
# plot(us_states[1]) #plot if desired

albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# reproject spatial data
world <- st_transform(world,albers)
us_states <- st_transform(us_states,albers)

# generate a polygon that includes all land in NY that is within 10km of the Canadian border and calculate the area
ny <- us_states %>% 
  filter(NAME=="New York")

world %>% 
  st_transform(albers) %>% 
  filter(name_long=="Canada") %>% 
  st_buffer(dist = 10000) %>% 
  st_intersection(ny) %>% 
  ggplot() +
  geom_sf(data=ny)+
  geom_sf(fill="red")+
  ggtitle("New York Land within 10km")

ggsave("Beware_the_Canadians.png")

# area calculation
area <- world %>% 
  st_transform(albers) %>% 
  filter(name_long=="Canada") %>% 
  st_buffer(dist = 10000) %>% 
  st_intersection(ny) %>% 
  st_area() %>%
  set_units(km^2) %>% 
  print()
