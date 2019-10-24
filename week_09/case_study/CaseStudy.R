setwd('/Users/zolalzzz/Documents/Main/PhD/Fall2019/SpatialDataScience/Git/geo511-tasks-zolal-ayazpour/week_09/case_study')
library(sf)
library(tidyverse)
library(ggmap)
library(rnoaa)
library(spData)
library(knitr)
data(world)
data(us_states)

path <- storm_shp(basin = "NA")
data <- read_sf(path)

storms <- storm_data %>% 
  filter(Season>=1950) %>% 
  mutate_if(is.numeric, function(x) ifelse(x==-999.0,NA,x)) %>% 
  mutate(decade=(floor(year/10)*10))

region <- st_bbox(storm_data)

ggplot(world)+
  geom_sf()+
  facet_wrap(~decade)+
  stat_bin2d(data=storms, aes(y=st_coordinates(storms)[,2], x=st_coordinates(storms)[,1]),bins=100)+
  scale_fill_distiller(palette="YlOrRd", trans="log", direction=-1, breaks = c(1,10,100,1000))+
  coord_sf(ylim=region[c(2,4)], xlim=region[c(1,3)])+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

crs <- st_crs(storms)
states <- st_transform(us_states,crs)

storm_states <- st_join(storms, states, join = st_intersects,left = F)  %>% 
  group_by(NAME) %>% 
  summarize(storms=length(unique(Name))) %>% 
  arrange(desc(storms)) %>% 
  slice(1:5) %>% 
  st_set_geometry(NULL)

ggsave("plot.png")

table <- gt(storm_states)
table
  