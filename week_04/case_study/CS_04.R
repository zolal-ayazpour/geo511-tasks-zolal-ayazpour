library(plyr)
library(tidyverse)
library(nycflights13)

flights <- flights
airports <- airports

# first solution

farthest_airport_solution_1 <- flights %>%
  arrange(desc(distance)) %>% 
  rename("faa"="dest") %>% 
  left_join(airports[,c("faa","name")],by="faa") %>% 
  group_by(origin) %>% 
  summarise(airport=first(name))

View(farthest_airport_solution_1)

# second solution:

farthest_airport_solution_2 <- flights %>%
  arrange(desc(distance)) %>% 
  select("origin","dest","distance") %>% 
  rename("faa"="dest") %>% 
  left_join(airports[,c("faa","name")],by="faa") %>% 
  group_by(origin) %>% 
  slice(1)

View(farthest_airport_solution_2)