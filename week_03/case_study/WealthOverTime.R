library(ggplot2)
library(gapminder)
library(dplyr)

# First dataset
dataSet_1 <- gapminder %>%
  filter(country != "Kuwait")

#First plot
ggplot(dataSet_1, aes(lifeExp, gdpPercap, size = pop/100000, color = continent))+ 
  geom_point()+
  facet_wrap(~year,nrow=1)+
  scale_y_continuous(trans = "sqrt")+
  theme_bw()+
  xlab("Life Expectancy")+
  ylab("GDP per capita")+
  labs(title = "Wealth and life expectancy through time")+
  labs(size = "Population(100K)", col="Continent")+
  theme(axis.text.x = element_text(size=6))+
  guides(size= guide_legend(order=1), color= guide_legend(order=2))
  
ggsave("firstPart.png", width=15)

# Second dataset
dataSet_2 <- dataSet_1 %>%
  group_by(continent, year) %>%
  summarize(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop),pop = sum(as.numeric(pop)))
  
#Second plot
ggplot(dataSet_1,aes(year, gdpPercap, color=continent))+
  geom_line(aes(group=country))+
  geom_point()+
  geom_line(data=dataSet_2,aes(year,gdpPercapweighted),col="black")+
  geom_point(data=dataSet_2,aes(year,gdpPercapweighted,size=pop/100000),col="black")+
  facet_wrap(~continent,nrow=1)+
  theme_bw()+
  xlab("Year")+
  ylab("GDP per capita")+
  labs(title = "Wealth and life expectancy through time")+
  labs(size="Population(100K)", col="Continent")+
  theme(axis.text.x = element_text(size=5))+
  scale_size_continuous(range=c(1,5))+
  guides(size= guide_legend(order=1), color= guide_legend(order=2))

ggsave("secondPart.png", width=15)

