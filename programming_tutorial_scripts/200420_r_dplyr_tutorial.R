# R intro with Lab - 4/20/20

library(tidyverse)

gapminder_orig <- read.csv("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder-FiveYearData.csv")

gapminder <- gapminder_orig

dim(gapminder)

head(gapminder)


gapminder_piped <- gapminder %>%
  filter(continent == 'Asia', year == '2007', country == 'Afghanistan') %>%
  select_all()


gapminder <- gapminder %>% 
  mutate(gdp = gdpPercap * pop) %>%
  select_all()

gapminder %>% 
  arrange(desc(lifeExp)) %>%
  head

gapminder %>%
  group_by(continent) %>%
  filter(lifeExp > mean(lifeExp)) %>%
  ungroup() 


gapminder %>% 
  summarise(mean_lifeExp = mean(lifeExp), total_gdp = sum(gdp))

gapminder %>% 
  group_by(country, year) %>%
  summarise(mean_lifeExp = mean(lifeExp),
            total_gdp = sum(gdp)) 



