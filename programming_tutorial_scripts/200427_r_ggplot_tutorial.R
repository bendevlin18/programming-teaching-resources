##ggplot tutorial

#Adapted from http://www.rebeccabarter.com/blog/2017-11-17-ggplot2_tutorial/

#Throughout this tutorial, we will use the gapminder dataset that can be loaded directly
#if you’re connected to the internet.
# to download the data directly:
gapminder_orig <- read.csv("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder-FiveYearData.csv")
# define a copy of the original dataset that we will clean and play with 
gapminder <- gapminder_orig

library(tidyverse)

#ggplot2 is the data visualization package made by Hadley Wickham, and it is based 
#a set of principles called the layered grammar of graphics. 

#The basic idea is that a ggplot graphic layers geometric objects (circles, lines, etc),
#themes, and scales ontop of data. The form of the geometric object is defined by a 
#geom_xxx() function and the properties (position, size, colour) of the geometric 
#objects that are based on the data variables are specified by the aesthetic 
#(aes()) function (within the geom_xxx() function).

#The base layer of any ggplot graph is the empty ggplot layer defined by the 
#ggplot() function, which describes the data frame that the plot will be based on.
#I haven’t told ggplot what type of geometric object(s) I want yet, nor how the
#variables should be mapped to the geometric objects, so I just have a blank plot.

ggplot(gapminder)

#Since you now know about pipes, you could pipe in the data that you want to plot.
#Piping makes it easy to do intermediate manipulations that you don’t necessarily 
#want to save in the data frame itself, such as only plotting one year’s worth of data

gapminder %>% 
  filter(year == 2007) %>%
  ggplot()

##Adding geom layers

#Next, I will add a “geom” layer to our ggplot object. Layers are added to ggplot 
#objects using +, instead of %>%, since you are not explicitly piping into 
#each subsequent layer (we are actually adding a layer on top). The error 
#messages have recently been improved to warn you if you are accidentally using a 
#pipe %>% to add layers to ggplot objects (which, once you start piping everything 
#into everything, becomes an easy mistake to make).

#Probably the most common geom layer is geom_point. Inside geom_point(), you will 
#specify the aesthetic mappings from the variables to the geometric objects that 
#you want. For instance, if you want to plot a scatterplot with gdpPercap on the 
#x-axis and lifeExp on the y-axis, then you would add a geom_point() geometric
#layer with relevant aesthetic function: geom_point(aes(x = gdpPercap, y = lifeExp)).

# describe the base ggplot object and tell it what data we are interested in along
#with the aesthetic mapping
gapminder %>%
  filter(year == 2007) %>%
  ggplot() +
  # add a points layer on top
  geom_point(aes(x = gdpPercap, y = lifeExp))

#We could also add a smoothed trend line layer on top of the points using geom_smooth().

# describe the base ggplot object and tell it what data we are interested in along 
#with the aesthetic mapping
gapminder %>%
  filter(year == 2007) %>%
  ggplot() +
  # add a points layer on top
  geom_point(aes(x = gdpPercap, y = lifeExp)) +
  # add a smoothed LOESS layer
  geom_smooth(aes(x = gdpPercap, y = lifeExp), method = "loess")

#Note that since the aesthetics for geom_point() and geom_smooth() are the same, you 
#might want to just specify global aesthetics in the ggplot() function, rather than
#layer-specific aesthetics.

# describe the base ggplot object and tell it what data we are interested in along
#with the aesthetic mapping
gapminder %>%
  filter(year == 2007) %>%
  # specify global aesthetic mappings
  ggplot(aes(x = gdpPercap, y = lifeExp)) +
  # add a points layer on top
  geom_point() +
  # add a smoothed LOESS layer
  geom_smooth(method = "loess")

#We could also combine a points geom layer with a line geom layer, or any other type of 
#geom layer. Line plots work well for plotting time series, so below we plot the average 
#life expectancy over time using both point and line layers.

#Here you can do some clever combinations of dplyr manipulations with ggplot2 by 
#summarising life expectancy by year and piping the results into a ggplot without
#having to define any intermediate variables.

gapminder %>%
  # calculate the average life expectency for each year
  group_by(year) %>%
  summarise(avg_lifeExp = mean(lifeExp)) %>%
  ungroup() %>%
  # specify global aesthetic mappings
  ggplot(aes(x = year, y = avg_lifeExp)) +
  # add a points layer on top
  geom_point() +
  # add a line layer on top
  geom_line()

#If you wanted to have a separate line on our plot for each continent (rather than
#an aggregated line across all continents), you don’t need to add an individual layer
#for each continent to get the following plot.

#Instead, start by also grouping by continent when you calculate the average life 
#expectency by year.

gapminder %>%
  group_by(continent, year) %>%
  summarise(avg_lifeExp = mean(lifeExp))

#However if you try to use the same code as above to plot a line on the 
#country-year grouped data frame, you get a weird zig-zag pattern.

gapminder %>%
  group_by(continent, year) %>%
  summarise(avg_lifeExp = mean(lifeExp)) %>%
  ungroup() %>%
  ggplot() +
  # add a points layer on top
  geom_point(aes(x = year, y = avg_lifeExp)) +
  # add a lines layer ontop
  geom_line(aes(x = year, y = avg_lifeExp))

#This happens because you now have multiple average life expectancy values for each 
#year, but you haven’t specified which ones go together. To fix this plot, you need 
#to specify how the rows are grouped together (i.e. which variable defines the individual
#lines) by specifying the group = continent argument in the aes() function of the 
#geom_line() layer.

gapminder %>%
  group_by(continent, year) %>%
  summarise(avg_lifeExp = mean(lifeExp)) %>%
  ggplot() +
  # add a points layer on top
  geom_point(aes(x = year, y = avg_lifeExp)) +
  # add a lines layer on top that is grouped by continent
  geom_line(aes(x = year, y = avg_lifeExp, group = continent))

#More aesthetic mappings based on variables

#So far we have only specified the x- and y-position aesthetic mappings from the 
#data to the geom objects. But you can also specify other types of aesthetic mappings,
#such as using a variable to specify the colour of the points.

#If you want all of the points to be the same colour, you can specify a global point 
#colour argument (that lies outside the aes() function).

gapminder %>%
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp),
             col = "cornflowerblue")

#However, if you wanted to use a variable from the data frame to define the colour
#(or any other aesthetic feature) of the geoms, you will need to include it inside 
#the aes() function.

gapminder %>%
  ggplot() +
  geom_point(aes(x = gdpPercap, 
                 y = lifeExp, 
                 col  = continent))

#Note that the continent variable does not specify the colours themselves: this is 
#done automatically. You can specify the colours you want yourself by adding a scale
#layer for colour.

gapminder %>%
  ggplot() +
  geom_point(aes(x = gdpPercap, 
                 y = lifeExp, 
                 col  = continent)) +
  scale_colour_manual(values = c("orange", "red4", "purple", "darkgreen", "blue"))

#There are lots of types of scales that you can use for every type of aesthetic mapping
#(including x- and y-positions), and typically scales are specific to whether your 
#variable using in the aesthetic mapping is discrete or continuous.

#We could also add aesthetic mappings for other features such as shape, size, 
#transparency (alpha) and more. For example, changing the size based on population:
  
  gapminder %>%
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp, 
                 col = continent, size = pop),
             alpha = 0.5)

#There are lots of types of scales that you can use for every type of aesthetic 
#mapping (including x- and y-positions), and typically scales are specific to whether
#your variable using in the aesthetic mapping is discrete or continuous.
  
#We could also add aesthetic mappings for other features such as shape, size, 
#transparency (alpha) and more! For example, changing the size based on population:
    
gapminder %>%
ggplot() +
geom_point(aes(x = gdpPercap, y = lifeExp, 
              col = continent, size = pop),
               alpha = 0.5)  

#Other types of layers

#So far, we have only seen scatterplots (points) and line plots, however, there 
#are many other geoms you could add, including:
  
#Histograms-
#only require an x-aesthetic (the y-aesthetic is a count by default, but you can
#force it to be a density by specifying y = ..density..).

gapminder %>%
  ggplot() + 
  geom_histogram(aes(x = lifeExp), binwidth = 3)    

#Boxplots
#Automatically grouped by the x-aesthetic provided (e.g. continent in the plot below).
#To colour boxplots, use the fill argument instead of the col (or color/colour) 
#argument.

gapminder %>%
  ggplot() +
  geom_boxplot(aes(x = continent, y = lifeExp, fill = continent))

#Faceting
#You can create a grid (or “facet”) of plots separated by a categorical variable 
#of your choosing (e.g. continent) by adding a facet layer.

gapminder %>%
  ggplot() +
  geom_point(aes(x = gdpPercap, y = lifeExp)) +
  facet_wrap(~continent, ncol = 2)

#Lines
ggplot(gapminder, aes(x = year, y = lifeExp, group = country, color = continent)) +
  geom_line(alpha = 0.5)
