#Case Study: Trends in World Health and Economics

#We will use real data to answer the following questions about world health and economics:
#Is it still fair to consider the world as divided into the West and the developing world?
#Has income inequality across countries worsened over the last 40 years?
library(dslabs)
library(tidyverse)
data("gapminder")
head(gapminder)
#Compare infant mortality in Sri Lanka and Turkey
gapminder %>% filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>% 
  select(country, infant_mortality)

ds_theme_set()
#A scatterplot of life expectancy versus fertility rate in 1962 suggests that this viewpoint was grounded in reality 50 years ago
filter(gapminder, year == 1962) %>% 
  ggplot(aes(fertility, life_expectancy, color = continent)) + geom_point()

#Faceting makes multiple side-by-side plots stratified by some variable
filter(gapminder, year%in%c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) + 
  geom_point() + 
#The facet_grid() function allows faceting by up to two variables, with rows faceted by one variable and columns faceted by the other variable
  facet_grid(. ~ year)
#To facet by only one variable, use the dot operator as the other variable.

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
#The facet_wrap() function facets by one variable and automatically wraps the series of plots so they have readable dimensions
gapminder %>% filter(year %in% years & continent %in% continents) %>% 
  ggplot(aes(fertility, life_expectancy, col = continent)) + 
  geom_point() + facet_wrap(~ year)

#Time series plots have time on the x-axis and a variable of interest on the y-axis
gapminder %>% filter(country == "United States") %>% 
#The geom_line() geometry connects adjacent data points to form a continuous line
  ggplot(aes(year, fertility)) + geom_line()

countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>% 
#group or color by a variable so that the lines are plotted independently.
  ggplot(aes(year, fertility, col = country)) + geom_line()

labels <- data.frame(country = countries, x = c(1975,1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>% 
  ggplot(aes(year, life_expectancy, col = country)) + geom_line() + 
  geom_text(data = labels, aes(x, y, label = country), size = 5) + 
  theme(legend.position = "none")

#Add dollars per day variable
gapminder <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365)

past_year <- 1970

#Log transformations convert multiplicative changes into additive changes
gapminder %>% filter(year == past_year & !is.na(gdp)) %>% 
#Common transformations are the log base 2 transformation and the log base 10 transformation
  ggplot(aes(log2(dollars_per_day))) + 
  geom_histogram(binwidth = 1, color = "black") 

#Repeat histogram with log2 scaled x-axis
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
#The mode of a distribution is the value with the highest frequency
  ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + 
#Scale the x-axis using scale_x_continuous() or scale_x_log10() layers
  scale_x_continuous(trans = "log2")

#Number of regions
length(levels(gapminder$region))
#Make boxplots stratified by a categorical variable using the geom_boxplot() geometry
p <- gapminder %>% filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(region, dollars_per_day))
#Rotate axis labels by changing the theme through element_text()
p + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#The reorder() function changes the order of factor levels based on a related numeric vector
fac <- factor(c("Asia","Asia","West","West","West"))
levels(fac)
value <- c(10,11,12,6,4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)
#Reorder by median income and color by continent
gapminder %>% filter(year == past_year & !is.na(gdp)) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
  ggplot(aes(region, dollars_per_day, fill = continent)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#log2 scale y-axis
  xlab("") + scale_y_continuous(trans = "log2") + 
#Adding data points to the boxplot with a geom_point() layer
  geom_point(show.legend = FALSE)

#Comparing Distribution
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
#Facet by West vs devloping
gapminder %>% filter(year == past_year & !is.na(gdp)) %>% 
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% 
  ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + 
  scale_x_continuous(trans = "log2") + facet_grid(. ~ group)
#Facet by West/developing and year
present_year <- 2010

gapminder %>% filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>% 
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% 
  ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1, color = "black") + 
  scale_x_continuous(trans = "log2") + facet_grid(year ~ group)

#Define countries that have data available in both years
country_list_1 <- gapminder %>% 
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%  
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)
#Make histogram including only countries with data available in both years
gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% 
  ggplot(aes(dollars_per_day)) + 
  geom_histogram(binwidth = 1, color = "black") + 
  scale_x_continuous(trans = "log2") + facet_grid(year ~ group)

#Boxplots of income in West versus developing world, 1970 and 2010
gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% 
           ggplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("") + scale_y_continuous(trans = "log2") + 
  geom_boxplot(aes(region, dollars_per_day, fill = factor(year))) 

#Density Plots
gapminder %>% filter(year == past_year & country %in% country_list) %>% 
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>% 
  summarize(n = n()) %>%  knitr::kable()
#Change the y-axis of density plots to variable counts using ..count.. as the y argument
gapminder %>%  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% 
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) + 
  scale_x_continuous(trans = "log2") + geom_density(alpha = 0.2) + 
  facet_grid(year ~ .)
#The case_when() function defines a factor whose levels are defined by a variety of logical operations to group data
gapminder <- gapminder %>% mutate(group = case_when(
  .$region %in% west ~ "West", 
  .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
  .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
  .$continent == "Africa" & .$region != "North Africa" ~ "Sub-Saharan Africa",
  TRUE ~ "Others"))
#Reorder factor levels
gapminder <- gapminder %>% 
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
#Stacked density plot
gapminder %>%  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  ggplot(aes(dollars_per_day, fill = group)) + 
  scale_x_continuous(trans = "log2") + 
#Plot stacked density plots
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + 
  facet_grid(year ~ .)

#Define a weight aesthetic mapping to change the relative weights of density plots
gapminder %>% 
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  group_by(year) %>% 
  mutate(weight = population/sum(population*2)) %>% 
  ungroup() %>% 
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) + 
  scale_x_continuous(trans = "log2") + 
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + 
  facet_grid(year ~ .)

#Add cases
gapminder <- gapminder %>% mutate(group = case_when(
  .$region %in% west ~ "The West",
  .$region %in% "Northern Africa" ~ "Northern Africa",
  .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
  .$region == "Southern Asia" ~ "Southern Asia",
  .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
  .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
  .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacfic Islands"))
#Define a data frame with group average income and average infant survival rate
surv_income <- gapminder %>% 
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>% 
  group_by(group) %>% 
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population)) 
surv_income %>%  arrange(income)
#The breaks argument allows us to set the location of the axis labels and tick marks
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) + 
  scale_x_continuous(trans = "log2", limit= c(0.25, 150)) + 
  scale_y_continuous(trans = "logit", limit = c(0.875, 0.9981),
                     breaks = c(0.85, 0.9, 0.95, 0.99, 0.995, 0.998)) +
  geom_label(size = 3, show.legend = FALSE)
#The logistic or logit transformation is defined as f(p) = log(p/(1-p)), or the log of odds
#The ecological fallacy is assuming that conclusions made from the average of a group apply to all members of that group