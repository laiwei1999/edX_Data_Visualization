library(dslabs)
library(tidyverse)
library(ggplot2)
library(ggrepel)
data(heights)
data(gapminder)

#By showing the data, you provide viewers extra information about distributions
heights %>% ggplot(aes(sex, height)) + geom_point()
#Jitter is adding a small random shift to each point in order to minimize the number of overlapping points
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)
#Alpha blending is making points somewhat transparent, helping visualize the density of overlapping points
#Align plots vertically to see horizontal changes. Align plots horizontally to see vertical changes.

color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A4")
p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>% 
  ggplot(aes(x, y, color = col)) + geom_point(size = 5) 
p1 + scale_color_manual(values = color_blind_friendly_cols)

#Consider using a slope chart or Bland-Altman plot when comparing one variable at two different time points, especially for a small number of observations.
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
#Slope charts use angle to encode change
dat <- gapminder %>% 
  filter(year %in% c(2010,2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)
#The Bland-Altman plot (Tukey mean difference plot, MA plot) graphs the difference between conditions on the y-axis and the mean between conditions on the x-axis
dat %>% mutate(year = paste0("life_expectancy_", year)) %>% 
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>% 
  mutate(average = (life_expacyancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>% 
  ggplot(aes(average, difference, label = country)) + geom_point() + 
  geom_text_repel() + geom_abline(lty = 2) + 
  xlab("Average of 2010 and 2015") + ylab("Difference between 2015 and 2010")

#Use geom_line() to create slope charts
dat %>% mutate(location = ifelse(year == 2010,1,2),
               location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"), location + 0.22, location),
               hjust = ifelse(year == 2010,1,0)) %>% 
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(year, life_expectancy, group = country)) + 
  geom_line(aes(color = country), show.legend = FALSE) + 
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) + 
  xlab("") + ylab("Life Expectancy")

#Case study: Vaccines
data(us_contagious_diseases)
str(us_contagious_diseases)
#assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count/population*10000) %>%
  mutate(state = reorder(state, rate))
#plot disease rates per year in California
dat %>% filter(state == "California") %>%
  ggplot(aes(year, rate)) + geom_line() + 
  ylab("Cases per 10,000") + geom_vline(xintercept = 1963, col = "blue")

#The RColorBrewer package offers several color palettes.
library(RColorBrewer)
#Sequential color palettes are best suited for data that span from high to low.
display.brewer.all(type = "seq")
#Diverging color palettes are best suited for data that are centered and diverge towards high or low values.
display.brewer.all(type = "div")
dat %>% ggplot(aes(year, state, fill = rate)) +
#The geom_tile() geometry creates a grid of colored tiles.
  geom_tile(color = "grey50") + scale_x_continuous(expand = c(0,0)) + 
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans = "sqrt") + 
#Position and length are stronger cues than color for numeric values, but color can be appropriate sometimes.
  geom_vline(xintercept = 1963, col = "blue") + theme_minimal() + 
  theme(panel.grid = element_blank()) + ggtitle(the_disease) + 
  ylab("") + xlab("")

#compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
#make line plot of measles rate by year by state
dat %>% filter(!is.na(rate)) %>% ggplot() + 
  geom_line(aes(year, rate, group = state), color = "grey50", show.legend = F, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") + 
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) + 
  xlab("") + ylab("")
#Reduce the number of digits locally using round() or signif()