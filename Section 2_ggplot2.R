#Needed Library 
library(ggplot2)
library(dslabs)
library(dplyr)
library(ggthemes)
library(ggrepel)
library(tidyverse)
library(gridExtra)
#Load data
data(murders)
data(heights)
#Aesthetic mappings describe how properties of the data connect with features of the graph
ggplot(data = murders) + geom_point(aes(x = population/10^6, y = total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)
#Add points layer to predefined ggplot object
P <- ggplot(data = murders, aes(population/10^6, total, label = abb))
#Local aesthetics override global aesthetics
P + geom_point(size = 3) + geom_text(aes(x = 10, y = 800, label = "Hello there"))
#Convert the x-axis to log scale with scale_x_continuous
P + geom_point(size = 3) + geom_text(nudge_x = 0.05) + 
  scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") 

#Define average murder rate
r <- murders %>% summarize(rate = sum(total)/sum(population) * 10^6) %>% pull(rate)

#Add a line with the geom_abline() geometry.Change the color with col or color and line type with lty
P + geom_abline(intercept = log10(r),lty = 2, color = "darkgrey") +
  #Add a color mapping that colors points by a variable by defining the col argument within aes()
  geom_point(size = 3, aes(color = region)) + geom_text(nudge_x = 0.075) + 
  geom_text_repel() + 
#To overlay points on the line, place the line layer before the point layer.
  scale_x_log10() + scale_y_log10() + 
#Add axis titles with xlab() and ylab() functions
  xlab("Population in millions (log scale)") +ylab("Total number of murders (log scale)") + 
#Add a plot title with the ggtitle() function
  ggtitle("US Gun Murders in 2020") +
#Change the legend title with scale_color_discrete().
  scale_color_discrete(name = "Region") + 
#Style of the Economist magazine
  theme_economist() 

#The style of a ggplot graph can be changed using the theme() function
ds_theme_set()

#Define P
P <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
#geom_histogram() creates a histogram, use the binwidth argument to change the width of bins
P + geom_histogram(binwidth = 1, fill = "blue", col = "black") + 
  xlab("Male heihgts in inches") + ggtitle("Histogram")
#geom_density() creates smooth density plots
P + geom_density()

#geom_qq() creates a quantile-quantile plot
heights %>% filter(sex == "Male") %>% ggplot(aes(sample = height)) + geom_qq()
#QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>% filter(sex == "Male") %>% summarize(mean = mean(height), sd = sd(height))
#By default, the data are compared to a standard normal distribution with a mean of 0 and standard deviation of 1.
heights %>% filter(sex == "Male") %>% ggplot(aes(sample = height)) + geom_qq(dparams = params) + geom_abline()
#Plots can be arranged adjacent to each other using the grid.arrange() function from the gridExtra package.
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")
#Pass the plot objects to grid.arrange()
grid.arrange(p1,p2,p3, ncol = 3)
