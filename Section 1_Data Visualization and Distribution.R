library(dslabs)
library(tidyverse)
data(murders)
#Categorical data: ordinal categorical data(have an inherent order) & non-ordinal categorical data
head(murders)
#Numerical data: continuous variables & discrete variables 

#For categorical variables, the distribution describes the proportions of each category
data(heights)
#Use prop.table() to convert a table of counts to a frequency table
prop.table(table(heights$sex))
#The cumulative distribution function (CDF) is a function that reports the proportion of data below a value a for all values of a: F(a) = Pr(x <= a)

#Define range of values spanning the dataset
a <- seq(min(heights$height), max(heights$height), length = 100)
#Computes prob. for a single value
cdf_function <- function(x) {
  mean(heights$height <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

#The smoothing function makes estimates of the true continuous trend of the data given the available sample of data points
#The degree of smoothness can be controlled by an argument in the plotting function

#Normal Distribution
#average <- sum(x)/length(x)
#SD <- sqrt(sum((x - average)^2)/length(x))

index <- heights$sex == "Male"
x <- heights$height[index]
#Built-in mean and sd functions
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)
#Calculate standard units
z <- scale(x)
#Calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)

#Given male heights y
y <- heights %>% filter(sex == "Male") %>% pull(height)
#The normal distribution has a mathematically defined CDF which can be computed in R with the function pnorm()
1 - pnorm(70.5, mean(y), sd(y))
#Plot distribution of exact heights in data
plot(prop.table(table(y)), xlab = "a = height in inches", ylab = "Pr(x = a)")
#Probabilities in actual data over length 1 ranges containing an integer
mean(y <= 68.5) - mean(y <= 67.5)
#Probabilities in normal approximation match well
pnorm(68.5, mean(y), sd(y)) - pnorm(67.5, mean(y), sd(y))
#Probabilities in actual data over other ranges don't match normal approx as well(discretization)
mean(y <= 70.9) - mean(y <= 70.1)
pnorm(70.9, mean(y), sd(y)) - pnorm(70.1, mean(y), sd(y))

#Quantiles are cutoff points that divide a dataset into intervals with set probabilities
#The qth quantile is the value at which q% of the observations are equal to or less than that value
p <- seq(0.01,0.99,0.01)
#Percentiles are the quantiles that divide a dataset into 100 intervals each with 1% probability
percentiles <- quantile(heights$height,p)
#Quartiles divide a dataset into 4 parts each with 25% probability
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
#The summary() function returns the minimum, quartiles and maximum of a vector
summary(heights$height)

#The qnorm() function gives the theoretical value of a quantile with probability p of observing a value equal to or less than that quantile value given a normal distribution with mean mu and standard deviation sigma
#qnorm(p,mu,sigma)
#use qnorm() to determine the theoretical quantiles of a dataset
theoretical_quantiles <- qnorm(p,mean = mean(y),sd= sd(y))

mean(y <= 69.5)
#Quantile-quantile plots, or QQ-plots, are used to check whether distributions are well-approximated by a normal distribution
observed_quantiles <- quantile(y,p)
#Make Q-Q plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)
#Percentiles are the quantiles obtained when defining p as 0.01, 0.02, ..., 0.99
#Five-number summary: range (ignoring outliers) and the quartiles (25th, 50th, 75th percentile)
#In a boxplot, the box is defined by the 25th and 75th percentiles and the median is a horizontal line through the box
