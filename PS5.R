#Program taking some code and analyzing data using Regression Discontinuity
#Libraries and packages
library(foreign)
library(rdd)
library(dplyr)
install.packages('dplyr')

#Extracting deathrate data file and naming it data
data <- read.csv(file.choose(), header=TRUE)
data

#Summary statistics of agecell column and all column in data file
all <- data$all
age <- data$agecell
summary(all)
summary(age)

#Scatterplot comparing age, and death rates from all causes
plot(age, all, main="Scatterplot Comparing Age and Death Rates",xlab= "Age", ylab= "Death rate from all causes")

#Cleaned data, filtered records containing ages < 21
under21 <-filter(data, agecell < 21)
deathrate_under21 <- under21$all
cleaned_deathrate_under21 <- na.omit(deathrate_under21)
cleaned_deathrate_under21

#Cleaned data, filtered records containing ages > 21
over21 <-filter(data, agecell > 21)
deathrate_over21 <- over21$all
cleaned_deathrate_over21 <- na.omit(deathrate_over21)
cleaned_deathrate_over21

#Two-sampled T-test results to find whether or not a significant difference between
#The two means of the age groups exists
result <- t.test(cleaned_deathrate_under21,cleaned_deathrate_over21)
result

#Calculating the difference in means between the two age groups
diff_in_means <- mean(cleaned_deathrate_over21)-mean(cleaned_deathrate_under21)
diff_in_means

