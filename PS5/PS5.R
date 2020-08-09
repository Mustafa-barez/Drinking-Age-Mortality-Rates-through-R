#Analyzing Mortality Rates with Drinking Age using Rmarkdown
#Libraries and packages
library(foreign)
library(rdd)
library(dplyr)
library(broom)
library(tidyverse)
install.packages('dplyr')

#Extracting deathrate data file and naming it data
data <- read.csv("C:\deathrate", header=TRUE)
data

#Summary statistics of agecell column and all column in data file including: 
#Min, Max, Mean and Standard Deviation
all <- data$all
age <- data$agecell
summary(all)
summary(age)
sd(all)
sd(age)


age#Scatterplot comparing age, and death rates from all causes
plot(age, all, main="Scatterplot Comparing Age Against Death Rates",xlab= "Age", 
     ylab= "Death rate from all causes")

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

#Displays Scatterplot plot with Regression Discontinuity Design cut-off and trend lines 
data %>% 
        ggplot(aes(x = age+21,
                    y = all)) +
        geom_point(alpha = 2, 
                    na.rm = TRUE) +
        geom_smooth(data = data %>% filter(agecell < 21), 
                    method='lm',
                    color = "black") +
        geom_smooth(data = data %>% filter(agecell >= 21), 
                    method='lm',
                    color = "black") +
        theme_minimal() +
        labs(x = "Age",
             y = "Death Rates from all Causes (per 100,000)") +
        ggtitle("All Causes of Deaths per 100,000") +
        theme(plot.title = element_text(hjust = 0.5))

