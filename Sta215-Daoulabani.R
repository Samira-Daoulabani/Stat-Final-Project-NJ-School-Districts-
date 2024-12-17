## Project:  STAT 215, Fall 2024, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Zachary D. Kline

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Set Working Directory 
setwd("H:/sta215")

# Load data 
library(readr)
raw_data <- read_csv("raw_data.csv")
dataset <- na.omit(raw_data)
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
# Display the frequency distribution of the 'tax' variable in the dataset.
table(dataset$tax)
# Display the frequency distribution of the 'budget_cuts' variable in the dataset.
table(dataset$budget_cuts
# Display the frequency distribution of the 'funding' variable in the dataset.  
table(dataset$funding)

#The `summary()` function provides basic descriptive statistics for a numeric or factor variable.
summary(dataset$graduation)
#The `sd()` function calculates the standard deviation, a measure of the spread or dispersion of the data.
sd(dataset$graduation)

#The `summary()` function provides a quick statistical summary of the variable.
summary(dataset$population)
#The `sd()` function calculates the standard deviation of the variable.
sd(dataset$population)

#summary()` provides a statistical summary of the variable `homeless` in the dataset.
summary(dataset$homeless)
#`sd()` calculates the standard deviation of the `homeless` variable in the dataset.
sd(dataset$homeless)




##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
#This line creates a boxplot to visualize the distribution of the `graduation` variable
boxplot(graduation ~ budget_cuts, data = dataset)
#The `aov()` function fits an ANOVA model to the data, analyzing the effect of `budget_cuts` on `graduation`.
anova <- aov(graduation ~ budget_cuts, data = dataset)
#The `summary()` function provides a detailed output of the ANOVA results
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
# Plotting a scatter plot of 'population' vs. 'homeless' from the 'dataset'
plot(dataset$population,dataset$homeless)

meanx <- mean(dataset$homeless)
meany <- mean(dataset$population)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(homeless ~ population, data = dataset)
summary(linear_relationship)
abline(linear_relationship, col = "red")


##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset$population, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset$tax,dataset$funding)

chisq.test(table(dataset$tax,dataset$funding))
