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
table(dataset$tax)
table(dataset$budget_cuts)
table(dataset$funding)


summary(dataset$graduation)
sd(dataset$graduation)


summary(dataset$population)
sd(dataset$population)

summary(dataset$homeless)
sd(dataset$homeless)




##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
boxplot(graduation ~ budget_cuts, data = dataset)
anova <- aov(graduation ~ budget_cuts, data = dataset)
summary(anova)
##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
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
