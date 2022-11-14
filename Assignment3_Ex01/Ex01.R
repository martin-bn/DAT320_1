
# Exercise 1
# ------------------------------------------------------------------------------
"
Hidden Markov Models.
Jogging dataset: 2866 time points. 
HR: heart rate over time.
Speed: heart speed over time.
GT: discrete ground truth speed level.
"

# Loading necessary packages
#-------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(reshape2)
library(tseries)
library(forecast)
library(tidyr)
library(imputeTS)
library(depmixS4)
library(caret)


# Task 1
# ------------------------------------------------------------------------------

# Loading data
jogging <- read.csv('VET/Courses/DAT320/Compulsary/Assignment3/data/jogging.csv')
column_names <- colnames(jogging)

# Missing values
statsNA(jogging$HR)
statsNA(jogging$Speed)
statsNA(jogging$GT)

# Summary
summary(jogging)

# Unique values for GT
unique(jogging$GT)


# Plotting
# HR
plot_hr <- ggplot(jogging, aes(x=Time, y=HR, color=GT)) +
  geom_line()

plot(plot_hr)

# Speed
plot_speed <- ggplot(jogging, aes(x=Time, y=Speed, color=GT)) +
  geom_line()

plot(plot_speed)

"
Any obvious change points, suggesting a transition from one 
underlying state to the next?

From HR plot: Hard to point out specific time points, but it can be
considered that the local minimum- and maximum values in the graph
represent different underlying states. 
From Speed plot: Clustering of measurements around what could be 
considered different underlying states. Change points would in this case
be points where the value either rapidly increases or decreases. 
"


# Task 2
# ------------------------------------------------------------------------------
# Gaussian Hidden Markov Models

# HR model
# ------------------------------------------------------------------------------
set.seed(1)
modelHR <- depmix(HR ~ 1, data=jogging, nstates=3,
                  family=gaussian())

# Parameter estimation
modelHR <- depmixS4::fit(modelHR)

# Summary of model
summary(modelHR)

# Estimated states and probabilities
stateHR <- posterior(modelHR, type='viterbi')

# Adding estimated states to dataframe
jogging$modelHR <- stateHR$state


# Plotting 
# ------------------------------------------------------------------------------
# States
hr <- ggplot(jogging, aes(x=Time, y=HR, color=modelHR)) +
  geom_line()
plot(hr)



# Speed model
# ------------------------------------------------------------------------------
set.seed(1)
modelSpeed <- depmix(Speed ~ 1, data=jogging, nstates=3,
                     familiy=gaussian())

# Parameter estimation
modelSpeed <- depmixS4::fit(modelSpeed)

# Summary of model
summary(modelSpeed)

# Estimated states and probabilities
stateSpeed <- posterior(modelSpeed, type='viterbi')

# Adding estimated states to dataframe
jogging$modelSpeed <- stateSpeed$state


# Plotting 
# ------------------------------------------------------------------------------
# States
speed <- ggplot(jogging, aes(x=Time, y=Speed, color=modelSpeed)) +
  geom_line()
plot(speed)



# Task C
# ------------------------------------------------------------------------------
"
Comparing the estimated states with the ground truth using 
a confusion matrix. 
"

actual = factor(jogging$GT)
prediction1 = factor(jogging$modelHR)
prediction2 = factor(jogging$modelSpeed)

# HR
matrixHR <- confusionMatrix(data=prediction1, reference=actual)
matrixHR

# Speed
matrixSpeed <- confusionMatrix(data=prediction2, reference=actual)
matrixSpeed

# Task D
# ------------------------------------------------------------------------------
"
Interpret the results. Are the model assumptions for Hidden Markov Models 
fulfilled in the two scenarios? 
"

"
For the HR model: Best at estimating correctly for state 2, and 0 Specificity 
for state 3. 
Speed model: Lower overall accuracy compared to the HR model. Almost all the 
state 1 cases is correctly classified, but much lower correct estimations 
for the other states. 
"
