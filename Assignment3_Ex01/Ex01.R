
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
ts <- ts(jogging)


# Missing values
statsNA(ts[, 'HR'])
statsNA(ts[, 'Speed'])
statsNA(ts[, 'GT'])

# Summary
summary(ts)


# Plotting
colors <- c('HR'='#6495ED', 'Speed'='#20B2AA', 'GT'='#8968CD')
ggplot(jogging, aes(x=Time)) +
  geom_point(aes(y=HR, color='HR'), size=0.5) +
  geom_point(aes(y=Speed, color='Speed'), size=0.5) +
  geom_point(aes(y=GT, color='GT'), size=0.5) +
  labs(x='Time', y='Value', color='Measurement') +
  scale_color_manual(values=colors)

# Obvious change-points in the variables HR and speed?
# Dividing plots
# HR
colors <- c('HR'='#6495ED')
ggplot(jogging, aes(x=Time)) +
  geom_point(aes(y=HR, color='HR'), size=0.5) +
  labs(x='Time', y='Value', color='Measurement') +
  scale_color_manual(values=colors)

# Speed and GT
colors <- c('Speed'='#20B2AA', 'GT'='#8968CD')
ggplot(jogging, aes(x=Time)) +
  geom_point(aes(y=Speed, color='Speed'), size=0.5) +
  geom_point(aes(y=GT, color='GT'), size=0.5) +
  labs(x='Time', y='Value', color='Measurement') +
  scale_color_manual(values=colors)



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
posteriorHR <- posterior(modelHR, type='viterbi')


# Plotting 
# ------------------------------------------------------------------------------
# States
ggplot(posteriorHR, aes(x=as.numeric(row.names(posteriorHR)), 
                        y=state, colour = factor(state))) +
  geom_point(size=1.5) + 
  labs(x='Time', y='Value', color='State')

# Probabilities
colors <- c('S1'='#6495ED', 'S2'='#20B2AA', 'S3'='#8968CD')
ggplot(posteriorHR, aes(x = as.numeric(row.names(posteriorHR)))) +
  geom_point(aes(y=S1, color='S1'), size=1.5) +
  geom_point(aes(y=S2, color='S2'), size=1.5) +
  geom_point(aes(y=S3, color='S3'), size=1.5) +
  labs(x='Time', y='Value', color='StateProb') +
  scale_color_manual(values=colors)


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
posteriorSpeed <- posterior(modelSpeed, type='viterbi')


# Plotting 
# ------------------------------------------------------------------------------
# States
ggplot(posteriorSpeed, aes(x=as.numeric(row.names(posteriorSpeed)), 
                        y=state, colour=factor(state))) +
  geom_point(size=1.5) + 
  labs(x='Time', y='Value', color='State')

# Probabilities
colors <- c('S1'='#6495ED', 'S2'='#20B2AA', 'S3'='#8968CD')
ggplot(posteriorSpeed, aes(x = as.numeric(row.names(posteriorSpeed)))) +
  geom_point(aes(y=S1, color='S1'), size=1.5) +
  geom_point(aes(y=S2, color='S2'), size=1.5) +
  geom_point(aes(y=S3, color='S3'), size=1.5) +
  labs(x='Time', y='Value', color='StateProb') +
  scale_color_manual(values=colors)


# Task C
# ------------------------------------------------------------------------------
"
Comparing the estimated states with the ground truth using 
a confusion matrix. 
"

actual = factor(jogging$GT)
prediction1 = factor(posteriorHR$state)
prediction2 = factor(posteriorSpeed$state)

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
