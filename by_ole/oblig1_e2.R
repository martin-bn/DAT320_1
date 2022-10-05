# (A)
data <- read.csv(
  file="~/Documents/skole/dat320/oblig/oblin1/compulsory_1/data/temperature.csv")

#install.packages("naniar")
library(naniar)
vis_miss(data)
# The plot shows the placement of missing values.
# - total 701 NA values
# - longest list of consecutive NA are 401

sum_of_na = sum(is.na(data$T.missing))
list_consecutive_na = c(0)
consecutive_na = 0
for (i in 1:length(data[,1])) {
  if (is.na(data[i, 3])) {
    consecutive_na = consecutive_na + 1
    if(!is.na(data[i + 1, 3])) {
      list_consecutive_na = append(list_consecutive_na, c(consecutive_na))
      consecutive_na = 0
    }
  } 
}

print(list_consecutive_na)
# lagest consecutive number of missing values are 401 and is the last block of 
# missing values. The missing values appear irregular. Most of the missing data 
# singler enteries of 1 og 2 consecutive.

# Remove type caste date remove 29 of february
max(list_consecutive_na)
library(lubridate)
leap = function(x){
  +   day(x) == 29 & month(x) == 2 
}

data$Date <- as.Date(data$Date)
data = data[!leap(data$Date), ]



# (B)
# Impute missing values by local and global mean.

library(imputeTS)
library(dplyr)
gobal_solution_data <- data
gobal_solution_data$T.missing <- na_mean(gobal_solution_data$T.missing)

locf_solution_data <- data
locf_solution_data$T.missing <- na_locf(locf_solution_data$T.missing)
vis_miss(locf_solution_data)

colnames(data)


library(ggplot2)
library(patchwork)
p1 <- ggplot(gobal_solution_data, aes(x=Date,
                             y=T.missing)) +
  geom_line(color="purple", alpha=0.5) +
  geom_line(aes(x=Date, y=T.full), alpha=0.5) 

p2 <- ggplot(locf_solution_data, aes(x=Date,
                             y=T.missing)) +
  geom_line(color="purple", alpha=0.5) +
  geom_line(aes(x=Date, y=T.full), alpha=0.5) 
p1 / p2

# RMSE
# By visualization and MSE we can see that local replacement is more acurate then global.
global_MSE <- mean((gobal_solution_data$T.full - gobal_solution_data$T.missing)^2)
locf_MSE <- mean((locf_solution_data$T.full - locf_solution_data$T.missing)^2)

global_MSE
locf_MSE

# (C):

# There are some clear problem when we interpolation the larger group of values.
# 1. most of the methods are noe suited to cover longer segments of missing values.
# 2. spline is prone to error caused by the direction from the two segments which 
# we try to connect. Therefor spline perform the worst.

# Result:
# By comparing plots and MSE there are one solution that perform as we want, and that is
# seasonal imputation by the function: na_seasplit. by trial and error we find out that 
# the function is not suited to find the right seasonality, this can be fixed by 
# transforming the feature vector into a time series object and specify the right 
# FQ for the series.


library(imputeTS)

# Linear imputation 
LI_data <- data
LI_data$T.missing <- na_interpolation(LI_data$T.missing, option = "linear")

p1 <- ggplot(LI_data, aes(x=Date,
                          y=T.missing)) +
  geom_line(color="purple", alpha=0.5) +
  geom_line(aes(x=Date, y=T.full), alpha=0.5) 

linear_MSE <- mean((LI_data$T.full - LI_data$T.missing)^2)
linear_MSE

# Spinel imputation

SPINEL_data <- data
SPINEL_data$T.missing <- na_interpolation(SPINEL_data$T.missing, option = "spline")

p2 <- ggplot(SPINEL_data, aes(x=Date,
                              y=T.missing)) +
  geom_line(color="purple", alpha=0.5) +
  geom_line(aes(x=Date, y=T.full), alpha=0.5) 

SPINEL_MSE <- mean((SPINEL_data$T.full - SPINEL_data$T.missing)^2)
SPINEL_MSE


# Stine imputation
Stine_data <- data
Stine_data$T.missing <- na_interpolation(Stine_data$T.missing, option = "stine")

p3<- ggplot(Stine_data, aes(x=Date,
                              y=T.missing)) +
  geom_line(color="purple", alpha=0.5) +
  geom_line(aes(x=Date, y=T.full), alpha=0.5) 

STINE_MSE <- mean((Stine_data$T.full - Stine_data$T.missing)^2)
STINE_MSE


# kalman
kalman_data <- data
kalman_data$T.missing <- na_kalman(kalman_data$T.missing)

p4 <- ggplot(kalman_data, aes(x=Date, y=T.missing)) +
  geom_line(color="purple", alpha=0.5) +
  geom_line(aes(x=Date, y=T.full), alpha=0.5) 

KALMAN_MSE <- mean((kalman_data$T.full - kalman_data$T.missing)^2)
KALMAN_MSE

# seasplit interpolation
seasplit_data <- data
seasplit_data$T.missing <- ts(seasplit_data$T.missing, start=2012, frequency = 365)
seasplit_data$T.missing <- na_seasplit(seasplit_data$T.missing,
                                       algorithm = "interpolation",
                                       find_frequency = FALSE)

p5 <- ggplot(seasplit_data,
             aes(x=Date, y=T.missing)) +
  geom_line(color="purple", alpha=0.5) +
  geom_line(aes(x=Date, y=T.full), alpha=0.5)

SEASSPLIT_MSE <- mean((seasplit_data$T.full - seasplit_data$T.missing)^2)
SEASSPLIT_MSE

p1 / p2 / p3 /p4 /p5

#(D)
# Results form B and C we chose na_seasplit and na_locf
locf_solution_data
seasplit_data

# na_locf
locf.full.stl.w5 <- stl(ts(locf_solution_data$T.full, frequency = 365),
                     s.window = 5)
plot(locf.full.stl.w5)

locf.full.stl.wp <- stl(ts(locf_solution_data$T.full, frequency = 365),
                     s.window = "periodic")
plot(locf.full.stl.wp)

locf.missing.stl.w5 <- stl(ts(locf_solution_data$T.missing, frequency = 365),
                        s.window = 5)
plot(locf.missing.stl.w5)

locf.missing.stl.wp <- stl(ts(locf_solution_data$T.missing, frequency = 365),
                        s.window = "periodic")
plot(locf.missing.stl.wp)

# na_seasplit

seasplit.full.stl.w5 <- stl(ts(seasplit_data$T.full, frequency = 365),
                        s.window = 5)
plot(seasplit.full.stl.w5)

seasplit.full.stl.wp <- stl(ts(seasplit_data$T.full, frequency = 365),
                        s.window = "periodic")
plot(seasplit.full.stl.wp)

seasplit.missing.stl.wp <- stl(ts(seasplit_data$T.missing, frequency = 365),
                           s.window = 5)
plot(seasplit.missing.stl.w5)

seasplit.missing.stl.wp <- stl(ts(seasplit_data$T.missing, frequency = 365),
                           s.window = "periodic")
plot(seasplit.missing.stl.wp)



#(D)
manuel_data <- data

manuel_data$month <- as.integer(format(manuel_data$Date, "%m"))
manuel_data$day <- as.integer(format(manuel_data$Date, "%d"))

for (val in 1:31)
{
  for (val2 in 1:12)
    {
    manuel_data$T.missing[manuel_data$day == val & manuel_data$month == val2] <- na_mean(manuel_data$T.missing[manuel_data$day == val & manuel_data$month == val2])
    }
}

p_manuel <- ggplot(manuel_data, aes(x=Date, y=T.missing)) +
  geom_line(color="purple", alpha=0.5) +
  geom_line(aes(x=Date, y=T.full), alpha=0.5) 
p_manuel

manuel_MSE <- mean((manuel_data$T.full - manuel_data$T.missing)^2)
manuel_MSE


maunel.full.stl.w5 <- stl(ts(manuel_data$T.full, frequency = 365),
                            s.window = 5)
plot(maunel.full.stl.w5)

maunel.full.stl.wp <- stl(ts(manuel_data$T.full, frequency = 365),
                            s.window = "periodic")
plot(maunel.full.stl.wp)

maunel.missing.stl.w5 <- stl(ts(manuel_data$T.missing, frequency = 365),
                               s.window = 5)
plot(maunel.full.stl.w5)

maunel.missing.stl.wp <- stl(ts(manuel_data$T.missing, frequency = 365),
                               s.window = "periodic")
plot(maunel.missing.stl.wp)

# Results
# by compering na_seasplit and our implementation we can se that they both follow the 
# the full data, but with some difference in error. 
p5 / p_manuel

# error between manuel and na_seasplit
SEASSPLIT_MSE
manuel_MSE
diff_MSE <- SEASSPLIT_MSE - manuel_MSE
diff_MSE
