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

max(list_consecutive_na)
library(lubridate)
leap = function(x){
  +   day(x) == 29 & month(x) == 2 
}

data$Date <- as.Date(data$Date)
data = data[!leap(data$Date), ]

# (B)
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
local_MSE <- mean((locf_solution_data$T.full - locf_solution_data$T.missing)^2)

global_MSE
local_MSE

# (C):

library(imputeTS)

# Moving avrage
ma_data <- 2

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

p1 / p2 / p3 /p4

#(D)
# based on the rms the best performing imputations are local mean replacement and KALMAN
# so i will use the local mean imputation amd KALMAN.


locf_solution_data

#numbers <- ts(locf_solution_data$T.full, start=2012, frequency = 10)

fit_stl <- stl(ts(locf_solution_data$T.full, frequency = 2),
           s.window = 365)
plot(fit_stl)

locf_stl <- stl(ts(locf_solution_data$T.full, start=2012, frequency = 10),
               s.window = 5,
               robust = TRUE)
plot(locf_stl)

locf_miss_stl <- stl(ts(locf_solution_data$T.missing, start=2012, frequency = 2),
                s.window = 5)
plot(locf_miss_stl)

KALMAN_stl <- stl(ts(kalman_data$T.missing, start=2012, frequency = 2),
                     s.window = 365)
plot(KALMAN_stl)

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

manuel_stl <- stl(ts(manuel_data$T.missing, frequency = 5),
                  s.window = 10)
plot(manuel_stl)

