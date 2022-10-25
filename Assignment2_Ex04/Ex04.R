
# Loading packages
# ------------------------------------------------------------------------------
library(tseries)
library(forecast)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(imputeTS)


# Exercise 4 - Stationarity in ARIMA models
# ------------------------------------------------------------------------------
# Task A
# ------------------------------------------------------------------------------

# Function for simulation
"
Implementation of the formulas given in the assignment.
Inputs to the function are a list containing the two model parameters,
the name of the model to simulate and a value for the seed.
The function return the list of values for the model specified in the
input, after cutting the first 100 values. 
"

ts_sim <- function(params, model, seed){
  set.seed(seed=seed)
  t_max <- 1000
  n <- t_max + 100
  sigma <- 0.1
  e <- rnorm(n=n, mean=0, sd=sigma)
  k <- 2
  ar <- 0
  ma <- 0
  p <- params
  for(t in 1:length(e)){
    if(t <= k){
      ar[t] <- e[t]
      ma[t] <- e[t]
    }
    else{
      ar[t] <- p[1]*ar[t-1] + p[2]*ar[t-2] + e[t]
      ma[t] <- p[1]*e[t-1] + p[2]*e[t-2] + e[t]
    }
  }
  ar <- ar[101:length(ar)]
  ma <- ma[101:length(ma)]
  if(model=='AR'){
    return(ar)
  } else if(model=='MA'){
    return(ma)
  }
}


# Simulations
# ------------------------------------------------------------------------------
# Model 1
model1 <- list()
params <- c(0.6, -0.3)
seeds <- c(2, 7, 9)

for(i in 1:3){
  seed <- seeds[i]
  sim <- ts_sim(params, 'AR', seed)
  model1 <- append(model1, list(sim))
}


# Model 2
model2 <- list()
params <- c(0.8, 0.2)
seeds <- c(2, 7, 9)

for(i in 1:3){
  seed <- seeds[i]
  sim <- ts_sim(params, 'AR', seed)
  model2 <- append(model2, list(sim))
}


# Model 3
model3 <- list()
params <- c(0.6, -0.3)
seeds <- c(2, 7, 9)

for(i in 1:3){
  seed <- seeds[i]
  sim <- ts_sim(params, 'MA', seed)
  model3 <- append(model3, list(sim))
}


# Model 4
model4 <- list()
params <- c(0.8, 0.2)
seeds <- c(2, 7, 9)

for(i in 1:3){
  seed <- seeds[i]
  sim <- ts_sim(params, 'MA', seed)
  model4 <- append(model4, list(sim))
}


# Which of the parameter combinations describe proper
# AR(2) or MA(2) models?


# Task 2
# ------------------------------------------------------------------------------
# Plotting simulated time series, ACF, PACF and KPSS test
# ------------------------------------------------------------------------------
# Model 1
# ------------------------------------------------------------------------------
# Creating data.frame
index1 <- c(1:length(model1[[1]]))
df1 <- data.frame(index1, model1[[1]], model1[[2]], model1[[3]])
names(df1) <- c('index', 'simulation1', 'simulation2', 'simulation3')

# Reshaping
df1_long <- gather(df1, simulation, value, 
                   simulation1:simulation3, factor_key=TRUE)

# Subplots of each simulation
ggplot(data = df1_long) +
  geom_line(mapping = aes(x = index, y = value), color = '#009ACD') +
  labs(x='', y='') + 
  facet_wrap(~ simulation) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  ggtitle('Model 1')


# ACF, PACF and KPSS test
p_list1 = list()
for(i in 1:3){
  ts <- model1[[i]]
  acf(ts)
  pacf(ts)
  kpss <- kpss.test(ts)
  p <- kpss['p.value']
  p_list1[i] <- p
}
 vb

# Model 2
# ------------------------------------------------------------------------------
# Creating data.frame
index2 <- c(1:length(model2[[1]]))
df2 <- data.frame(index2, model2[[1]], model2[[2]], model2[[3]])
names(df2) <- c('index', 'simulation1', 'simulation2', 'simulation3')
nm
# Reshaping
df2_long <- gather(df2, simulation, value, 
                   simulation1:simulation3, factor_key=TRUE)
# Subplots of each simulation

ggplot(data = df2_long) +
  geom_line(mapping = aes(x = index, y = value), color = '#009ACD') +
  labs(x='', y='') + 
  facet_wrap(~ simulation) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  ggtitle('Model 2')


# ACF, PACF and KPSS test
p_list2 = list()
for(i in 1:3){
  ts <- model2[[i]]
  acf(ts)
  pacf(ts)
  kpss <- kpss.test(ts)
  p <- kpss['p.value']
  p_list2[i] <- p
}


# Model 3
# ------------------------------------------------------------------------------
# Creating data.frame
index3 <- c(1:length(model3[[1]]))
df3 <- data.frame(index3, model3[[1]], model3[[2]], model3[[3]])
names(df3) <- c('index', 'simulation1', 'simulation2', 'simulation3')

# Reshaping
df3_long <- gather(df3, simulation, value, 
                   simulation1:simulation3, factor_key=TRUE)
# Subplots of each simulation

ggplot(data = df3_long) +
  geom_line(mapping = aes(x = index, y = value), color = '#009ACD') +
  labs(x='', y='') + 
  facet_wrap(~ simulation) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  ggtitle('Model 3')

# ACF, PACF and KPSS test
p_list3 = list()
for(i in 1:3){
  ts <- model3[[i]]
  acf(ts)
  pacf(ts)
  kpss <- kpss.test(ts)
  p <- kpss['p.value']
  p_list3[i] <- p
}


# Model 4
# ------------------------------------------------------------------------------
# Creating data.frame
index4 <- c(1:length(model4[[1]]))
df4 <- data.frame(index4, model4[[1]], model4[[2]], model4[[3]])
names(df4) <- c('index', 'simulation1', 'simulation2', 'simulation3')

# Reshaping
df4_long <- gather(df4, simulation, value, simulation1:simulation3, factor_key=TRUE)
# Subplots of each simulation

ggplot(data = df4_long) +
  geom_line(mapping = aes(x = index, y = value), color = '#009ACD') +
  labs(x='', y='') + 
  facet_wrap(~ simulation) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  ggtitle('Model 4')


# ACF, PACF and KPSS test
p_list4 = list()
for(i in 1:3){
  ts <- model4[[i]]
  acf(ts)
  pacf(ts)
  kpss <- kpss.test(ts)
  p <- kpss['p.value']
  p_list4[i] <- p
}



"
From the plots of the time series, the ACFs, PACFs and KPSS-test, 
model 1, 3 and 4 are stationary time series. All the mentioned 
models have a p-value greater than 0.05, meaning that they are 
trend stationary.
Model 2 has a clear trend in the ACF and also a p-value less than 0.05, 
meaning it is not trend stationary. 
"


# Task C
# ------------------------------------------------------------------------------
# Fit parameters using auto.arima
# Model 1
model1_parameters = list()
for(i in 1:3){
  ar <- auto.arima(model1[[i]])
  coeff <- ar['coef']
  model1_parameters[i] <- coeff
}
# 0.605, -0.292

# Model 2
model2_parameters = list()
for(i in 1:3){
  ar <- auto.arima(model2[[i]])
  coeff <- ar['coef']
  model2_parameters[i] <- coeff
}
# -0.199

# Model 3
model3_parameters = list()
for(i in 1:3){
  ar <- auto.arima(model3[[i]])
  coeff <- ar['coef']
  model3_parameters[i] <- coeff
}
# 0.608, -0.285

# Model 4
model4_parameters = list()
for(i in 1:3){
  ar <- auto.arima(model4[[i]])
  coeff <- ar['coef']
  model4_parameters[i] <- coeff
}
# 0.804, 0.221

"
Although the estimated model parameters vary some for the three
simulations done for each model, the estimated parameters for the 
stationary models 1, 3 and 4, are very close to the actual parameters.
The estimated model parameters for model 2 are very different from 
the actual parameters. 
"

# Task D
# ------------------------------------------------------------------------------
# Show that first-order differences lead to a proper AR(2) model
# (fulfill the side conditions)
