# (A)

ozone_data = read.csv(
  file="~/Documents/skole/dat320/oblig/oblin1/compulsory_1/data/ozone.csv")

# sliceing down to the right size
relevant_feature_names = colnames(ozone_data)[1:51]
ozone_data <- ozone_data[relevant_feature_names]

# typecast the date to datetime type
ozone_data[['Date']] <- as.Date(ozone_data$Date,
                                   format="%Y-%m-%d")

# plot
library(ggplot2)
library(patchwork)
p1 <- ggplot(ozone_data, aes(x=Date,
                            y=T.0)) +
  geom_line() +
  theme(legend.position = "top")

p2 <- ggplot(ozone_data, aes(x=Date,
                             y=WSR.0)) +
  geom_line() +
  theme(legend.position = "top")

p1 / p2

# We can not detect any trend in the data. But on the other hand, there are clear sesonality in
# both wind and temprature. However the shape of wind is a bit more grouped then Temprature.


#(B)
# We can se a spesefic duration of missing values, to solve this is not as simple.
# There are muliple stratergies. There are konvinient that the missing part is between
# a top and bottom point. witch is not the case for WSR. To methods wort trying, spline interpolation
# and some median based dayes of other years.

# inserting row at end
x <- seq(as.Date("1998-01-01"), as.Date("2004-12-31"), by="days")

#x <- seq(min(ozone_data$Date), max(ozone_data$Date), by="days")
missing_n = ozone_data$Date[!x %in% ozone_data$Date]  

# At this point we have figured out that there are missing days as well,
# this is a problem that sould be delt with first
# adding missing NA rows
for (i in 1:length(missing_n))
{
  n_rows <- nrow(ozone_data)
  ozone_data[n_rows+1,] <- NA
  ozone_data$Date[n_rows+1] = missing_n[i]
}

ozone_data <- ozone_data[seq(ozone_data$Date),]
ozone_data$Year <- as.integer(format(ozone_data$Date, "%Y"))
ozone_data$month <- as.integer(format(ozone_data$Date, "%m"))
ozone_data$day <- as.integer(format(ozone_data$Date, "%d"))


library(imputeTS)

ozone_data$T.0 <- na_interpolation(ozone_data$T.0, option = "linear")
#ozone_data$WSR.0 <- na_mean(ozone_data$WSR.0)
#mean_repl <- na_mean(ozone)
#for (val in 1:31)
#{
#  ozone_data[ozone_data$day == val, ] <- na_mean(ozone_data[ozone_data$day == val, ])
#}


for (val in 1:31)
{
  for (val2 in 1:12)
  {
    ozone_data[ozone_data$day == val & ozone_data$month == val2, ] <- na_mean(ozone_data[ozone_data$day == val & ozone_data$month == val2, ])
  }
}

p1 <- ggplot(ozone_data, aes(x=Date,
                             y=T.0)) +
  geom_line() +
  theme(legend.position = "top")

p2 <- ggplot(ozone_data, aes(x=Date,
                             y=WSR.0)) +
  geom_line() + 
  theme(legend.position = "top")

p1 / p2


library(lubridate)
leap = function(x){
  +   day(x) == 29 & month(x) == 2 
}

ozone_data = ozone_data[!leap(ozone_data$Date), ]
did_we_do_it = length(ozone_data[,1])/365

#(C): Reshape the data
library("stats")

M = rbind(c("WSR.0", "WSR.1","WSR.2", "WSR.3","WSR.4", "WSR.5", "WSR.6",
            "WSR.7", "WSR.8", "WSR.9", "WSR.10", "WSR.11", "WSR.12", "WSR.13",
            "WSR.14", "WSR.15", "WSR.16", "WSR.17", "WSR.18", "WSR.19", "WSR.20",
            "WSR.21", "WSR.22", "WSR.23"),
          c("T.0", "T.1", "T.2", "T.3", "T.4", "T.5", "T.6", "T.7", "T.8", "T.9",
            "T.10", "T.11", "T.12", "T.13", "T.14", "T.15", "T.16", "T.17", "T.18",
            "T.19", "T.20", "T.21", "T.22", "T.23"))
M


re = reshape(data = ozone_data,
        direction="long",
        varying = M,
        drop=c("WSR_PK", "WSR_AV", "Year", "month", "day", "id"))

# should be able to do this in fewer lines
re$Date <- as.POSIXct(re$Date)
re$Date <- as.POSIXlt(re$Date)
#re$time <- re$time - 1
re$Date$hour = re$time
re$Date <- as.POSIXct(re$Date)
length_of = length(re[,1])
for (i in 1:length_of) {re[i,]$Date - 1 * 60 * 60}
re$T.0 <- na_mean(re$T.0)
re$WSR.0 <- na_mean(re$WSR.0)
re <- re[seq(re$Date),]

#(D)
library(dplyr)
library(ggplot2)

summary = summarise(re)
re
data = re[c("Date", "WSR.0", "T.0")]
data

#summarise(data, mean())
data$year <-  as.integer(format(data$Date, "%Y"))

df = data %>% group_by(year) %>%
  summarise(standard_deviation = sd(T.0),
            average = mean(T.0)) %>%
  as.data.frame()
df = df[1:7,]

ggplot(df, aes(x = year,
              y = average)) +
  geom_line() +
  geom_errorbar(aes(ymin=average - standard_deviation, ymax = average + standard_deviation),
                width=.2)

df2 = data %>% group_by(year) %>%
  summarise(median_speed = median(WSR.0),
            max_speed = max(WSR.0),
            min_speed = min(WSR.0))
df2 = df2[1:7,]
ggplot(df2, aes(x = year,
                y = median_speed)) +
  geom_line() +
  geom_line(aes(x = year, y = max_speed)) +
  geom_line(aes(x = year, y = min_speed))
  
df2
