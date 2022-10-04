# Exercise 1

####
# Oppgave 1
####


##
# a)
##

# Last inn data
library(readr)
ozone <- read_csv("/Users/martinbergsholmnesse/Documents/NMBU/DAT320/ozone.csv")

# Print ut kolonnenavn
colnames(ozone)

# Slett kolonner
ozone = subset(ozone, select = -c(WSR_PK, WSR_AV, T_PK, T_AV, T85, RH85, U85, V85, 
                                  HT85, T70, RH70, U70, V70, HT70, T50, RH50, U50, 
                                  V50, HT50, KI, TT, SLP, SLP_, Precp, response))
# Sjekk at datasettet ser ok ut
View(ozone)

# Konverter Date til Dato-format
library(dplyr)
library(magrittr)
mutate(ozone, Date= as.Date(Date, format= "%d.%m.%Y"))

# Plot T.0 
library(ggplot2)

ggplot(data = ozone ,
       aes(x = Date , y =
             T.0)) + geom_point()
# Plot WSR.0
ggplot(data = ozone ,
       aes(x = Date , y =
             WSR.0)) + geom_point()
###
# Comments on the graphs. Very interesting comments!
###

# https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5

###
# b)
###

# Antall missing values
sum(is.na(ozone))

# Date er fra 01.01.1998 - 31.12.2004
# Skal ha 2555 rader etter fjernet skuddår
# Legg til missing dates
library(tidyr)

ozone = ozone %>%
  mutate(Date = as.Date(Date)) %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day"))

# Remove leap year dates
ozone = ozone[!(format(ozone$Date,"%m") == "02" & format(ozone$Date, "%d") == "29"), ,drop = FALSE]

# Check if number of rows is correct. It is.
nrow(ozone)


###
# c)
###
library(stats)

WSR = subset(ozone, select = c("WSR.0", "WSR.1", "WSR.2", "WSR.3", "WSR.4", "WSR.5", "WSR.6", "WSR.7", "WSR.8", "WSR.9", "WSR.10", "WSR.11", "WSR.12", "WSR.13", "WSR.14", "WSR.15", "WSR.16", "WSR.17", "WSR.18", "WSR.19", "WSR.20", "WSR.21", "WSR.22", "WSR.23")) 

TS = subset(ozone, select = c("T.1", "T.2", "T.3", "T.4", "T.5", "T.6", "T.7", "T.8", "T.9", "T.10", "T.11", "T.12", "T.13", "T.14", "T.15", "T.16", "T.17", "T.18", "T.19", "T.20", "T.21", "T.22", "T.23"))
tid = c("1", "2", "3", "4", "5", "6",
        "7", "8", "9", "10", "11", "12",
        "13", "14", "15", "16", "17", "18",
        "19", "20", "21", "22", "23", "24")

# Liste over kolonnenavn
names = colnames(ozone)

ozone <- as.data.frame(ozone)

ozone_long = reshape(ozone, varying = list(grepl(colnames(ozone), pattern = "T"),
                              grepl(colnames(ozone), pattern = "WSR")), 
        direction = "long",
        v.names = c("T", "WSR"),
        timevar = "Time",
        times = 0:23) %>%
  arrange(Date, Time)

library(lubridate)

ozone_long = mutate(ozone_long, DateTime = ymd_h(paste(Date, Time)))

View(ozone_long)




###
# d)
###

# VIRKER IKKE LENGER. FUNKA ISTAD!




# Yearly average temperatures
ozone_long %>%
  group_by(year(Date)) %>%
  summarize(median_WSR = median(WSR, na.rm = TRUE))


# Yearly median wind
ozone_long %>%
  group_by(year(Date)) %>%
  summarize(median_WSR = median(WSR, na.rm = TRUE),
            max_wind = max(WSR, na.rm = TRUE),
            min_wind = min(WSR, na.rm = TRUE))




#library(tidyverse)
#library(plyr)  

# Format into data frame
yat <- as.data.frame(yat)
# Rename date
yat =plyr::rename(yat, c("year(Date)" = "Date"))
# Format date
mutate(yat, Date= as.Date(Date, format= "%Y"))
# Plot Average temp and year
ggplot(data = yat,
       aes(x = Date , y =T_avg)) + geom_line()


# Format into data frame
ymw <- as.data.frame(ymw)
# Rename date
ymw =plyr::rename(ymw, c("year(Date)" = "Date"))


View(ymv)

