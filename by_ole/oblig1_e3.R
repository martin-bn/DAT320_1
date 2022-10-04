# (A)

df <- read.csv("~/Documents/skole/dat320/oblig/oblin1/compulsory_1/data/covid.csv")
column_names <- colnames(df)
iso_codes <- c("SWE", "DNK", "NOR", "GBR", "ITA", "IND")
time_interval <- seq(as.Date("2020-03-16"), as.Date("2022-01-01"), by="days")

df$date <- as.Date(df$date)

df <- df[df$iso_code %in% iso_codes, c("date", "iso_code", "new_cases_per_million")]
df

data_wide <- reshape(data = df,
                     idvar = "date",
                     timevar = "iso_code",
                     v.names = "new_cases_per_million",
                     direction = "wide")
#missing_n = ozone_data$Date[!x %in% ozone_data$Date]  
missing_dates <- time_interval[! time_interval %in% df$date ]
missing_dates
# There seems to be no missing days.

data_wide <- data_wide[order(data_wide$date),]
data_wide <- data_wide[data_wide$date %in% time_interval, ]

# Finding missing values
sum_of_na_SWE = sum(is.na(data_wide$new_cases_per_million.SWE))
sum_of_na_DNK = sum(is.na(data_wide$new_cases_per_million.DNK))
sum_of_na_NOR = sum(is.na(data_wide$new_cases_per_million.NOR))
sum_of_na_GBR = sum(is.na(data_wide$new_cases_per_million.GBR))
sum_of_na_ITA = sum(is.na(data_wide$new_cases_per_million.ITA))
sum_of_na_IND = sum(is.na(data_wide$new_cases_per_million.IND))

library(imputeTS)

p_SWE <- ggplot_na_distribution(data_wide$new_cases_per_million.SWE,
                                color_missing="red")


p_DNK <- ggplot_na_distribution(data_wide$new_cases_per_million.DNK,
                                color_missing="red")

p_NOR <- ggplot_na_distribution(data_wide$new_cases_per_million.NOR,
                                color_missing="red")

p_GBR <- ggplot_na_distribution(data_wide$new_cases_per_million.GBR,
                                color_missing="red")

p_ITA <- ggplot_na_distribution(data_wide$new_cases_per_million.ITA,
                                color_missing="red")

p_IND <- ggplot_na_distribution(data_wide$new_cases_per_million.IND,
                                color_missing="red")
(p_SWE + p_DNK) / (p_NOR + p_GBR) / (p_ITA + p_IND)
# Only 4 missing values 3 in SWE and 1 in GBR

# Density plot

p2_SWE <- ggplot(data_wide, aes(x=new_cases_per_million.SWE)) +
                   geom_histogram(binwidth = 1) + 
  xlim(0, 500) +
  ylim(0, 30)
p2_DNK <- ggplot(data_wide, aes(x=new_cases_per_million.DNK)) +
  geom_histogram(binwidth = 1) + 
  xlim(0, 500)+
  ylim(0, 30)
p2_NOR <- ggplot(data_wide, aes(x=new_cases_per_million.NOR)) +
  geom_histogram(binwidth = 1) + 
  xlim(0, 500)+
  ylim(0, 30)
p2_GBR <- ggplot(data_wide, aes(x=new_cases_per_million.GBR)) +
  geom_histogram(binwidth = 1) + 
  xlim(0, 500)+
  ylim(0, 30)
p2_ITA <- ggplot(data_wide, aes(x=new_cases_per_million.ITA)) +
  geom_histogram(binwidth = 1) + 
  xlim(0, 500)+
  ylim(0, 30)
p2_IND <- ggplot(data_wide, aes(x=new_cases_per_million.IND)) +
  geom_histogram(binwidth = 1) + 
  xlim(0, 500)+
  ylim(0, 30)

(p2_SWE + p2_DNK) / (p2_NOR + p2_GBR) / (p2_ITA + p2_IND)


#(B)
# correlation
library(hydroTSM)
#install.packages("hydroTSM")
correaltion_matrix <- cor(data_wide[c(2, 3, 4, 5, 6, 7)])
matrixplot(correaltion_matrix)

# cross-correlation
lagging = "Insert her"

ccf(data_wide$new_cases_per_million.SWE, data_wide$new_cases_per_million.DNK)
ccf(data_wide$new_cases_per_million.SWE, data_wide$new_cases_per_million.NOR )
ccf(data_wide$new_cases_per_million.SWE, data_wide$new_cases_per_million.GBR )
ccf(data_wide$new_cases_per_million.SWE, data_wide$new_cases_per_million.ITA )
ccf(data_wide$new_cases_per_million.SWE, data_wide$new_cases_per_million.IND )

ccf(data_wide$new_cases_per_million.DNK, data_wide$new_cases_per_million.NOR )
ccf(data_wide$new_cases_per_million.DNK, data_wide$new_cases_per_million.GBR )
ccf(data_wide$new_cases_per_million.DNK, data_wide$new_cases_per_million.ITA )
ccf(data_wide$new_cases_per_million.DNK, data_wide$new_cases_per_million.IND )

ccf(data_wide$new_cases_per_million.NOR, data_wide$new_cases_per_million.GBR )
ccf(data_wide$new_cases_per_million.NOR, data_wide$new_cases_per_million.ITA )
ccf(data_wide$new_cases_per_million.NOR, data_wide$new_cases_per_million.IND )

ccf(data_wide$new_cases_per_million.GBR, data_wide$new_cases_per_million.ITA )
ccf(data_wide$new_cases_per_million.GBR, data_wide$new_cases_per_million.IND )

ccf(data_wide$new_cases_per_million.ITA, data_wide$new_cases_per_million.IND )

#auto-correlation
acf(data_wide$new_cases_per_million.SWE)
acf(data_wide$new_cases_per_million.DNK)
acf(data_wide$new_cases_per_million.NOR)
acf(data_wide$new_cases_per_million.GBR)
acf(data_wide$new_cases_per_million.ITA)
acf(data_wide$new_cases_per_million.IND)


# (C)
data.pca <- prcomp(data_wide[c(2, 3, 4, 5, 6, 7)],
                   center = TRUE,
                   scale. = TRUE)

summary(data.pca)

library(ggfortify)
#install.packages("ggfortify")

data.pca.plot <- autoplot(data.pca,
                          data = data_wide)
data.pca.plot


biplot.data.pca <- biplot(data.pca)
biplot.data.pca


#(D): Smoothing
library(zoo)
data_wide_rm <- data_wide
data_wide_rm$new_cases_per_million.SWE <- rollapply(data_wide$new_cases_per_million.SWE,
                  width = 7,
                  FUN = weighted.mean,
                  w = c(1, 1, 2, 3, 2, 2,1))[1:3]


p1 <- ggplot(gobal_solution_data, aes(x=Date,
                                      y=T.missing)) +
  geom_line(color="purple", alpha=0.5) +
  geom_line(aes(x=Date, y=T.full), alpha=0.5) 

p2 <- ggplot(data_wide_rm, aes(x=Date,
                                     y=new_cases_per_million.SWE)) +
  geom_line(color="purple", alpha=0.5)

p2

