
# Loading libraries
#-------------------------------------------------------------------------------
library('readr')
library('dplyr')
library('ggplot2')
library('tidyverse')
library('lubridate')
library('zoo')
library('reshape2')
library('devtools')
library('factoextra')


# Data
# ------------------------------------------------------------------------------
covid <- read_csv('VET/Courses/DAT320/Compulsary/Assignment1/data/covid.csv')
# View(covid)
# str(covid)

# Selecting time-window 
covid <- covid[covid$date >= '2020-03-16' & covid$date <= '2022-01-01', ] 

# Selecting countries and columns
countries <- c('SWE', 'DNK', 'NOR', 'GBR', 'ITA', 'IND')
covid <- covid[covid$iso_code %in% countries, c('date', 'iso_code', 'new_cases_per_million')]

# Checking that date-column is in correct format
class(covid$date)

# Summary statistics of data.frame
summary(covid)

"
# Reshaping data.frame
data_wide <- reshape(data = covid,
                     idvar = 'date',
                     timevar = 'iso_code',
                     v.names = 'new_cases_per_million',
                     direction = 'wide')
"


# Reshaping data.frame
date <- covid$date
date <- date[!duplicated(date)]

swe <- covid[covid$iso_code == 'SWE', ]
swe <- select(swe, new_cases_per_million)
dnk <- covid[covid$iso_code == 'DNK', ]
dnk <- select(dnk, new_cases_per_million)
nor <- covid[covid$iso_code == 'NOR', ]
nor <- select(nor, new_cases_per_million)
gbr <- covid[covid$iso_code == 'GBR', ]
gbr <- select(gbr, new_cases_per_million)
ita <- covid[covid$iso_code == 'ITA', ]
ita <- select(ita, new_cases_per_million)
ind <- covid[covid$iso_code == 'IND', ]
ind <- select(ind, new_cases_per_million)

df <- data.frame(date, swe, dnk, nor, gbr, ita, ind)

# Renaming data.frame
names(df) <- c('Date', 'Sweden', 'Denmark', 'Norway', 
               'Britain', 'Italia', 'India')


# Exploratory analysis
# ------------------------------------------------------------------------------
# Dimensionality
nrow(df)
ncol(df)
dim(df)

# Summary statistics
summary(df)

# Missing values
is.na(df$Sweden)
is.na(df$Denmark)
is.na(df$Norway)
is.na(df$Britain)
is.na(df$Italia)
is.na(df$India)
which(is.na(df))


# Plots 
# ------------------------------------------------------------------------------

# Distribution
boxdf <- select(df, Sweden, Denmark, Norway, Britain, Italia, India)
stacked_df <- stack(boxdf)
boxplot(stacked_df$values ~ stacked_df$ind,
        col = rainbow(ncol(df)), xlab='', ylab='New cases')


# Line plot for the countries separately
# Sweden
p <- ggplot(df, aes(x=Date, y=Sweden)) +
  geom_line() +
  geom_point() + 
  labs(x='', y='New cases', title='Sweden')
p

# Denmark
p <- ggplot(df, aes(x=Date, y=Denmark)) +
  geom_line() +
  geom_point() + 
  labs(x='', y='New cases', title='Denmark')
p

# Norway
p <- ggplot(df, aes(x=Date, y=Norway)) +
  geom_line() +
  geom_point() + 
  labs(x='', y='New cases', title='Norway')
p

# Great Britain
p <- ggplot(df, aes(x=Date, y=Norway)) +
  geom_line() +
  geom_point() + 
  labs(x='', y='New cases', title='Norway')
p

# Italia
p <- ggplot(df, aes(x=Date, y=Italia)) +
  geom_line() +
  geom_point() + 
  labs(x='', y='New cases', title='Italia')
p

# India
p <- ggplot(df, aes(x=Date, y=India)) +
  geom_line() +
  geom_point() + 
  labs(x='', y='New cases', title='India')
p


# Line plot, all countries combined
covid_cases <- melt(covid, id.vars=c('iso_code', 'date'),
                    value.name='Value')

covid_plot <- ggplot(data=covid_cases, aes(x=date,
                                           y=Value, 
                                           group=iso_code,
                                           colour=iso_code)) +
  geom_line() +
  geom_point(size=0.5) + 
  labs(y='New cases', x='')
covid_plot + ggtitle('Daily cases for each country')

covid_plot


# Auto- and cross-correlation
# ------------------------------------------------------------------------------
# Correlation-matrix
dfCorr <- select(df, Sweden, Denmark, Norway, Britain, Italia, India)
corr <- cor(dfCorr, method='pearson')
round(corr, 3)
heatmap(corr)

# Autocorrelation
acf(df$Sweden)
acf(df$Denmark)
acf(df$Norway)
acf(df$Britain)
acf(df$Italia)
acf(df$India)

# Cross-correlation
# Sweden
ccf(df$Sweden, df$Denmark)
ccf(df$Sweden, df$Norway)
ccf(df$Sweden, df$Britain)
ccf(df$Sweden, df$Italia)
ccf(df$Sweden, df$India)

# Denmark
ccf(df$Denmark, df$Norway)
ccf(df$Denmark, df$Britain)
ccf(df$Denmark, df$Italia)
ccf(df$Denmark, df$India)

# Norway
ccf(df$Norway, df$Denmark)
ccf(df$Norway, df$Britain)
ccf(df$Norway, df$Italia)
ccf(df$Norway, df$India)

# Britain
ccf(df$Britain, df$Denmark)
ccf(df$Britain, df$Norway)
ccf(df$Britain, df$Italia)
ccf(df$Britain, df$India)

# Italia
ccf(df$Italia, df$Denmark)
ccf(df$Italia, df$Norway)
ccf(df$Italia, df$Britain)
ccf(df$Italia, df$India)

# India
ccf(df$India, df$Denmark)
ccf(df$India, df$Norway)
ccf(df$India, df$Britain)
ccf(df$India, df$Italia)


# Transformations
# ------------------------------------------------------------------------------

# Copy of dataframe
dfT <- df
# Setting date as index
rownames(dfT) <- dfT$Date
dfT <- select(dfT, Sweden, Denmark, Norway, Britain, Italia, India)

# Remove missing values (even though there should not be any)
dfT <- na.omit(dfT)

# PCA transform
df.pca <- princomp(dfT, scale=TRUE)

# Scores and loadings
df.pca$scores
df.pca$loadings

# Summary and plot
# plot(df.pca)
# biplot(df.pca)
summary(df.pca)

# Explained variance
fviz_eig(df.pca)

# Plot of scores
fviz_pca_ind(df.pca,
             col.ind = 'cos2', 
             gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'),
             repel = FALSE  
)

# Plot of loadings
fviz_pca_var(df.pca,
             col.var = 'contrib', 
             gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'),
             repel = TRUE   
)

# Biplot
fviz_pca_biplot(df.pca, repel = FALSE,
                col.var = '#1874CD', 
                col.ind = '#66CDAA'  
)



# Smoothing 
# ------------------------------------------------------------------------------
# Sweden
swe1 <- zoo::rollmean(df$Sweden, k=3, fill=NA)
swe2 <- zoo::rollmean(df$Sweden, k=10, fill=NA)
swe3 <- zoo::rollmean(df$Sweden, k=30, fill=NA)
swe4 <- zoo::rollmean(df$Sweden, k=60, fill=NA)
dfSweden <- data.frame(date, swe1, swe2, swe3, swe4)


# Line plot of smoothing with different value for the kernel size
colors <- c('k=3'='#CD950C', 'k=10'='#009ACD', 'k=30'='#8A360F',
            'k=60'='#CD3333')

ggplot(dfSweden, aes(x=date)) +
  geom_line(aes(y=swe1, color='k=3'), size=1.0) +
  geom_point(aes(y=swe1, color='k=3'), size=0.5) +
  geom_line(aes(y=swe2, color='k=10'), size=1.0) + 
  geom_point(aes(y=swe2, color='k=10'), size=0.5) +
  geom_line(aes(y=swe3, color='k=30'), size=1.0) +
  geom_point(aes(y=swe3, color='k=30'), size=0.5) +
  geom_line(aes(y=swe4, color='k=60'), size=1.0) +
  geom_point(aes(y=swe4, color='k=60'), size=0.5) +
  labs(x='Date', y='New cases', color='Kernel size') +
  scale_color_manual(values=colors)


# Smoothing entire time-series
# ------------------------------------------------------------------------------
date <- df$Date
dfS <- select(df, Sweden, Denmark, Britain, Norway, Italia, India)
dfS <- zoo::rollmean(dfS, k=60, fill=NA)

# New data.frame
dfSm <- data.frame(date, dfS[,'Sweden'], dfS[,'Denmark'],
                   dfS[,'Norway'], dfS[,'Britain'], 
                   dfS[,'Italia'], dfS[,'India'])
# Renaming data.frame
names(dfSm) <- c('Date', 'Sweden', 'Denmark', 'Norway',
                 'Britain', 'Italia', 'India')


# Line plot of smoothed time-series
colors <- c('Sweden'='#CD950C', 'Norway'='#009ACD', 'Denmark'='#CD3333',
            'Britain'='#8A360F', 'Italia'='#9BCD9B', 'India'='#EE7600')

p <- ggplot(dfSm, aes(x=date)) +
     geom_line(aes(y=Sweden, color='Sweden'), size=1.0) +
     geom_point(aes(y=Sweden, color='Sweden'), size=0.5) + 
     geom_line(aes(y=Norway, color='Norway'), size=1.0) + 
     geom_point(aes(y=Norway, color='Norway'), size=0.5) + 
     geom_line(aes(y=Denmark, color='Denmark'), size=1.0) +
     geom_point(aes(y=Denmark, color='Denmark'), size=0.5) + 
     geom_line(aes(y=Britain, color='Britain'), size=1.0) +
     geom_point(aes(y=Britain, color='Britain'), size=0.5) + 
     geom_line(aes(y=Italia, color='Italia'), size=1.0) + 
     geom_point(aes(y=Italia, color='Italia'), size=0.5) + 
     geom_line(aes(y=India, color='India'), size=1.0) +
     geom_point(aes(y=India, color='India'), size=0.5) + 
     labs(x='Date', y='New cases', color='Country') +
     scale_color_manual(values=colors)
p


# PCA on smoothed time-series
# ------------------------------------------------------------------------------

# Copy of dataframe
dfT <- dfSm
# Setting date as index
rownames(dfT) <- dfT$Date
dfT <- select(dfT, Sweden, Denmark, Norway, Britain, Italia, India)

# Remove missing values
dfT <- na.omit(dfT)

# PCA transform
df.pca <- princomp(dfT, scale=TRUE)

# Scores and loadings
df.pca$scores
df.pca$loadings

# Summary and plot
# plot(df.pca)
# biplot(df.pca)
summary(df.pca)

# Explained variance
fviz_eig(df.pca)

# Plot of scores
fviz_pca_ind(df.pca,
             col.ind = 'cos2', 
             gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'),
             repel = FALSE  
)

# Plot of loadings
fviz_pca_var(df.pca,
             col.var = 'contrib', 
             gradient.cols = c('#00AFBB', '#E7B800', '#FC4E07'),
             repel = TRUE   
)

# Biplot
fviz_pca_biplot(df.pca, repel = FALSE,
                col.var = '#1874CD', 
                col.ind = '#66CDAA'  
)

