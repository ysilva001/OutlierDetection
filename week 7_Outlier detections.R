###################
# Title : Anomaly Detection 
# Data Source: https://datamarket.com/data/set/22p8/number-of-earthquakes-per-year-magnitude-70-or-greater-1900-1998#!ds=22p8&display=line
# Author : Yesenia Silva
# MSDS664_: Statistical Inference and Predictive Analytics
# Number of earthquakes per year magnitude 7.0 or greater. 1900-1998
###################

##############Packages#############
library(readr)  ##Read the file
library(ggplot2) #Visuals
library(AnomalyDetection) ##Twitter's package to finding spikes and dips
library(tsoutliers) # Automatic Detection of Outliers in Time Series by building an ARIMA model and analyzing the distance between the predicted and actual values
library(forecast) # arima model
library(outliers)



##Import Data
earthquakes <- read_csv("C:/Users/ysilva/Desktop/Statistical Inference and Predictive Analytics/earthquakes-per-year.csv", 
                        col_types = cols(Frequency = col_number(), 
                        Year = col_date(format = "%Y")), 
                        na = "NA")
str(earthquakes)
summary(earthquakes)


#Convert date to POSIXlt adn to a data.frame
earthquake <- data.frame(earthquake)
earthquake$Year <- as.POSIXlt(earthquake$Year)
str(earthquake)

#quick plotearthquake <- data.frame(earthquake)
earthquake$Year <- as.POSIXlt(earthquake$Year)
str(earthquake)

#Let's look at anomolies 
#alpha .05 and period 7
res = AnomalyDetectionVec(earthquake$Frequency, max_anoms=0.0005, direction='both', plot=TRUE, period=7)
res$plot
#alpha .05 and period 12
res1 = AnomalyDetectionVec(earthquake$Frequency, max_anoms=0.05, direction='both', plot=TRUE, period=12)
res1$plot
#alpha .05 and period 24
res2 = AnomalyDetectionVec(earthquake$Frequency, max_anoms=0.05, direction='both', plot=TRUE, period=24)
res2$plot

#general boxplot 
p <- ggplot(earthquake, aes(Year, Frequency))
p + geom_boxplot()

boxplot(earthquake$Frequency ~earthquake$Year)


#Create time series objects
ts.earthquake <- ts(earthquake$Frequency, start=1900, end=1998, frequency=1)
ts.earthquake
mean(ts.earthquake)
frequency(ts.earthquake)

#tso function to idnetify outliers
earthquake.ts.outliers <- tso(ts.earthquake)
earthquake.ts.outliers
plot(earthquake.ts.outliers)

#plot time series
plot.ts(ts.earthquake,main = 'Number of earthquakes per year magnitude 7.0 or greater 1900-1998')


