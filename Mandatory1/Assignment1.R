#Problem 1.
rm(list = ls())
# Reading the data.
no2.data <- read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/data/no2.txt",sep="\t",header=TRUE)

# Assigning parameters to each data type
log.cars = no2.data$log.cars
log.no2 = no2.data$log.no2
temp = no2.data$temp
wind.speed = no2.data$wind.speed
hour.of.day = no2.data$hour.of.day

#A.
# Printing the data.
no2.data

#summaries.
summary(log.no2)
summary(log.cars)


#Boxplot for each variable
boxplot(log.no2, ylab="log.no2")
boxplot(log.cars, ylab="log.cars")

#Scatterplot.
plot(log.cars, log.no2,lwd = 2)      

#B.

#simple linear fit.
fit = lm(log.no2~log.cars)
summary(fit)

#Plotting all the Data points togheter with the fitted model.
plot(log.no2~log.cars,lwd = 2)
abline(fit,col = "red",lwd = 3)


#C.
# Partial residual plot, checking for linearity 
par(mfrow = c(1,1))
library(car)
crPlots(fit, terms=~log.cars)

# Residuals vs fitted and sq-rt(standard residuals) vs fitted values
par(mfrow = c(1,2))
plot(fit,1)
plot(fit,3)

# histogram of the fitted residuals and box plot of fitted residuals 
par(mfrow = c(1,2))
hist(fit$res,breaks=20)
boxplot(fit$res)

#Normal q-q plot
par(mfrow = c(1,1))
qqnorm(fit$res); qqline(fit$res)


#D.
# Fitting multiple linear regression models.
# Finding the best model through trial and error.
multi.fit = lm(log.no2~log.cars+temp+wind.speed + log(hour.of.day))
summary(multi.fit)

multi.fit = lm(log.no2~log.cars+temp+wind.speed + hour.of.day)
summary(multi.fit)

multi.fit = lm(log.no2~log.cars+temp+log(wind.speed) + log(hour.of.day))
summary(multi.fit)

#Best model.
multi.fit = lm(log.no2~log.cars+temp+log(wind.speed) + hour.of.day)
summary(multi.fit)

#E.
#Cpr plot.
crPlots(multi.fit, terms=~log.cars+temp+log(wind.speed) + hour.of.day)