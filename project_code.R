rm(list=ls())
setwd("~/Documents/STAT503_MvAnalysis")
data = read.csv('train-1.csv', header=T)
data = as.data.frame(data)

library(ggplot2)
library(lubridate)

# extract time information
data$datetime = format(as.POSIXct((data$datetime), format = "%m/%d/%y %H:%M"))
data$hour = factor(hour(data$datetime))
data$weekday = factor(wday(data$datetime))
data$month = factor(month(data$datetime))

# make a early morning/ morning/ daytime/ early evening/ late evening
tod = function(x){
  if(x>= 0 && x<=6) {
    return ('early_morning')
  } else if(x>=7 && x<=8) {return ('morning')
  } else if(x>=9 && x<=16) {return ('daytime')
  } else if(x>=17 && x<=19) {return ('early_evening')
  } else if(x>=20 && x<=23) {return ('early_evening')
  } else return ('other')
}
data$tod = factor(sapply(as.numeric(data$hour), FUN=tod))

# make season a factor (1=spring, 2=summer, 3=fall, 4=winter)
data$season = factor(data$season)
# holiday = 1 if holiday
data$holiday = factor(data$holiday)
# workingday = 1 if neither a weekend or a holiday
data$workingday = factor(data$workingday)
# weather: (1 - clear; 2 - mist/ cloudy; 3 - light precip; 4 - heavy precip/ bad weather)
data$weather = factor(data$weather)

summary(data)

# Regression - exploration of discrete variables
g = ggplot(data, aes(tod, count, fill=season))
g+geom_boxplot()

g = ggplot(data, aes(tod, count, fill=weather))
g+geom_boxplot()

g = ggplot(data, aes(tod, count, fill=weekday))
g+geom_boxplot()

# Regression - exploration of continuous variables
ggplot(data, aes(x=count, y=temp)) + 
  geom_point(shape=1, size=0.5) +
  geom_smooth(method=lm)

# demand versus humidity
ggplot(data, aes(x=count, y=humidity)) + 
  geom_point(shape=1, size=0.5) +
  geom_smooth(method=lm)

# demand versus windspeed
ggplot(data, aes(x=count, y=windspeed)) + 
  geom_point(shape=1, size=0.5) +
  geom_smooth(method=lm)






m1 = lm(count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+casual, data=data)
summary(m1)

plot(data$count, data$registered)

