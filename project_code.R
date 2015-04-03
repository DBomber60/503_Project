rm(list=ls())
setwd("~/Documents/STAT503_MvAnalysis")
data = read.csv('train-1.csv', header=T)
data = as.data.frame(data)

library(ggplot2)
library(lubridate)
library(pls)
library(rpart)
library(tree)
library(MASS)

# extract time information
data$datetime = format(as.POSIXct((data$datetime), format = "%m/%d/%y %H:%M"))
data$hour = factor(hour(data$datetime))
data$weekday = factor(wday(data$datetime))
data$month = factor(month(data$datetime))

# make a early morning/ morning/ daytime/ early evening/ late evening
tod = function(x){
  if (x>=7 && x<=8) {return ('morning commute')
  } else if(x>=9 && x<=16) {return ('daytime')
  } else if(x>=17 && x<=19) {return ('early_evening')
  } else if(x>=20 && x<=23) {return ('late_evening')
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

# split into training and test
set.seed(1)
rownum = seq(1:nrow(data))
train_rows = sample(rownum,floor(nrow(data)*0.9))
train = data[train_rows,]
test = data[-train_rows,]

qplot(count, data=data, geom="histogram",binwidth=1)
qplot(log(count), data=data, geom="histogram",binwidth=.01)


# Regression - exploration of discrete variables
g = ggplot(data, aes(tod, count, fill=season))
g+geom_boxplot() #+scale_fill_manual(name="Season and Time of Day Demand", pallete="Blues")

g = ggplot(data, aes(tod, count, fill=weather))
g+geom_boxplot()

g = ggplot(data, aes(tod, count, fill=weekday))
g+geom_boxplot()

# Regression - exploration of continuous variables
ggplot(data, aes(x=count, y=temp)) + 
  geom_point(shape=1, size=2, aes(colour=season, colours=terrain.colors(1))) +
  geom_smooth(method=lm)

# demand versus humidity
ggplot(data, aes(x=count, y=humidity)) + 
  geom_point(shape=1, size=2, aes(colour=season)) +
  geom_smooth(method=lm)

# demand versus windspeed
ggplot(data, aes(x=count, y=windspeed)) + 
  geom_point(shape=1, size=0.5) +
  geom_smooth(method=lm)

# function to evaluate model fit
rmse = function(x,y) sqrt(mean((x-y)^2))

# multpiple linear regression - count as response variable
m1 = lm(count~season+tod+weather+temp+humidity+windspeed, data=train)
summary(m1)
qplot(m1$fitted, m1$residual, xlab="Fitted", ylab="Residuals")+geom_abline(intercept=0,slope=0,colour="red",size=2)
rmse(m1$fit, train$count)
rmse(predict(m1,test[,c(2,16,5,6,8,9)]), test$count)
qplot(m1$fit, train$count)
qplot(predict(m1,test[,c(2,16,5,6,8,9)]), test$count)

# multpiple linear regression - casual+registered as response variable
lm_casual = lm(casual~season+tod+weather+temp+humidity+windspeed, data=train)
summary(lm_casual)
lm_registered = lm(registered~season+tod+weather+temp+humidity+windspeed, data=train)
summary(lm_registered)
qplot(m1$fitted, m1$residual, xlab="Fitted", ylab="Residuals")+geom_abline(intercept=0,slope=0,colour="red",size=2)
rmse(lm_casual$fit+lm_registered$fit, train$count)
casual_predict = predict(lm_casual, test[,c(2,16,5,6,8,9)])
registered_predict = predict(lm_registered, test[,c(2,16,5,6,8,9)])
rmse(casual_predict+registered_predict, test$count)
qplot(m1$fit, train$casual)
qplot(predict(m1,test[,c(2,16,5,6,8,9)]), test$count)




# regression tree
reg_tree = tree(count~season+tod+weather+temp+humidity+windspeed, method="anova",data=train)
summary(reg_tree)
cv.reg_tree=cv.tree(reg_tree)
plot(cv.reg_tree$size, cv.reg_tree$dev, type="b")
yhat= predict(reg_tree, newdata=train[,c(2,16,5,6,8,9)])
rmse(yhat, train$count)
qplot(yhat, train$count)
yhat= predict(reg_tree, newdata=test[,c(2,16,5,6,8,9)])
rmse(yhat, test$count)
qplot(yhat, test$count)

# regression tree - individual predictions
c_reg_tree = tree(casual~season+tod+weather+temp+humidity+windspeed, method="anova",data=train)
r_reg_tree = tree(registered~season+tod+weather+temp+humidity+windspeed, method="anova",data=train)
cyhat= predict(c_reg_tree, newdata=train[,c(2,16,5,6,8,9)])
ryhat= predict(r_reg_tree, newdata=train[,c(2,16,5,6,8,9)])
rmse(cyhat+ryhat, train$count)
c_yhat= predict(c_reg_tree, newdata=test[,c(2,16,5,6,8,9)])
r_yhat= predict(r_reg_tree, newdata=test[,c(2,16,5,6,8,9)])
rmse(c_yhat+r_yhat, test$count)
tot = c_yhat+r_yhat
qplot(tot, test$count)

# poisson regression
poisrg = glm(count~season+tod+weather+temp+humidity+windspeed, data=train, family=poisson)
summary(poisrg)
# does the model fit well?
1-pchisq(593704, 9783)
rmse(poisrg$fit, train$count)
qplot(poisrg$fit, train$count)
rmse(exp(predict(poisrg,test[,c(2,16,5,6,8,9)])), test$count)
qplot(exp(predict(poisrg,test[,c(2,16,5,6,8,9)])), test$count)

# poisson regression - individual predictions
c_poisrg = glm(casual~season+tod+weather+temp+humidity+windspeed, data=train, family=poisson)
r_poisrg = glm(registered~season+tod+weather+temp+humidity+windspeed, data=train, family=poisson)
summary(poisrg)
# does the model fit well?
1-pchisq(593704, 9783)
rmse((c_poisrg$fit+r_poisrg$fit), train$count)
qplot(poisrg$fit, train$count)
c_yhat= exp(predict(c_poisrg, newdata=test[,c(2,16,5,6,8,9)]))
r_yhat= exp(predict(r_poisrg, newdata=test[,c(2,16,5,6,8,9)]))
rmse(c_yhat+r_yhat, test$count)
tot = c_yhat+r_yhat
qplot(tot, test$count)


