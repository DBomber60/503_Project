data = read.csv('train-1.csv', header=T)
data = as.data.frame(data)

# convert date field into two separate fields(one for date and one for time)
dates <- format(as.POSIXct((data$datetime), format = "%m/%d/%y %H:%M"))
date <- sapply(strsplit(as.character(dates), " "), "[", 1)
data$time <- sapply(strsplit(as.character(dates), " "), "[", 2)
data$month = format(as.Date(date), "%m")
data$day = format(as.Date(date), "%d")
data$year = format(as.Date(date), "%y")



# demand by hour/ month
boxplot(count~time, data = data)
boxplot(count~month, data = data)
