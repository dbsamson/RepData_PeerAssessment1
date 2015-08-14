raw$weekend<- (weekdays(ymd(as.character(raw$date))) %in% c("Saturday","Sunday"))
weekdaysData <- raw[!raw$weekend,]
weekendsData <- raw[raw$weekend,]
# par(mfrow = c(2, 1))
layout(matrix(c(1,1,2,2), 2,2, byrow = TRUE))

# Weekdays first 
weekdaysByInterval<-group_by(weekdaysData,interval)
weekdaystotalsByInterval <- summarise(weekdaysByInterval,steps=mean(steps,na.rm=T))
plot(x=weekdaysByInterval$interval,y=weekdaysByInterval$steps,xlab="Interval Nbr",ylab="Steps",main="Weekdays: Average Number of Steps per Interval",type="l")

# Weekends  
weekendsByInterval<-group_by(weekendsData,interval)
weekendstotalsByInterval <- summarise(weekendsByInterval,steps=mean(steps,na.rm=T))
plot(x=weekendsByInterval$interval,y=weekendsByInterval$steps,xlab="Interval Nbr",ylab="Steps",main="Weekends: Average Number of Steps per Interval",type="l")