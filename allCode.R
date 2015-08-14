
raw<-read.csv("activity.csv")
summary(raw) # For simple overview of loaded data.  


## breaks <-  as.Date.POSIXct(c("1-Oct-2012","15-Oct-2012","30-Oct-2012","14-Nov-2012","29-Nov-2012"))

grouped<-group_by(raw,date)
dailyTotals <- summarise(grouped,sumSteps=sum(steps),meanSteps=mean(steps,na.rm=T),medianSteps=median(steps,na.rm=T))

## Overview plot of the data
# breaks <-  as.Date.POSIXct(c("1-Oct-2012","15-Oct-2012","30-Oct-2012","14-Nov-2012","29-Nov-2012"))
p<-ggplot(dailyTotals,aes(x=date, y=sumSteps)) + 
  geom_histogram(stat="identity") 

p<-ggplot(dailyTotals,aes(x=date, y=sumSteps)) +             geom_histogram(stat="identity")+ 
  labs(title="Total Steps by Subject\nOct & Nov 2012",
       x="Date", y="Number of Steps")
##scale_x_date(labels = date_format("%m/%d"))
##scale_x_date(breaks = "1 month", minor_breaks = "1 week")

### Group by internval 
groupedByInterval<-group_by(raw,interval)
TotalsByInterval <- summarise(groupedByInterval,Steps=mean(steps,na.rm=T))
plot(TotalsByInterval$interval,TotalsByInterval$Steps,xlab="Interval Nbr",ylab="Steps",main="Average Number of Steps per Interval",type="l")

## Fill in empty values.  
## Careful attention must be paid to getting the correct interval for imputing
imputed<-raw # copy the raw data, with the nas
for(i in 1:nrow(imputed)) {
  if (is.na(imputed[i,1]))  {
    rownum <- (trunc(imputed[i,3]/100)*12+(imputed[i,3]%%100)/5)+1
    imputed[i,1]<- TotalsByInterval[rownum,2]
  }
}  
