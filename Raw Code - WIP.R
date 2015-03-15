getwd()
setwd("C:/Users/Mark Account/Documents/Software/Github/ReproduceResearchProject1/RepData_PeerAssessment1")


#unzips file and extracts it in same directory level.
unzip("./activity.zip")
    
#read in data file
dataFrame <- read.csv("./activity.csv", header = TRUE)

#convert the date column into a character type. It is just an intermediary step.
dataFrame[,2] <- as.character(dataFrame[,2])

#convert the date column into a Date data type.
dataFrame[,2] <- as.Date(dataFrame[,2], format = "%Y-%m-%d")


#Summarizing the Total Steps per day
library(dplyr)

#removing the rows of NA
good <- complete.cases(dataFrame)

#data set only includes complete data.
CompleteData <- dataFrame[good,]

#creates a data frame of total steps per day.
gg <- group_by(CompleteData, date)
pp <- summarise(gg, sumSteps = sum(steps))
sumOfStepsPerDay <- data.frame(pp)

#create a histogram
hist(sumOfStepsPerDay$sumSteps, main = "Number of Steps Per Day", 
     xlab = "Number of Steps", ylab = "Frequency (# of Days)")



#mean
mean(sumOfStepsPerDay$sumSteps)
#median
median(sumOfStepsPerDay$sumSteps)



#Average Daily Activity Pattern
Data2 <- CompleteData

options(scipen=999)


#group by time interval
groupedByTime <- group_by(Data2, interval)
yyy <- summarise(groupedByTime, avgSteps = mean(steps))
avgStepsPerInterval <- data.frame(yyy)

#convert interval into a date-time series, so i can be plotted by time.
avgStepsPerInterval$interval2 <- as.character(avgStepsPerInterval$interval)
avgStepsPerInterval$interval2 <- str_pad(avgStepsPerInterval$interval2, 4, pad = "0")
avgStepsPerInterval$interval3 <- strptime(avgStepsPerInterval$interval2, format = "%H%M")

plot(avgStepsPerInterval$interval3, avgStepsPerInterval$avgSteps, type = "l",
     main = "Average Number of Steps Per Interval", xlab = "Interval", ylab = "# of Steps")


#which interval has highest average steps?
maxi <- which.max(avgStepsPerInterval$avgSteps)
#plug index in to get the interval
ans <- avgStepsPerInterval[maxi,1]
#answer
ans




#Imputing Missing Values
Data3 <- dataFrame
#removing the rows of NA
bad <- is.na(Data3$steps)
vectorBad <- Data3[!bad,]
#calculate number of NA's
sum(bad)

#turn NA's into avg. # of steps for that interval over whole period
#need the avgStepsPerInterval Data Frame as a lookup table
numRows <- nrow(Data3)
for(x in 1:numRows){
    
    if(is.na(Data3$steps[x])){
        #y gets interval number
        y <- Data3[x,3]
        print(y)
        #retrieves the average for that interval
        z <- avgStepsPerInterval[avgStepsPerInterval$interval==y,2]
        #replaces the NA with the value
        print(z)
        Data3$steps[x] <- z
    }
        
}
#New Data Set with NA's filled in
Data3

#compute number of steps per day on new data set
gh <- group_by(Data3, date)
km <- summarise(gh, sumSteps = sum(steps))
NewStepsPerDay <- data.frame(km)

#create a histogram
hist(NewStepsPerDay$sumSteps, main = "Number of Steps Per Day", 
     xlab = "Number of Steps", ylab = "Frequency (# of Days)")

#mean
mean(NewStepsPerDay$sumSteps)
#median
median(NewStepsPerDay$sumSteps)


#weekdays vs weekends
Data4 <- Data3
Data4$dayWeek <- "Dummy Text"

library(timeDate)

weekDayBoolean <- isWeekday(Data3$date)
weekEndBoolean <- isWeekend(Data3$date)

Data4$dayWeek[weekDayBoolean] <- "weekday"
Data4$dayWeek[weekEndBoolean] <- "weekend"

Data4$dayWeek <- as.factor(Data4$dayWeek)


grouping <- group_by(Data4, dayWeek, interval)
summ <- summarise(grouping, averageSteps = mean(steps))
meanStepInt <- data.frame(summ)

meanStepInt$interval2 <- as.character(meanStepInt$interval)
meanStepInt$interval2 <- str_pad(meanStepInt$interval2, 4, pad = "0")
meanStepInt$interval3 <- strptime(meanStepInt$interval2, format = "%H%M")


library(ggplot2)
g <- ggplot(meanStepInt, aes(interval3, averageSteps))
g2 <- g + geom_line() + facet_grid(dayWeek ~ .)
g3 <- g2 + labs(title = "Average Number of Steps Per Interval", x = "Interval", 
    y = "Average # of Steps")
g3
