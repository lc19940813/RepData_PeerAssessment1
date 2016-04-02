setwd("C://Users//LeoChaos//Desktop//Git//RepData_PeerAssessment1")
unzip("activity.zip")

library(dplyr)
actdata <- read.csv("activity.csv")
actdata <- tbl_df(actdata)

#What is mean total number of steps taken per day?
grouped_data_bydate <- group_by(actdata, date)
sum1 <- summarise(grouped_data_bydate, sum = sum(steps,na.rm = TRUE))
res1 <- sum1$sum

hist(res1,col = "red",xlab = "Total number of steps", main = "Total number of steps taken per day" )
dev.copy(png, "mean total number of steps taken per day.png")
dev.off()
steps_mean <- mean(res1)
steps_median <- median(res1)

#What is the average daily activity pattern?
grouped_data_byinterval <- group_by(actdata, interval)
sum2 <- summarise(grouped_data_byinterval, mean = mean(steps,na.rm = TRUE))
res2 <- sum2$mean

plot(x = sum2$interval, y = res2, type = "l", xlab = "", ylab = "")
title(xlab = "Time interval", ylab = "Average steps taken", 
      main = "Average steps taken in given time interval")

max_act_time <- sum2$interval[which.max(res2)]

#Imputing missing values
sum(is.na(actdata$steps))

#Imputing..
grouped_data_bydate <- cbind(grouped_data_bydate, group_indices(grouped_data_bydate))
for(i in 1: length(grouped_data_bydate$steps)){
    if(is.na(grouped_data_bydate[i,1])){
        grouped_data_bydate[i,1] <- res1[grouped_data_bydate[i,4]]
    }
}

newdata <- grouped_data_bydate
sum3 <- summarise(newdata, sum = sum(steps,na.rm = TRUE))
res3 <- sum1$sum

hist(res3,col = "red",xlab = "Total number of steps", main = "Total number of steps taken per day" )

steps_mean_imputed <- mean(res3)
steps_median_imputed <- median(res3)
newdata <- ungroup(newdata)

#Are there differences in activity patterns between weekdays and weekends?
date <- newdata$date
weekday <- weekdays(as.Date(date))
state <- character()
for (i in 1: length(weekday)){
    if(weekday[i] == "ÐÇÆÚÁù" | weekday[i] ==  "ÐÇÆÚÈÕ"){
        state[i] <- "weekend"
    }
    else {
        state[i] <- "weekday"
    }
}
state <- as.factor(state)
dat <- mutate(newdata, state = state)
grouped_dat_byinterval <- group_by(dat, interval,state)
sum4 <- summarise(grouped_dat_byinterval, mean = mean(steps))

xyplot(mean ~ interval | state, sum4, type = "l",col = "blue",
       xlab = "Time interval", ylab = "Average steps taken",
       layout = c(1,2))
