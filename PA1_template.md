---
title: "PA1_template - Reproducible Research"
author: "Alistair Acheson"
date: "7/28/2019"
output:
  html_document: default
  pdf_document: default
---
### Importing packages

```{r results = "hide", message = FALSE}
library(tidyverse)
library(lubridate)
library(scales)
```

### Setting Work Directory

```{r echo = TRUE}
setwd("C:\\Users\\Alistair\\Desktop\\Data Science\\5 - Reproducible Research\\Week 2")
```


### 1: Code for reading in the dataset and/or processing the data

```{r echo = TRUE}
activity <- read.csv("activity.csv")
```


### 2: Histogram of the total number of steps taken each day

```{r echo = TRUE, message = FALSE}
step_sum_by_date <- activity %>%
    group_by(date) %>%
    summarise(stepsum = sum(steps))


histogram_2 <- ggplot(step_sum_by_date, aes(x = stepsum)) +
    geom_histogram(aes(fill = "red"), bins = 60) + 
    labs(title = "Frequency of total steps taken each day over 60 days",
         y = "Frequency", x = "Steps") +
    theme(legend.position = "none")
    
print(histogram_2)    
```
![test](/figures/histogram_2.png)

### 3: Mean and median number of steps taken each day

```{r echo = TRUE}
step_sum_by_date_mm <- activity %>%
    group_by(date) %>%
    summarise(mean = mean(steps), median = median(steps),
              stepsum = sum(steps))

print.data.frame(step_sum_by_date_mm)
```


### 4: Time series plot of the average number of steps taken

```{r echo = TRUE}
step_sum_by_interval_mm <- activity %>%
    group_by(interval) %>%
    summarise(mean = mean(steps, na.rm = TRUE), 
              median = median(steps, na.rm = TRUE),
              stepsum = sum(steps, na.rm = TRUE))


ggplot(step_sum_by_interval_mm, aes(x = interval, y = mean)) +
    geom_line(aes(colour = "red", group = 1), size = 1) +
    theme(legend.position = "none") +
    labs(title = "Average number of steps taken per 5-minute interval", 
         x = "Interval", y = "Mean number of steps")
```

### 5: The 5-minute interval that, on average, contains the maximum number of steps

```{r echo = TRUE}
step_interval <- activity %>%
    group_by(interval) %>%
    summarise(mean = mean(steps, na.rm = TRUE)) %>%
    arrange(desc(mean))

print.data.frame(head(step_interval))


```

### 6: Code to describe and show a strategy for imputing missing data

#### Total number of missing values in the dataset
```{r echo = TRUE}
print(sum(is.na(activity$steps)))
```

#### Filling in missing values of data set with mean for the 5 minute interval

This tells us the dates with NA's

```{r echo = TRUE}
step_sum_by_date %>%
    filter(is.na(stepsum))
```

#### Creating new dataset with missing data filled in

Since we calculated the mean value for every interval in this data frame, we can use those values to replace the NA values
```
step_sum_by_interval_mm[,2] 
``` 

Replacing NA's with means at specified NA rows
```{r echo = TRUE}
activity_na_mean <- activity

activity_na_mean[1:288,1] <- step_sum_by_interval_mm[,2] #2012-10-01
activity_na_mean[2017:2304,1] <- step_sum_by_interval_mm[,2] #2012-10-08
activity_na_mean[9216:8929,1] <- step_sum_by_interval_mm[,2] #2012-11-01
activity_na_mean[9793:10080,1] <- step_sum_by_interval_mm[,2] #2012-11-04
activity_na_mean[11233:11520,1] <- step_sum_by_interval_mm[,2] #2012-11-09
activity_na_mean[11521:11808,1] <- step_sum_by_interval_mm[,2] #2012-11-10
activity_na_mean[12673:12960,1] <- step_sum_by_interval_mm[,2] #2012-11-14
activity_na_mean[17281:17568,1] <- step_sum_by_interval_mm[,2] #2012-11-30
```

Confirms no missing data after replacement
```{r echo = TRUE}
sum(is.na(activity_na_mean))
```


### 7: Histogram of the total number of steps taken each day after missing values are imputed

```{r echo = TRUE}
step_sum_by_date_na <- activity_na_mean %>%
    group_by(date) %>%
    summarise(stepsum = sum(steps))

ggplot(step_sum_by_date_na, aes(x = stepsum)) +
    geom_histogram(aes(fill = "red"), bins = 60) + 
    labs(title = "Frequency of total steps taken each day over 60 days - Missing data imputed",
         y = "Frequency", x = "Steps") +
    theme(legend.position = "none")
```

Calculating the mean and median steps after missing data imputed

```{r echo = TRUE}
step_sum_by_date_na_mm <- activity_na_mean %>%
    group_by(date) %>%
    summarise(mean = mean(steps), median = median(steps),
              stepsum = sum(steps))

print.data.frame(step_sum_by_date_na_mm)
```


The impact of replacing the NA values with the mean values is that:  
1. We no longer have medians of all 0  
2. Increases overall mean value for each interval  
3. Frequency spikes at the mean  

  
  
### 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


Using mutate and ifelse to make a new column that identifies whether a particular date is a Weekday or Weekend

```{r echo = TRUE}
activity_factor <- activity

activity_factor$date <- as.Date(activity_factor$date)

weekdays_list <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                   "Friday")

activity_factor <- activity_factor %>%
    mutate(daytype = ifelse(weekdays(activity_factor$date) %in% weekdays_list, "Weekday", "Weekend"))
```


Panel plot - Grouped data to use with plot

```{r echo = TRUE}
activity_factor_grouped <- activity_factor %>%
    group_by(interval, daytype) %>%
    summarise(mean = mean(steps, na.rm = TRUE))


panel_plot <- ggplot(activity_factor_grouped, aes(x = interval, y = mean)) +
    geom_line(aes(colour = "red"), size = 1) +
    facet_grid(daytype ~.) +
    theme(legend.position = "none") +
    labs(title = "Weekday vs Weekend average Steps", 
         x = "Interval", y = "Average steps")
         
print(panel_plot)
```

