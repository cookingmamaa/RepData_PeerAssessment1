---
title: "PA1_template - Reproducible Research"
author: "Alistair Acheson"
date: "7/28/2019"
---

### Importing packages

``` r
library(tidyverse)
library(lubridate)
library(scales)
```

### Setting Work Directory

``` r
setwd("C:\\Users\\Alistair\\Desktop\\Data Science\\5 - Reproducible Research\\Week 2")
```

### 1: Code for reading in the dataset and/or processing the data

``` r
activity <- read.csv("activity.csv")
```

### 2: Histogram of the total number of steps taken each day

``` r
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

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![test](/figures/histogram_2.png)

### 3: Mean and median number of steps taken each day

``` r
step_sum_by_date_mm <- activity %>%
    group_by(date) %>%
    summarise(mean = mean(steps), median = median(steps),
              stepsum = sum(steps))

print.data.frame(step_sum_by_date_mm)
```

    ##          date       mean median stepsum
    ## 1  2012-10-01         NA     NA      NA
    ## 2  2012-10-02  0.4375000      0     126
    ## 3  2012-10-03 39.4166667      0   11352
    ## 4  2012-10-04 42.0694444      0   12116
    ## 5  2012-10-05 46.1597222      0   13294
    ## 6  2012-10-06 53.5416667      0   15420
    ## 7  2012-10-07 38.2465278      0   11015
    ## 8  2012-10-08         NA     NA      NA
    ## 9  2012-10-09 44.4826389      0   12811
    ## 10 2012-10-10 34.3750000      0    9900
    ## 11 2012-10-11 35.7777778      0   10304
    ## 12 2012-10-12 60.3541667      0   17382
    ## 13 2012-10-13 43.1458333      0   12426
    ## 14 2012-10-14 52.4236111      0   15098
    ## 15 2012-10-15 35.2048611      0   10139
    ## 16 2012-10-16 52.3750000      0   15084
    ## 17 2012-10-17 46.7083333      0   13452
    ## 18 2012-10-18 34.9166667      0   10056
    ## 19 2012-10-19 41.0729167      0   11829
    ## 20 2012-10-20 36.0937500      0   10395
    ## 21 2012-10-21 30.6284722      0    8821
    ## 22 2012-10-22 46.7361111      0   13460
    ## 23 2012-10-23 30.9652778      0    8918
    ## 24 2012-10-24 29.0104167      0    8355
    ## 25 2012-10-25  8.6527778      0    2492
    ## 26 2012-10-26 23.5347222      0    6778
    ## 27 2012-10-27 35.1354167      0   10119
    ## 28 2012-10-28 39.7847222      0   11458
    ## 29 2012-10-29 17.4236111      0    5018
    ## 30 2012-10-30 34.0937500      0    9819
    ## 31 2012-10-31 53.5208333      0   15414
    ## 32 2012-11-01         NA     NA      NA
    ## 33 2012-11-02 36.8055556      0   10600
    ## 34 2012-11-03 36.7048611      0   10571
    ## 35 2012-11-04         NA     NA      NA
    ## 36 2012-11-05 36.2465278      0   10439
    ## 37 2012-11-06 28.9375000      0    8334
    ## 38 2012-11-07 44.7326389      0   12883
    ## 39 2012-11-08 11.1770833      0    3219
    ## 40 2012-11-09         NA     NA      NA
    ## 41 2012-11-10         NA     NA      NA
    ## 42 2012-11-11 43.7777778      0   12608
    ## 43 2012-11-12 37.3784722      0   10765
    ## 44 2012-11-13 25.4722222      0    7336
    ## 45 2012-11-14         NA     NA      NA
    ## 46 2012-11-15  0.1423611      0      41
    ## 47 2012-11-16 18.8923611      0    5441
    ## 48 2012-11-17 49.7881944      0   14339
    ## 49 2012-11-18 52.4652778      0   15110
    ## 50 2012-11-19 30.6979167      0    8841
    ## 51 2012-11-20 15.5277778      0    4472
    ## 52 2012-11-21 44.3993056      0   12787
    ## 53 2012-11-22 70.9270833      0   20427
    ## 54 2012-11-23 73.5902778      0   21194
    ## 55 2012-11-24 50.2708333      0   14478
    ## 56 2012-11-25 41.0902778      0   11834
    ## 57 2012-11-26 38.7569444      0   11162
    ## 58 2012-11-27 47.3819444      0   13646
    ## 59 2012-11-28 35.3576389      0   10183
    ## 60 2012-11-29 24.4687500      0    7047
    ## 61 2012-11-30         NA     NA      NA

### 4: Time series plot of the average number of steps taken

``` r
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

![test](/figures/4.png)

### 5: The 5-minute interval that, on average, contains the maximum number of steps

``` r
step_interval <- activity %>%
    group_by(interval) %>%
    summarise(mean = mean(steps, na.rm = TRUE)) %>%
    arrange(desc(mean))

print.data.frame(head(step_interval))
```

    ##   interval     mean
    ## 1      835 206.1698
    ## 2      840 195.9245
    ## 3      850 183.3962
    ## 4      845 179.5660
    ## 5      830 177.3019
    ## 6      820 171.1509

### 6: Code to describe and show a strategy for imputing missing data

#### Total number of missing values in the dataset

``` r
print(sum(is.na(activity$steps)))
```

    ## [1] 2304

#### Filling in missing values of data set with mean for the 5 minute interval

This tells us the dates with NA’s

``` r
step_sum_by_date %>%
    filter(is.na(stepsum))
```

    ## # A tibble: 8 x 2
    ##   date       stepsum
    ##   <fct>        <int>
    ## 1 2012-10-01      NA
    ## 2 2012-10-08      NA
    ## 3 2012-11-01      NA
    ## 4 2012-11-04      NA
    ## 5 2012-11-09      NA
    ## 6 2012-11-10      NA
    ## 7 2012-11-14      NA
    ## 8 2012-11-30      NA

#### Creating new dataset with missing data filled in

Since we calculated the mean value for every interval in this data
frame, we can use those values to replace the NA values

    step_sum_by_interval_mm[,2] 

Replacing NA’s with means at specified NA rows

``` r
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

``` r
sum(is.na(activity_na_mean))
```

    ## [1] 0

### 7: Histogram of the total number of steps taken each day after missing values are imputed

``` r
step_sum_by_date_na <- activity_na_mean %>%
    group_by(date) %>%
    summarise(stepsum = sum(steps))

ggplot(step_sum_by_date_na, aes(x = stepsum)) +
    geom_histogram(aes(fill = "red"), bins = 60) + 
    labs(title = "Frequency of total steps taken each day over 60 days - Missing data imputed",
         y = "Frequency", x = "Steps") +
    theme(legend.position = "none")
```

![test](/figures/7.png)

Calculating the mean and median steps after missing data imputed

``` r
step_sum_by_date_na_mm <- activity_na_mean %>%
    group_by(date) %>%
    summarise(mean = mean(steps), median = median(steps),
              stepsum = sum(steps))

print.data.frame(step_sum_by_date_na_mm)
```

    ##          date       mean   median  stepsum
    ## 1  2012-10-01 37.3825996 34.11321 10766.19
    ## 2  2012-10-02  0.4375000  0.00000   126.00
    ## 3  2012-10-03 39.4166667  0.00000 11352.00
    ## 4  2012-10-04 42.0694444  0.00000 12116.00
    ## 5  2012-10-05 46.1597222  0.00000 13294.00
    ## 6  2012-10-06 53.5416667  0.00000 15420.00
    ## 7  2012-10-07 38.2465278  0.00000 11015.00
    ## 8  2012-10-08 37.3825996 34.11321 10766.19
    ## 9  2012-10-09 44.4826389  0.00000 12811.00
    ## 10 2012-10-10 34.3750000  0.00000  9900.00
    ## 11 2012-10-11 35.7777778  0.00000 10304.00
    ## 12 2012-10-12 60.3541667  0.00000 17382.00
    ## 13 2012-10-13 43.1458333  0.00000 12426.00
    ## 14 2012-10-14 52.4236111  0.00000 15098.00
    ## 15 2012-10-15 35.2048611  0.00000 10139.00
    ## 16 2012-10-16 52.3750000  0.00000 15084.00
    ## 17 2012-10-17 46.7083333  0.00000 13452.00
    ## 18 2012-10-18 34.9166667  0.00000 10056.00
    ## 19 2012-10-19 41.0729167  0.00000 11829.00
    ## 20 2012-10-20 36.0937500  0.00000 10395.00
    ## 21 2012-10-21 30.6284722  0.00000  8821.00
    ## 22 2012-10-22 46.7361111  0.00000 13460.00
    ## 23 2012-10-23 30.9652778  0.00000  8918.00
    ## 24 2012-10-24 29.0104167  0.00000  8355.00
    ## 25 2012-10-25  8.6527778  0.00000  2492.00
    ## 26 2012-10-26 23.5347222  0.00000  6778.00
    ## 27 2012-10-27 35.1354167  0.00000 10119.00
    ## 28 2012-10-28 39.7847222  0.00000 11458.00
    ## 29 2012-10-29 17.4236111  0.00000  5018.00
    ## 30 2012-10-30 34.0937500  0.00000  9819.00
    ## 31 2012-10-31 53.5208333  0.00000 15414.00
    ## 32 2012-11-01 37.3825996 34.11321 10766.19
    ## 33 2012-11-02 36.8055556  0.00000 10600.00
    ## 34 2012-11-03 36.7048611  0.00000 10571.00
    ## 35 2012-11-04 37.3825996 34.11321 10766.19
    ## 36 2012-11-05 36.2465278  0.00000 10439.00
    ## 37 2012-11-06 28.9375000  0.00000  8334.00
    ## 38 2012-11-07 44.7326389  0.00000 12883.00
    ## 39 2012-11-08 11.1770833  0.00000  3219.00
    ## 40 2012-11-09 37.3825996 34.11321 10766.19
    ## 41 2012-11-10 37.3825996 34.11321 10766.19
    ## 42 2012-11-11 43.7777778  0.00000 12608.00
    ## 43 2012-11-12 37.3784722  0.00000 10765.00
    ## 44 2012-11-13 25.4722222  0.00000  7336.00
    ## 45 2012-11-14 37.3825996 34.11321 10766.19
    ## 46 2012-11-15  0.1423611  0.00000    41.00
    ## 47 2012-11-16 18.8923611  0.00000  5441.00
    ## 48 2012-11-17 49.7881944  0.00000 14339.00
    ## 49 2012-11-18 52.4652778  0.00000 15110.00
    ## 50 2012-11-19 30.6979167  0.00000  8841.00
    ## 51 2012-11-20 15.5277778  0.00000  4472.00
    ## 52 2012-11-21 44.3993056  0.00000 12787.00
    ## 53 2012-11-22 70.9270833  0.00000 20427.00
    ## 54 2012-11-23 73.5902778  0.00000 21194.00
    ## 55 2012-11-24 50.2708333  0.00000 14478.00
    ## 56 2012-11-25 41.0902778  0.00000 11834.00
    ## 57 2012-11-26 38.7569444  0.00000 11162.00
    ## 58 2012-11-27 47.3819444  0.00000 13646.00
    ## 59 2012-11-28 35.3576389  0.00000 10183.00
    ## 60 2012-11-29 24.4687500  0.00000  7047.00
    ## 61 2012-11-30 37.3825996 34.11321 10766.19

The impact of replacing the NA values with the mean values is that:  
1. We no longer have medians of all 0  
2. Increases overall mean value for each interval  
3. Frequency spikes at the mean

### 8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Using mutate and ifelse to make a new column that identifies whether a
particular date is a Weekday or Weekend

``` r
activity_factor <- activity

activity_factor$date <- as.Date(activity_factor$date)

weekdays_list <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                   "Friday")

activity_factor <- activity_factor %>%
    mutate(daytype = ifelse(weekdays(activity_factor$date) %in% weekdays_list, "Weekday", "Weekend"))
```

Panel plot - Grouped data to use with plot

``` r
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

![test](/figures/8.png)
