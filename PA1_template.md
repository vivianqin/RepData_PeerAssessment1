

```r
---
title: "CourseProject1"
output: html_document
---
```

```
## Error: <text>:6:0: unexpected end of input
## 4: ---
## 5: 
##   ^
```


Read the data and prepare for analysis 


```r
library(data.table)
```

```
## data.table 1.10.4
```

```
## **********
## This installation of data.table has not detected OpenMP support. It will still work but in single-threaded mode. If this a Mac and you obtained the Mac binary of data.table from CRAN, CRAN's Mac does not yet support OpenMP. In the meantime please follow our Mac installation instructions on the data.table homepage. If it works and you observe benefits from multiple threads as others have reported, please convince Simon Ubanek by sending him evidence and ask him to turn on OpenMP support when CRAN builds package binaries for Mac. Alternatives are to install Ubuntu on your Mac (which I have done and works well) or use Windows where OpenMP is supported and works well.
## **********
```

```
##   The fastest way to learn (by data.table authors): https://www.datacamp.com/courses/data-analysis-the-data-table-way
```

```
##   Documentation: ?data.table, example(data.table) and browseVignettes("data.table")
```

```
##   Release notes, videos and slides: http://r-datatable.com
```

```r
d= read.csv("activity.csv", sep=",", header=TRUE) 
dim(d)
```

```
## [1] 17568     3
```

```r
str(d)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

1.  What is mean total number of steps taken per day? 


```r
library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## data.table + dplyr code now lives in dtplyr.
## Please library(dtplyr)!
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
d$date<- as.Date(d$date)  ##covert chr to date


TotalSteps<- d%>%
  group_by(date)%>%
        filter(!is.na(steps))%>%
        summarise(total_steps = sum(steps, na.rm=TRUE))
TotalSteps
```

```
## # A tibble: 53 × 2
##          date total_steps
##        <date>       <int>
## 1  2012-10-02         126
## 2  2012-10-03       11352
## 3  2012-10-04       12116
## 4  2012-10-05       13294
## 5  2012-10-06       15420
## 6  2012-10-07       11015
## 7  2012-10-09       12811
## 8  2012-10-10        9900
## 9  2012-10-11       10304
## 10 2012-10-12       17382
## # ... with 43 more rows
```

Plot 

```r
library(ggplot2)

ggplot(TotalSteps, aes(x = total_steps)) +
        geom_histogram(fill = "red", binwidth = 800) +
        labs(title = "Steps Per Day", x = "Total Steps", y = "Frequency")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

calculate mean and median 

```r
meansteps<- mean(TotalSteps$total_steps, na.rm=TRUE)
mediansteps<- median(TotalSteps$total_steps, na.rm=TRUE)

meansteps
```

```
## [1] 10766.19
```

```r
mediansteps
```

```
## [1] 10765
```

2. What is the average daily activity pattern? 

Create interval data 

```r
Intervald<- d%>%
        group_by(interval)%>%
        filter(!is.na(steps))%>%
        summarise(avg_steps = mean(steps, na.rm=TRUE))
Intervald
```

```
## # A tibble: 288 × 2
##    interval avg_steps
##       <int>     <dbl>
## 1         0 1.7169811
## 2         5 0.3396226
## 3        10 0.1320755
## 4        15 0.1509434
## 5        20 0.0754717
## 6        25 2.0943396
## 7        30 0.5283019
## 8        35 0.8679245
## 9        40 0.0000000
## 10       45 1.4716981
## # ... with 278 more rows
```

Make a time seris plot

```r
ggplot(Intervald, aes(x =interval , y=avg_steps)) +
        geom_line(color="red", size=1) +
        labs(title = "Average Daily Activity", x = "Interval", y = "Steps per day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

which interval contains the max 

```r
Intervald[which.max(Intervald$avg_steps),]
```

```
## # A tibble: 1 × 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835  206.1698
```

3. Inputting missing values 

calculate the total number of missing values 

```r
sum(is.na(d$steps))
```

```
## [1] 2304
```

fill in missing values by using mean and create a new dataset d2

```r
d2<- d
neg<- is.na(d2$steps)
avg_interval<- tapply(d2$steps, d2$interval, mean, na.rm=TRUE, simplify = TRUE)
d2$steps[neg] <- avg_interval[as.character(d2$interval[neg])]
sum(is.na(d$steps))
```

```
## [1] 2304
```

```r
sum(is.na(d2$steps))
```

```
## [1] 0
```

Make a histogram of total steps taken each day 

```r
TotalSteps2<- d2%>%
  group_by(date)%>%
        filter(!is.na(steps))%>%
        summarise(total_steps2 = sum(steps, na.rm=TRUE))
TotalSteps2
```

```
## # A tibble: 61 × 2
##          date total_steps2
##        <date>        <dbl>
## 1  2012-10-01     10766.19
## 2  2012-10-02       126.00
## 3  2012-10-03     11352.00
## 4  2012-10-04     12116.00
## 5  2012-10-05     13294.00
## 6  2012-10-06     15420.00
## 7  2012-10-07     11015.00
## 8  2012-10-08     10766.19
## 9  2012-10-09     12811.00
## 10 2012-10-10      9900.00
## # ... with 51 more rows
```

```r
ggplot(TotalSteps2, aes(x = total_steps2)) +
        geom_histogram(fill = "red", binwidth = 800) +
        labs(title = "Steps Per Day (NA inputted)", x = "Total Steps", y = "Frequency")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

Do the values differ from the original dataset? They do differ but only a little. 

```r
meansteps2<- mean(TotalSteps2$total_steps2, na.rm=TRUE)
mediansteps2<- median(TotalSteps2$total_steps2, na.rm=TRUE)

meansteps
```

```
## [1] 10766.19
```

```r
meansteps2
```

```
## [1] 10766.19
```

```r
mediansteps
```

```
## [1] 10765
```

```r
mediansteps2
```

```
## [1] 10766.19
```

4. Are there differences in activity patterns between weekdays and weekends? 

```r
d2<- d2%>%
        mutate(daytype= ifelse(weekdays(d2$date)=="Saturday" | weekdays(d2$date)=="Sunday", "Weekend", "Weekday"))
head(d2)
```

```
##       steps       date interval daytype
## 1 1.7169811 2012-10-01        0 Weekday
## 2 0.3396226 2012-10-01        5 Weekday
## 3 0.1320755 2012-10-01       10 Weekday
## 4 0.1509434 2012-10-01       15 Weekday
## 5 0.0754717 2012-10-01       20 Weekday
## 6 2.0943396 2012-10-01       25 Weekday
```

Make a plot for both day types. 

```r
Intervald2<- d2%>%
        group_by(interval, daytype)%>%
        summarise(avg_steps2 = mean(steps, na.rm=TRUE))
head(Intervald2)
```

```
## Source: local data frame [6 x 3]
## Groups: interval [3]
## 
##   interval daytype avg_steps2
##      <int>   <chr>      <dbl>
## 1        0 Weekday 2.25115304
## 2        0 Weekend 0.21462264
## 3        5 Weekday 0.44528302
## 4        5 Weekend 0.04245283
## 5       10 Weekday 0.17316562
## 6       10 Weekend 0.01650943
```

```r
plot<- ggplot(Intervald2, aes(x =interval , y=avg_steps2, color=daytype)) +
       geom_line() +
       labs(title = "Daily Steps by Day Type", x = "Interval", y = "# of Steps") +
       facet_wrap(~daytype, ncol = 1, nrow=2)
print(plot)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

Thank you for reading through. 
```
```

