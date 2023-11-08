Bellabeat Case Study
================
Leopoldine Mirtil

### Disclaimer

This analysis was made as part of the *Bellabeat Case Study: How Can a
Wellness Technology Company Play It Smart?*, offered through the Google
Data Analytics Certificate program on Coursera.com. The data is from the
[FitBit Fitness Tracker
Data](https://www.kaggle.com/datasets/arashnic/fitbit?resource=download)
and was originally uploaded to Kaggle.com by the user Möbius for public
use. The data covers one month of collection from 4/12/2016 to 5/12/2016
from over 30 consenting users.

## Introduction

#### Scenario

You are a junior data analyst working on the marketing analyst team at
Bellabeat, a high-tech manufacturer of health-focused products for
women. Bellabeat is a successful small company, but they have the
potential to become a larger player in the global smart device market.
Urška Sršen, cofounder and Chief Creative Officer of Bellabeat, believes
that analyzing smart device fitness data could help unlock new growth
opportunities for the company. You have been asked to focus on one of
Bellabeat’s products and analyze smart device data to gain insight into
how consumers are using their smart devices. The insights you discover
will then help guide marketing strategy for the company. You will
present your analysis to the Bellabeat executive team along with your
high-level recommendations for Bellabeat’s marketing strategy.

#### Characters

- **Urška** **Sršen:** Bellabeat’s cofounder and Chief Creative Officer

- **Sando** **Mur:** Mathematician and Bellabeat’s cofounder; key member
  of the Bellabeat executive team

- **Bellabeat** **marketing** **analytics** **team:** A team of data
  analysts responsible for collecting, analyzing, and reporting data
  that helps guide Bellabeat’s marketing strategy. You joined this team
  six months ago and have been busy learning about Bellabeat’’s mission
  and business goals — as well as how you, as a junior data analyst, can
  help Bellabeat achieve them.

#### Products

- **Bellabeat** **app:** The Bellabeat app provides users with health
  data related to their activity, sleep, stress, menstrual cycle, and
  mindfulness habits. This data can help users better understand their
  current habits and make healthy decisions. The Bellabeat app connects
  to their line of smart wellness products.

- **Leaf:** Bellabeat’s classic wellness tracker can be worn as a
  bracelet, necklace, or clip. The Leaf tracker connects to the
  Bellabeat app to track activity, sleep, and stress.

- **Time:** This wellness watch combines the timeless look of a classic
  timepiece with smart technology to track user activity, sleep, and
  stress. The Time watch connects to the Bellabeat app to provide you
  with insights into your daily wellness.

- **Spring:** This is a water bottle that tracks daily water intake
  using smart technology to ensure that you are appropriately hydrated
  throughout the day. The Spring bottle connects to the Bellabeat app to
  track your hydration levels.

- **Bellabeat** **membership:** Bellabeat also offers a
  subscription-based membership program for users. Membership gives
  users 24/7 access to fully personalized guidance on nutrition,
  activity, sleep, health and beauty, and mindfulness based on their
  lifestyle and goals.

### Objective

Sršen asks you to analyze smart device usage data in order to gain
insight into how consumers use non-Bellabeat smart devices. She then
wants you to select one Bellabeat product to apply these insights to in
your presentation.

1.  What are some trends in smart device usage?
2.  How could these trends apply to Bellabeat customers?
3.  How could these trends help influence Bellabeat marketing strategy?

## Get to Work

### Step 1 - Import Data

#### Load Packages

``` r
library(tidyr)
library(tidyverse)
library(dplyr)
library(knitr)
library(lubridate)
library(chron)
library(pastecs)
library(ggplot2)
library(cowplot)
```

I chose these specific packages to enable: data manipulation,
documentation, descriptive statistics and visualization.

#### Set Directory & Import Data

``` r
setwd('C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Bellabeat/00_raw_data')

dailyActs <- read.csv('dailyActivity_merged.csv')
dailyCals <- read.csv('dailyCalories_merged.csv')
dailyInts <- read.csv('dailyIntensities_merged.csv')
dailySteps <- read.csv('dailySteps_merged.csv')
hrate_sec <- read.csv('heartrate_seconds_merged.csv')
hrCals <- read.csv('hourlyCalories_merged.csv')
hrInts <- read.csv('hourlyIntensities_merged.csv')
hrSteps <- read.csv('hourlySteps_merged.csv')
minCalsN <- read.csv('minuteCaloriesNarrow_merged.csv')  
minCalsW <- read.csv('minuteCaloriesWide_merged.csv')
minIntsN <- read.csv('minuteIntensitiesNarrow_merged.csv')
minIntsW <- read.csv('minuteIntensitiesWide_merged.csv')
minMETsN <- read.csv('minuteMETsNarrow_merged.csv')
minSleep <- read.csv('minuteSleep_merged.csv')
minStepsN <- read.csv('minuteStepsNarrow_merged.csv')
minStepsW <- read.csv('minuteStepsWide_merged.csv')
sleepDay <- read.csv('sleepDay_merged.csv')
weightLog <- read.csv('weightLogInfo_merged.csv')
```

I set the directory first to make it easier to import all the data files
without having to include the complete file path every time.

### Step 2 - Clean Data

#### Review Daily Dataframes

I noticed that the dailyActs file had columns that were similar to those
in the other daily data files. First, I decided to confirm if these
columns were identical, and if so, remove those files.

##### Compare Daily Dataframes

``` r
##dailyActs vs dailyCals
# TRUE = equal, False=not equal
all.equal(dailyActs$Calories, dailyCals$Calories) 
```

    ## [1] TRUE

``` r
all.equal(dailyActs$Id, dailyCals$Id)
```

    ## [1] TRUE

``` r
all.equal(dailyActs$ActivityDate, dailyCals$ActivityDay)
```

    ## [1] TRUE

``` r
##dailyActs vs dailySteps
all.equal(dailyActs$TotalSteps, dailySteps$StepTotal) 
```

    ## [1] TRUE

``` r
all.equal(dailyActs$Id, dailySteps$Id)
```

    ## [1] TRUE

``` r
all.equal(dailyActs$ActivityDate, dailySteps$ActivityDay)
```

    ## [1] TRUE

``` r
#dailyActs vs dailyInts
all.equal(dailyActs$Id, dailyInts$Id)
```

    ## [1] TRUE

``` r
all.equal(dailyActs$ActivityDate, dailyInts$ActivityDay)
```

    ## [1] TRUE

``` r
#dailyActs vs dailyInts
##convert columns into data tables for comparison
d_Acts <- data.table::setDT(dailyActs[7:14])
d_Ints <- data.table::setDT(dailyInts[3:10])

all.equal(d_Acts, d_Ints, ignore.col.order = TRUE)
```

    ## [1] TRUE

I needed to convert the remaining columns in dailyActs & dailyInts data
frames into data tables (due to their different column orders) in order
to perform a comparison.

``` r
#compare similar columns 'TotalDistance' vs 'TrackerDistance'
tod <- data.table::setDT(dailyActs[5])
trd <- data.table::setDT(dailyActs[6])

all.equal(tod, trd)
```

    ## [1] "Different column names"

``` r
#remove a column
dailyActs <- select(dailyActs, -c(TotalDistance))
```

I removed the TotalDistance column after confirming it was identical to
the TrackerDistance column. I kept this column because it has the more
illustrative name of the two.

##### Modify Daily Dataframe

``` r
dailyActs$ActivityDate <- as.Date(dailyActs$ActivityDate, '%m/%d/%Y') 
```

After removing the unnecessary ‘daily’ files, I changed the data type of
the ActivityDate column as I will use it as a key point when merging the
data sets later on.

#### Review Hourly Dataframes

##### Compare Hourly Dataframes

``` r
#check Id columns
all.equal(hrCals$Id, hrInts$Id)
```

    ## [1] TRUE

``` r
all.equal(hrSteps$Id, hrInts$Id)
```

    ## [1] TRUE

``` r
#check Activity columns
all.equal(hrCals$ActivityHour, hrInts$ActivityHour)
```

    ## [1] TRUE

``` r
all.equal(hrSteps$ActivityHour, hrInts$ActivityHour)
```

    ## [1] TRUE

##### Merge Hourly Dataframes

``` r
hourlyActs <- bind_cols(hrCals, hrSteps[3], hrInts[3:4])

# view data
str(hourlyActs)
```

    ## 'data.frame':    22099 obs. of  6 variables:
    ##  $ Id              : num  1503960366 1503960366 1503960366 1503960366 1503960366 ...
    ##  $ ActivityHour    : chr  "4/12/2016 12:00:00 AM" "4/12/2016 1:00:00 AM" "4/12/2016 2:00:00 AM" "4/12/2016 3:00:00 AM" ...
    ##  $ Calories        : int  81 61 59 47 48 48 48 47 68 141 ...
    ##  $ StepTotal       : int  373 160 151 0 0 0 0 0 250 1864 ...
    ##  $ TotalIntensity  : int  20 8 7 0 0 0 0 0 13 30 ...
    ##  $ AverageIntensity: num  0.333 0.133 0.117 0 0 ...

#### Review Minute Dataframes

##### Compare Minute Dataframes

``` r
# check Id&ActivityMinute columns 
all.equal(minCalsN[1:2],minStepsN[1:2])
```

    ## [1] TRUE

``` r
all.equal(minIntsN[1:2],minMETsN[1:2])
```

    ## [1] TRUE

``` r
all.equal(minCalsN[1:2],minMETsN[1:2])
```

    ## [1] TRUE

``` r
all.equal(minCalsW[1:2],minStepsW[1:2])
```

    ## [1] TRUE

``` r
all.equal(minIntsW[1:2],minCalsW[1:2])
```

    ## [1] TRUE

##### Merge Minute-Narrow Dataframes

``` r
minActs <- bind_cols(minCalsN, minStepsN[3], minIntsN[3], minMETsN[3])

str(minActs) 
```

    ## 'data.frame':    1325580 obs. of  6 variables:
    ##  $ Id            : num  1503960366 1503960366 1503960366 1503960366 1503960366 ...
    ##  $ ActivityMinute: chr  "4/12/2016 12:00:00 AM" "4/12/2016 12:01:00 AM" "4/12/2016 12:02:00 AM" "4/12/2016 12:03:00 AM" ...
    ##  $ Calories      : num  0.786 0.786 0.786 0.786 0.786 ...
    ##  $ Steps         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Intensity     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ METs          : int  10 10 10 10 10 12 12 12 12 12 ...

##### Merge Minute-Wide Dataframes

``` r
minWide <- bind_cols(minCalsW, minIntsW[,3:62], minStepsW[,3:62]) 
```

### Step 3 - Modify Data

#### Rename Columns

``` r
dailyActs <- rename(dailyActs, DailyActsDate=ActivityDate, DailyId=Id)
sleepDay <- rename(sleepDay, DateTime=SleepDay, SleepDayId=Id)
hourlyActs <- rename(hourlyActs, DateTime=ActivityHour, TotalSteps=StepTotal, HourlyId=Id)
hrate_sec <- rename(hrate_sec, HRateId=Id, DateTime=Time, HeartRate=Value)
minActs <- rename(minActs, MinActsId=Id, DateTime=ActivityMinute)
minSleep <- rename(minSleep, MinSleepId=Id, DateTime=date, Sleep=value)
weightLog <- rename(weightLog, WeightId=Id, DateTime=Date)
minWide <- rename(minWide, DateTime=ActivityHour)
```

I changed the names of these columns to be more identifiable when
modifying and merging the data.

#### Remove Columns

``` r
minSleep <- select(minSleep, -c(logId))
weightLog <- select(weightLog, -c(LogId))
```

I removed these columns because they would be redundant as the Id column
exists and would allow easier analysis.

#### Convert Logical Values to Integers

``` r
weightLog$IsManualReport <- as.integer(as.logical(weightLog$IsManualReport))
```

I converted the logical values of the column into integers where TRUE=1
and FALSE=0 to enable easier analysis of this feature.

#### Add New Columns

##### Add Date-Only Columns

``` r
hourlyActs$HourlyDate <- as.Date(hourlyActs$DateTime, '%m/%d/%Y')
minActs$MinuteDate <- as.Date(minActs$DateTime, '%m/%d/%Y')
minSleep$MinSleepDate <- as.Date(minSleep$DateTime, '%m/%d/%Y')
sleepDay$SleepDate <- as.Date(sleepDay$DateTime, '%m/%d/%Y')
weightLog$WeightDate <- as.Date(weightLog$DateTime, '%m/%d/%Y')
hrate_sec$HRateDate <- as.Date(hrate_sec$DateTime, '%m/%d/%Y')
minWide$Date <- as.Date(minWide$DateTime, '%m/%d/%Y %r')
```

##### Add Time-Only Columns

``` r
hourlyActs$Hour <- chron(times.=(format(strptime(hourlyActs$DateTime, '%m/%d/%Y %r'), '%H:%M:%S')))
minActs$Minutes<- chron(times.=(format(strptime(minActs$DateTime, '%m/%d/%Y %r'), '%H:%M:%S')))
minSleep$SleepMinutes <- chron(times.=(format(strptime(minSleep$DateTime, '%m/%d/%Y %r'), '%H:%M:%S')))
sleepDay$SleepTime <- chron(times.=(format(strptime(sleepDay$DateTime, '%m/%d/%Y %r'), '%H:%M:%S')))
weightLog$WeightTime <- chron(times.=(format(strptime(weightLog$DateTime, '%m/%d/%Y %r'), '%H:%M:%S')))
hrate_sec$HRateTime <- chron(times.=(format(strptime(hrate_sec$DateTime, '%m/%d/%Y %r'), '%H:%M:%S')))
minWide$Time <- chron(times.=(format(strptime(minWide$DateTime, '%m/%d/%Y %r'), '%H:%M:%S')))
dailyActs$Time <- chron(times.=(as.numeric(NA))) 
```

I added separate date and time columns to the data to make it easier to
identify patterns or trends. I added a blank time column to the
dailyActs data frame because it will be a one of the primary columns
when merging the files together.

#### Remove DateTime Columns

``` r
hourlyActs <- select(hourlyActs, -c(DateTime))
minSleep <- select(minSleep, -c(DateTime))
weightLog <- select(weightLog, -c(DateTime))
minActs <-select(minActs, -c(DateTime))
sleepDay <- select(sleepDay, -c(DateTime))
hrate_sec <- select(hrate_sec, -c(DateTime))
minWide <- select(minWide, -c(DateTime))
```

#### Reorganize Columns

``` r
hourlyActs <- hourlyActs[, c(1, 6, 7, 2:5)]
minActs <- minActs[, c(1, 6, 7, 2:5)]
minSleep <- minSleep[, c(1, 3, 4, 2)]
sleepDay <- sleepDay[, c(1, 5, 6, 2:4)]
weightLog <- weightLog[, c(1, 7, 8, 2:6)]
hrate_sec <- hrate_sec[, c(1, 3, 4, 2)]
minWide <- minWide[, c(1, 182, 183, 2:181)]
dailyActs <- dailyActs[, c(1, 2, 15, 14, 3:13)] 
```

I moved the new date and time columns for readability and to make it
easier to merge the data sets all together. I also took the time to move
the Calories column from the end of the dailyActs data frame.

#### Add Suffix to Columns

``` r
library(datawizard)

dailyActs <- data_addsuffix(dailyActs,"_Daily", Calories:SedentaryMinutes)
minActs <- data_addsuffix(minActs,"_Minute", Calories:METs)
minSleep <- data_addsuffix(minSleep ,"_Minute", Sleep)
sleepDay <- data_addsuffix(sleepDay,"_Daily", TotalSleepRecords:TotalTimeInBed)
hourlyActs <- data_addsuffix(hourlyActs,"_Hourly", Calories:AverageIntensity)
weightLog <- data_addsuffix(weightLog,"_Daily", WeightKg:IsManualReport)
hrate_sec <- data_addsuffix(hrate_sec,"_Seconds", HeartRate)
```

Since the data was recorded at different time intervals, I felt that it
would be important to make note of this when analyzing the data later
on. To that end, I added a suffix to the feature columns to mark what
time interval they were recorded in. Later on, I can split the interval
into its own column.

#### Confirm Date Ranges

The data was collected over a one month period from 4/12/2016 to
5/12/2016. I wanted to verify that all the dates of the files are within
range.

``` r
range(dailyActs$DailyActsDate)
```

    ## [1] "2016-04-12" "2016-05-12"

``` r
range(hourlyActs$HourlyDate) 
```

    ## [1] "2016-04-12" "2016-05-12"

``` r
range(minActs$MinuteDate)
```

    ## [1] "2016-04-12" "2016-05-12"

``` r
range(minSleep$MinSleepDate) 
```

    ## [1] "2016-04-11" "2016-05-12"

``` r
range(sleepDay$SleepDate) 
```

    ## [1] "2016-04-12" "2016-05-12"

``` r
range(weightLog$WeightDate) 
```

    ## [1] "2016-04-12" "2016-05-12"

``` r
range(hrate_sec$HRateDate)  
```

    ## [1] "2016-04-12" "2016-05-12"

``` r
range(minWide$Date) 
```

    ## [1] "2016-04-13" "2016-05-13"

The only data sets containing out of range dates are minSleep
(4/11/2016 - 5/12/2016) and minWide (4/13/2016- 5/13/2016).

#### Remove Out of Range Dates

``` r
minSleep <- minSleep[minSleep[['MinSleepDate']]>='2016-04-12', ] 

minWide <- minWide[minWide[['Date']]<='2016-05-12', ]

# confirm date range
range(minSleep$MinSleepDate)
```

    ## [1] "2016-04-12" "2016-05-12"

``` r
range(minWide$Date)
```

    ## [1] "2016-04-13" "2016-05-12"

#### Export Modified Dataframes

``` r
setwd('C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Bellabeat/01_tidy_data')

write.csv(dailyActs, "dailyActs.csv", row.names = FALSE)
write.csv(hourlyActs, "hourlyActs.csv", row.names = FALSE)
write.csv(hrate_sec, "hrate_sec.csv", row.names = FALSE)
write.csv(minActs, "minActs.csv", row.names = FALSE)
write.csv(minSleep, "minSleep.csv", row.names = FALSE)
write.csv(sleepDay, "sleepDay.csv", row.names = FALSE)
write.csv(weightLog, "weightLog.csv", row.names = FALSE)
write.csv(minWide, "minWide.csv", row.names = FALSE)
```

I exported these modified files as a precaution before merging the
remaining data sets together. If anything goes wrong, I would be able to
start over from this point.

#### Merge Dataframes

``` r
#join dailyActs & sleepDay datsets by Id & date as keys
daily <- full_join(dailyActs, sleepDay, by=join_by(DailyId==SleepDayId, DailyActsDate==SleepDate), keep=TRUE) # keep=TRUE keeps matching column

gc()
```

``` r
# join daily data and hourly dataset
## merge smaller dataset to larger data set to keep order in Id, Date and Time
daily_hour <- full_join(daily, hourlyActs, by=join_by(DailyId==HourlyId, DailyActsDate==HourlyDate, SleepTime==Hour), keep=TRUE) 

# clear previous joined files
rm(daily, hourlyActs, dailyActs, sleepDay)
gc()
```

``` r
# join minute datasets together
minutes <- full_join(minActs, minSleep, by=join_by(MinActsId==MinSleepId, MinuteDate==MinSleepDate, Minutes==SleepMinutes), keep=TRUE)

rm(minActs, minSleep)
gc()

# combine daily_hour &  minutes; use hour as next longest available time format
d_hr_min <- full_join(daily_hour, minutes, by=join_by(DailyId==MinActsId, DailyActsDate==MinuteDate, Hour==Minutes), keep=TRUE)
rm(daily_hour, minutes)
gc()

# join d_hr_min + hrate_sec
smarties <- full_join(d_hr_min, hrate_sec, by=join_by(DailyId==HRateId, DailyActsDate==HRateDate, Minutes==HRateTime), keep=TRUE) 

# join smarties + weight Log
smart <- full_join(smarties, weightLog, by=join_by(DailyId==WeightId, DailyActsDate==WeightDate, HRateTime==WeightTime), keep=TRUE) 

#clear environment
rm(smarties, d_hr_min, hrate_sec, weightLog) 
gc()
```

I merged all the remaining files together, with the exception of the
minWide (due to its different format). Since I was creating a large data
file, I cleared the environment and memory in between merging to avoid a
system crash.

#### Export Merged Dataframe

``` r
# set directory
setwd('C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Bellabeat/01_tidy_data')

#export file 
write.csv(smart, 'smart.csv', row.names = FALSE)
```

### Step 4 - Further Modify Combined Dataset

#### Create Primary Id Column

``` r
#new copy in case of error
smartDev <- smart 

smartDev$DailyId <- ifelse(is.na(smartDev$DailyId), smartDev$MinActsId, smartDev$DailyId)
smartDev$DailyId <- ifelse(is.na(smartDev$DailyId), smartDev$HourlyId, smartDev$DailyId)
smartDev$DailyId <- ifelse(is.na(smartDev$DailyId), smartDev$HRateId, smartDev$DailyId)
smartDev$DailyId <- ifelse(is.na(smartDev$DailyId), smartDev$MinSleepId, smartDev$DailyId)
smartDev$DailyId <- ifelse(is.na(smartDev$DailyId), smartDev$WeightId, smartDev$DailyId)


## confirm no more NA values 
sum(is.na(smartDev$DailyId))
```

    ## [1] 0

``` r
# remove other Id columns
smartDev <- select(smartDev, -c(SleepDayId, HourlyId, MinActsId, HRateId, WeightId, MinSleepId))
```

I made the DailyId column the key Id column and filled in the blanks
with the other Id columns(except SleepId which was in range). I
confirmed there were no missing values once completed, then removed the
other unnecessary Id columns.

#### Create Primary Date Column

``` r
smartDev <- 
  smartDev %>% 
    mutate(DailyActsDate=coalesce(DailyActsDate, SleepDate)) %>%
    mutate(DailyActsDate=coalesce(DailyActsDate, MinuteDate)) %>%
    mutate(DailyActsDate=coalesce(DailyActsDate, HourlyDate)) %>%
    mutate(DailyActsDate=coalesce(DailyActsDate, HRateDate)) %>%
    mutate(DailyActsDate=coalesce(DailyActsDate, MinSleepDate)) %>%
    mutate(DailyActsDate=coalesce(DailyActsDate, WeightDate))

## confirm no more NA values 
sum(is.na(smartDev$DailyActsDate))
```

    ## [1] 0

``` r
# remove other date columns
smartDev <- select(smartDev, -c(SleepDate, HourlyDate, MinuteDate, MinSleepDate, HRateDate, WeightDate))
```

I turned the DailyActsDate column into the primary date column, filling
the blanks with the values from the other date columns. I removed the
other columns after confirming that there were no missing values in the
primary column.

#### Create Primary Time Column

``` r
smartDev <- 
  smartDev %>% 
    mutate(Time=coalesce(Time, SleepTime)) %>% 
    mutate(Time=coalesce(Time, Minutes)) %>% 
    mutate(Time=coalesce(Time, Hour)) %>% 
    mutate(Time=coalesce(Time, HRateTime)) %>% 
    mutate(Time=coalesce(Time, SleepMinutes)) %>% 
    mutate(Time=coalesce(Time, WeightTime))

# remove columns
smartDev <- select(smartDev, -c(SleepTime, Hour, Minutes, SleepMinutes, HRateTime, WeightTime))
```

I used the other time columns to fill in the blanks of the primary Time
column. I moved the time column from the end to after the date column
for easier readability.

#### Clean up Modified Dataframe

``` r
#Rename columns
smartDev <- rename(smartDev, Id=DailyId, Date=DailyActsDate)

# clear environment
rm(smart)
gc()
```

I renamed the primary columns to those that are more self-descriptive
and appropriate.

#### View Merged Dataframe

``` r
str(smartDev)
```

    ## 'data.frame':    3893669 obs. of  33 variables:
    ##  $ Id                            : num  1503960366 1503960366 1503960366 1503960366 1503960366 ...
    ##  $ Date                          : Date, format: "2016-04-12" "2016-04-13" ...
    ##  $ Time                          : 'times' num  00:00:00 00:00:00 NA 00:00:00 00:00:00 00:00:00 NA 00:00:00 00:00:00 00:00:00 ...
    ##   ..- attr(*, "format")= chr "h:m:s"
    ##  $ Calories_Daily                : int  1985 1797 1776 1745 1863 1728 1921 2035 1786 1775 ...
    ##  $ TotalSteps_Daily              : int  13162 10735 10460 9762 12669 9705 13019 15506 10544 9819 ...
    ##  $ TrackerDistance_Daily         : num  8.5 6.97 6.74 6.28 8.16 ...
    ##  $ LoggedActivitiesDistance_Daily: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ VeryActiveDistance_Daily      : num  1.88 1.57 2.44 2.14 2.71 ...
    ##  $ ModeratelyActiveDistance_Daily: num  0.55 0.69 0.4 1.26 0.41 ...
    ##  $ LightActiveDistance_Daily     : num  6.06 4.71 3.91 2.83 5.04 ...
    ##  $ SedentaryActiveDistance_Daily : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ VeryActiveMinutes_Daily       : int  25 21 30 29 36 38 42 50 28 19 ...
    ##  $ FairlyActiveMinutes_Daily     : int  13 19 11 34 10 20 16 31 12 8 ...
    ##  $ LightlyActiveMinutes_Daily    : int  328 217 181 209 221 164 233 264 205 211 ...
    ##  $ SedentaryMinutes_Daily        : int  728 776 1218 726 773 539 1149 775 818 838 ...
    ##  $ TotalSleepRecords_Daily       : int  1 2 NA 1 2 1 NA 1 1 1 ...
    ##  $ TotalMinutesAsleep_Daily      : int  327 384 NA 412 340 700 NA 304 360 325 ...
    ##  $ TotalTimeInBed_Daily          : int  346 407 NA 442 367 712 NA 320 377 364 ...
    ##  $ Calories_Hourly               : int  81 69 NA 60 77 47 NA 47 54 54 ...
    ##  $ TotalSteps_Hourly             : int  373 144 NA 83 459 0 NA 0 16 17 ...
    ##  $ TotalIntensity_Hourly         : int  20 14 NA 6 15 0 NA 0 2 2 ...
    ##  $ AverageIntensity_Hourly       : num  0.333 0.233 NA 0.1 0.25 ...
    ##  $ Calories_Minute               : num  0.786 1.888 NA 0.944 4.09 ...
    ##  $ Steps_Minute                  : int  0 4 NA 0 77 0 NA 0 0 0 ...
    ##  $ Intensity_Minute              : int  0 1 NA 0 2 0 NA 0 0 0 ...
    ##  $ METs_Minute                   : int  10 24 NA 12 52 10 NA 10 12 12 ...
    ##  $ Sleep_Minute                  : int  NA NA NA NA NA 1 NA NA NA NA ...
    ##  $ HeartRate_Seconds             : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ WeightKg_Daily                : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ WeightPounds_Daily            : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Fat_Daily                     : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ BMI_Daily                     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ IsManualReport_Daily          : int  NA NA NA NA NA NA NA NA NA NA ...

#### Export Final Merged Dataframe

``` r
setwd('C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Bellabeat/01_tidy_data')

#export final merged file
write.csv(smartDev, 'smartDev.csv', row.names = FALSE)
```

I exported the final merged file as a precaution before manipulating the
files further.

### Step 5 - Final Manipulation of Dataframes

#### Modify Smart Features

##### Pivot Smart Features Dataframe

``` r
# create new dataframe in case of error
smart_d2 <- smartDev

#transpose data 
mod_smart <- 
smart_d2 %>%
  pivot_longer(cols=c(Calories_Daily:IsManualReport_Daily), 
               names_to = 'SmartFeatures', 
               values_to = 'Values', 
               values_drop_na = TRUE)

#view pivoted data
head(mod_smart) 
```

    ## # A tibble: 6 × 5
    ##           Id Date       Time     SmartFeatures                     Values
    ##        <dbl> <date>     <times>  <chr>                              <dbl>
    ## 1 1503960366 2016-04-12 00:00:00 Calories_Daily                  1985    
    ## 2 1503960366 2016-04-12 00:00:00 TotalSteps_Daily               13162    
    ## 3 1503960366 2016-04-12 00:00:00 TrackerDistance_Daily              8.5  
    ## 4 1503960366 2016-04-12 00:00:00 LoggedActivitiesDistance_Daily     0    
    ## 5 1503960366 2016-04-12 00:00:00 VeryActiveDistance_Daily           1.88 
    ## 6 1503960366 2016-04-12 00:00:00 ModeratelyActiveDistance_Daily     0.550

I decided to transpose (or pivot) the data to have all the smart
features in one column and their values in another.

##### Create Interval Column

``` r
mod_smart <-
mod_smart %>%
  separate_wider_delim(SmartFeatures, "_", names=c('SmartFeatures', 'Interval'))

head(mod_smart)
```

    ## # A tibble: 6 × 6
    ##           Id Date       Time     SmartFeatures            Interval    Values
    ##        <dbl> <date>     <times>  <chr>                    <chr>        <dbl>
    ## 1 1503960366 2016-04-12 00:00:00 Calories                 Daily     1985    
    ## 2 1503960366 2016-04-12 00:00:00 TotalSteps               Daily    13162    
    ## 3 1503960366 2016-04-12 00:00:00 TrackerDistance          Daily        8.5  
    ## 4 1503960366 2016-04-12 00:00:00 LoggedActivitiesDistance Daily        0    
    ## 5 1503960366 2016-04-12 00:00:00 VeryActiveDistance       Daily        1.88 
    ## 6 1503960366 2016-04-12 00:00:00 ModeratelyActiveDistance Daily        0.550

I created this new column by splitting the ‘SmartFeaures’ column to
remove the interval suffix I had added earlier.

##### Add Category Column

``` r
mod_smart$Category <- with(mod_smart, ifelse(SmartFeatures == "Calories", 'Calories',
                            ifelse(SmartFeatures == "METs", 'METs',
                            ifelse(SmartFeatures == "Sleep" |SmartFeatures =="TotalSleepRecords"
                                   |SmartFeatures =="TotalMinutesAsleep"
                                   |SmartFeatures =="TotalTimeInBed", 'Sleep', 
                            ifelse(SmartFeatures == "Steps" |SmartFeatures =="TotalSteps", 'Steps', 
                            ifelse(SmartFeatures == "WeightKg"|SmartFeatures =="WeightPounds"
                                   |SmartFeatures =="Fat"|SmartFeatures =="BMI"
                                   |SmartFeatures =="IsManualReport", 'Weight', 
                            ifelse(SmartFeatures =="HeartRate", 'Heart Rate', 'Intensity')))))))
```

I added the Category column in order to group related features which
would allow easier analysis of the features.

##### Reorganize Columns

``` r
mod_smart <- mod_smart[, c(1:4, 6, 7, 5)] 
head(mod_smart)
```

    ## # A tibble: 6 × 7
    ##           Id Date       Time     SmartFeatures          Values Category Interval
    ##        <dbl> <date>     <times>  <chr>                   <dbl> <chr>    <chr>   
    ## 1 1503960366 2016-04-12 00:00:00 Calories              1.99e+3 Calories Daily   
    ## 2 1503960366 2016-04-12 00:00:00 TotalSteps            1.32e+4 Steps    Daily   
    ## 3 1503960366 2016-04-12 00:00:00 TrackerDistance       8.5 e+0 Intensi… Daily   
    ## 4 1503960366 2016-04-12 00:00:00 LoggedActivitiesDist… 0       Intensi… Daily   
    ## 5 1503960366 2016-04-12 00:00:00 VeryActiveDistance    1.88e+0 Intensi… Daily   
    ## 6 1503960366 2016-04-12 00:00:00 ModeratelyActiveDist… 5.50e-1 Intensi… Daily

#### Modify Minute-Wide Features Dataframe

##### Pivot Minute-Wide Features Dataframe

``` r
## split Cals00-59, Steps00-59, Ints00-59 
mod_minWide <-
minWide %>%
pivot_longer(
    cols=!c(Id, Date, Time),
    names_to = c('Features', '.value'), 
    names_pattern = '(.*)(.\\d)')

#view pivoted data
head(mod_minWide)
```

    ## # A tibble: 6 × 64
    ##          Id Date       Time  Features  `00`   `01`  `02`  `03`  `04`  `05`  `06`
    ##       <dbl> <date>     <tim> <chr>    <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1    1.50e9 2016-04-13 00:0… Calories 1.89   2.20  0.944 0.944 0.944 2.04  0.944
    ## 2    1.50e9 2016-04-13 00:0… Intensi… 1      1     0     0     0     1     0    
    ## 3    1.50e9 2016-04-13 00:0… Steps    4     16     0     0     0     9     0    
    ## 4    1.50e9 2016-04-13 01:0… Calories 0.786  0.786 0.786 0.786 0.944 0.944 0.944
    ## 5    1.50e9 2016-04-13 01:0… Intensi… 0      0     0     0     0     0     0    
    ## 6    1.50e9 2016-04-13 01:0… Steps    0      0     0     0     0     0     0    
    ## # ℹ 53 more variables: `07` <dbl>, `08` <dbl>, `09` <dbl>, `10` <dbl>,
    ## #   `11` <dbl>, `12` <dbl>, `13` <dbl>, `14` <dbl>, `15` <dbl>, `16` <dbl>,
    ## #   `17` <dbl>, `18` <dbl>, `19` <dbl>, `20` <dbl>, `21` <dbl>, `22` <dbl>,
    ## #   `23` <dbl>, `24` <dbl>, `25` <dbl>, `26` <dbl>, `27` <dbl>, `28` <dbl>,
    ## #   `29` <dbl>, `30` <dbl>, `31` <dbl>, `32` <dbl>, `33` <dbl>, `34` <dbl>,
    ## #   `35` <dbl>, `36` <dbl>, `37` <dbl>, `38` <dbl>, `39` <dbl>, `40` <dbl>,
    ## #   `41` <dbl>, `42` <dbl>, `43` <dbl>, `44` <dbl>, `45` <dbl>, `46` <dbl>, …

Since the columns were originally in the “Feature##” format, with “\##”
covering minutes ‘00-59’, it made sense to split the columns between the
words and numbers.

##### Create Minutes Column

``` r
mod_minWide <-
mod_minWide %>%
pivot_longer(cols = !c(Id, Date, Time, Features),
             names_to = 'Minutes',
             values_to = 'Value')

#move 'Minutes' after Time(contains 'hours')
mod_minWide <- mod_minWide[,c(1:3,5,4,6)] 
mod_minWide$Minutes <- chron(times.=(format(strptime(mod_minWide$Minutes, '%M'), '%H:%M:%S')))

#view pivoted data
head(mod_minWide)
```

    ## # A tibble: 6 × 6
    ##           Id Date       Time     Minutes  Features Value
    ##        <dbl> <date>     <times>  <times>  <chr>    <dbl>
    ## 1 1503960366 2016-04-13 00:00:00 00:00:00 Calories 1.89 
    ## 2 1503960366 2016-04-13 00:00:00 00:01:00 Calories 2.20 
    ## 3 1503960366 2016-04-13 00:00:00 00:02:00 Calories 0.944
    ## 4 1503960366 2016-04-13 00:00:00 00:03:00 Calories 0.944
    ## 5 1503960366 2016-04-13 00:00:00 00:04:00 Calories 0.944
    ## 6 1503960366 2016-04-13 00:00:00 00:05:00 Calories 2.04

I created the new Minutes column from the ‘00-59’ column range. I also
moved the feature values into their own column. I also took the time to
change the data type of the Minutes column and move it in preparation of
merging both time columns.

##### Merge Time Columns

``` r
mod_minWide <-
mod_minWide %>% 
  mutate(Hour=hours(Time), Minutes=minutes(Minutes)) %>%
  unite(Time, Hour, Minutes, sep=':') %>%
  mutate(Time=chron(times.=(format(strptime(Time, '%H:%M'), '%H:%M:%S'))))

# view final modified data frame
head(mod_minWide)
```

    ## # A tibble: 6 × 5
    ##           Id Date       Time     Features Value
    ##        <dbl> <date>     <times>  <chr>    <dbl>
    ## 1 1503960366 2016-04-13 00:00:00 Calories 1.89 
    ## 2 1503960366 2016-04-13 00:01:00 Calories 2.20 
    ## 3 1503960366 2016-04-13 00:02:00 Calories 0.944
    ## 4 1503960366 2016-04-13 00:03:00 Calories 0.944
    ## 5 1503960366 2016-04-13 00:04:00 Calories 0.944
    ## 6 1503960366 2016-04-13 00:05:00 Calories 2.04

#### Export Final Modified Files

``` r
setwd('C:/Users/Leopoldine/Desktop/Mine/Coding Projects & Portfolio/Bellabeat/01_tidy_data')

write.csv(mod_smart, 'mod_smart.csv', row.names = FALSE)
write.csv(mod_minWide, 'mod_minWide.csv', row.names = FALSE)
```

I made sure to export both files after completing all of the data
modifications.

### Step 5 - Analyze Data

#### Descriptive Analysis of Smart Features

``` r
round(stat.desc(mod_smart), 2)
```

    ##                                  Id            Date       Time SmartFeatures
    ## nbr.val                  8077043.00      8077043.00 8070683.00            NA
    ## nbr.null                       0.00            0.00   14024.00            NA
    ## nbr.na                         0.00            0.00    6360.00            NA
    ## min                   1503960366.00        16903.00       0.00            NA
    ## max                   8877689391.00        16933.00       1.00            NA
    ## range                 7373729025.00           30.00       1.00            NA
    ## sum            40839429581798520.00 136640198476.00 4065016.41            NA
    ## median                4702921684.00        16917.00       0.51            NA
    ## mean                  5056235256.12        16917.11       0.50            NA
    ## SE.mean                   809086.64            0.00       0.00            NA
    ## CI.mean.0.95             1585780.92            0.01       0.00            NA
    ## var          5287403540259544064.00           74.78       0.08            NA
    ## std.dev               2299435482.95            8.65       0.28            NA
    ## coef.var                       0.45            0.00       0.56            NA
    ##                    Values Category Interval
    ## nbr.val        8077043.00       NA       NA
    ## nbr.null       2270097.00       NA       NA
    ## nbr.na               0.00       NA       NA
    ## min                  0.00       NA       NA
    ## max              36019.00       NA       NA
    ## range            36019.00       NA       NA
    ## sum          241703468.86       NA       NA
    ## median               8.00       NA       NA
    ## mean                29.92       NA       NA
    ## SE.mean              0.04       NA       NA
    ## CI.mean.0.95         0.08       NA       NA
    ## var              13543.45       NA       NA
    ## std.dev            116.38       NA       NA
    ## coef.var             3.89       NA       NA

#### Descriptive Analysis of Minute-Wide Features

``` r
round(stat.desc(mod_minWide), 2)
```

    ##                                 Id           Date       Time Features
    ## nbr.val                  3876480.0     3876480.00 3876480.00       NA
    ## nbr.null                       0.0           0.00    2712.00       NA
    ## nbr.na                         0.0           0.00       0.00       NA
    ## min                   1503960366.0       16904.00       0.00       NA
    ## max                   8877689391.0       16933.00       1.00       NA
    ## range                 7373729025.0          29.00       1.00       NA
    ## sum            18761748658369560.0 65581122780.00 1931096.50       NA
    ## median                4445114986.0       16917.00       0.50       NA
    ## mean                  4839893062.4       16917.70       0.50       NA
    ## SE.mean                  1230544.5           0.00       0.00       NA
    ## CI.mean.0.95             2411823.7           0.01       0.00       NA
    ## var          5869920417303835648.0          71.11       0.08       NA
    ## std.dev               2422791864.2           8.43       0.29       NA
    ## coef.var                       0.5           0.00       0.58       NA
    ##                   Value
    ## nbr.val      3876480.00
    ## nbr.null     2179853.00
    ## nbr.na             0.00
    ## min                0.00
    ## max              220.00
    ## range            220.00
    ## sum          9292110.37
    ## median             0.00
    ## mean               2.40
    ## SE.mean            0.01
    ## CI.mean.0.95       0.01
    ## var              115.42
    ## std.dev           10.74
    ## coef.var           4.48

#### User Numbers

##### Smart Features User Count

``` r
mod_smart %>%
  group_by(SmartFeatures)%>%
  summarise(UserCount=length(unique(Id))) %>%
  arrange(desc(UserCount)) %>%
  print(n=27)
```

    ## # A tibble: 27 × 2
    ##    SmartFeatures            UserCount
    ##    <chr>                        <int>
    ##  1 AverageIntensity                33
    ##  2 Calories                        33
    ##  3 FairlyActiveMinutes             33
    ##  4 Intensity                       33
    ##  5 LightActiveDistance             33
    ##  6 LightlyActiveMinutes            33
    ##  7 LoggedActivitiesDistance        33
    ##  8 METs                            33
    ##  9 ModeratelyActiveDistance        33
    ## 10 SedentaryActiveDistance         33
    ## 11 SedentaryMinutes                33
    ## 12 Steps                           33
    ## 13 TotalIntensity                  33
    ## 14 TotalSteps                      33
    ## 15 TrackerDistance                 33
    ## 16 VeryActiveDistance              33
    ## 17 VeryActiveMinutes               33
    ## 18 Sleep                           24
    ## 19 TotalMinutesAsleep              24
    ## 20 TotalSleepRecords               24
    ## 21 TotalTimeInBed                  24
    ## 22 HeartRate                       14
    ## 23 BMI                              8
    ## 24 IsManualReport                   8
    ## 25 WeightKg                         8
    ## 26 WeightPounds                     8
    ## 27 Fat                              2

##### Minute-Wide User Count

``` r
mod_minWide %>%
  group_by(Features)%>%
  summarise(UserCount=length(unique(Id))) %>%
  arrange(desc(UserCount)) 
```

    ## # A tibble: 3 × 2
    ##   Features  UserCount
    ##   <chr>         <int>
    ## 1 Calories         33
    ## 2 Intensity        33
    ## 3 Steps            33

#### Total Use Count

##### Feature Category Usage

``` r
mod_smart %>%
  group_by(Category) %>%
  summarise(Total=n()) %>%
  arrange(desc(Total)) 
```

    ## # A tibble: 7 × 2
    ##   Category     Total
    ##   <chr>        <int>
    ## 1 Heart Rate 2483659
    ## 2 Intensity  1379785
    ## 3 Calories   1349176
    ## 4 Steps      1349176
    ## 5 METs       1326127
    ## 6 Sleep       188850
    ## 7 Weight         270

##### Smart Features Usage by Category

``` r
mod_smart %>%
  group_by(Category, SmartFeatures)%>%
  summarise(Count=n()) %>%
  arrange(Category, desc(Count)) %>%
  print(n=27)
```

    ## # A tibble: 27 × 3
    ## # Groups:   Category [7]
    ##    Category   SmartFeatures              Count
    ##    <chr>      <chr>                      <int>
    ##  1 Calories   Calories                 1349176
    ##  2 Heart Rate HeartRate                2483659
    ##  3 Intensity  Intensity                1326127
    ##  4 Intensity  AverageIntensity           22104
    ##  5 Intensity  TotalIntensity             22104
    ##  6 Intensity  FairlyActiveMinutes          945
    ##  7 Intensity  LightActiveDistance          945
    ##  8 Intensity  LightlyActiveMinutes         945
    ##  9 Intensity  LoggedActivitiesDistance     945
    ## 10 Intensity  ModeratelyActiveDistance     945
    ## 11 Intensity  SedentaryActiveDistance      945
    ## 12 Intensity  SedentaryMinutes             945
    ## 13 Intensity  TrackerDistance              945
    ## 14 Intensity  VeryActiveDistance           945
    ## 15 Intensity  VeryActiveMinutes            945
    ## 16 METs       METs                     1326127
    ## 17 Sleep      Sleep                     187605
    ## 18 Sleep      TotalMinutesAsleep           415
    ## 19 Sleep      TotalSleepRecords            415
    ## 20 Sleep      TotalTimeInBed               415
    ## 21 Steps      Steps                    1326127
    ## 22 Steps      TotalSteps                 23049
    ## 23 Weight     BMI                           67
    ## 24 Weight     IsManualReport                67
    ## 25 Weight     WeightKg                      67
    ## 26 Weight     WeightPounds                  67
    ## 27 Weight     Fat                            2

##### Minute-Wide Features Usage

``` r
mod_minWide %>% count(Features, sort = TRUE) %>% rename(TotalCount=n)
```

    ## # A tibble: 3 × 2
    ##   Features  TotalCount
    ##   <chr>          <int>
    ## 1 Calories     1292160
    ## 2 Intensity    1292160
    ## 3 Steps        1292160

##### Total Count by Interval

``` r
mod_smart %>%
  group_by(Interval) %>%
  summarise(Count=n()) %>%
  arrange(desc(Count))
```

    ## # A tibble: 4 × 2
    ##   Interval   Count
    ##   <chr>      <int>
    ## 1 Minute   5492113
    ## 2 Seconds  2483659
    ## 3 Hourly     88416
    ## 4 Daily      12855

##### Total Count by Category and Interval

``` r
mod_smart %>%
  group_by(Interval, Category) %>%
  summarise(Count=n()) %>%
  arrange(Interval, desc(Count))
```

    ## # A tibble: 14 × 3
    ## # Groups:   Interval [4]
    ##    Interval Category     Count
    ##    <chr>    <chr>        <int>
    ##  1 Daily    Intensity     9450
    ##  2 Daily    Sleep         1245
    ##  3 Daily    Calories       945
    ##  4 Daily    Steps          945
    ##  5 Daily    Weight         270
    ##  6 Hourly   Intensity    44208
    ##  7 Hourly   Calories     22104
    ##  8 Hourly   Steps        22104
    ##  9 Minute   Calories   1326127
    ## 10 Minute   Intensity  1326127
    ## 11 Minute   METs       1326127
    ## 12 Minute   Steps      1326127
    ## 13 Minute   Sleep       187605
    ## 14 Seconds  Heart Rate 2483659

#### Daily Frequency

##### Daily Frequency of Feature Categories

``` r
mod_smart %>% 
  group_by(Date, Category) %>%
  summarise(TotalCount = n()) %>%
  arrange(Date, desc(TotalCount)) 
```

    ## # A tibble: 217 × 3
    ## # Groups:   Date [31]
    ##    Date       Category   TotalCount
    ##    <date>     <chr>           <int>
    ##  1 2016-04-12 Heart Rate      99149
    ##  2 2016-04-12 Intensity       49434
    ##  3 2016-04-12 Calories        48345
    ##  4 2016-04-12 Steps           48345
    ##  5 2016-04-12 METs            47520
    ##  6 2016-04-12 Sleep            6130
    ##  7 2016-04-12 Weight              8
    ##  8 2016-04-13 Heart Rate      72525
    ##  9 2016-04-13 Intensity       49434
    ## 10 2016-04-13 Calories        48345
    ## # ℹ 207 more rows

##### Frequency of Daily-Tracked Features

``` r
mod_smart %>% 
  filter(Interval=="Daily") %>%
  group_by(Date, SmartFeatures) %>%
  summarise(TotalCount = n()) %>%
  arrange(Date, desc(TotalCount)) 
```

    ## # A tibble: 591 × 3
    ## # Groups:   Date [31]
    ##    Date       SmartFeatures            TotalCount
    ##    <date>     <chr>                         <int>
    ##  1 2016-04-12 Calories                         33
    ##  2 2016-04-12 FairlyActiveMinutes              33
    ##  3 2016-04-12 LightActiveDistance              33
    ##  4 2016-04-12 LightlyActiveMinutes             33
    ##  5 2016-04-12 LoggedActivitiesDistance         33
    ##  6 2016-04-12 ModeratelyActiveDistance         33
    ##  7 2016-04-12 SedentaryActiveDistance          33
    ##  8 2016-04-12 SedentaryMinutes                 33
    ##  9 2016-04-12 TotalSteps                       33
    ## 10 2016-04-12 TrackerDistance                  33
    ## # ℹ 581 more rows

#### Weekday Frequency

##### Weekday Frequency of Feature Categories

``` r
mod_smart %>% 
  mutate(Weekday=wday(Date, label=TRUE)) %>%
  group_by(Weekday, Category) %>%
  summarise(TotalCount = n()) %>%
  arrange(Weekday, desc(TotalCount)) 
```

    ## # A tibble: 49 × 3
    ## # Groups:   Weekday [7]
    ##    Weekday Category   TotalCount
    ##    <ord>   <chr>           <int>
    ##  1 Sun     Heart Rate     287147
    ##  2 Sun     Intensity      180762
    ##  3 Sun     Calories       176777
    ##  4 Sun     Steps          176777
    ##  5 Sun     METs           173760
    ##  6 Sun     Sleep           28927
    ##  7 Sun     Weight             41
    ##  8 Mon     Heart Rate     289764
    ##  9 Mon     Intensity      178595
    ## 10 Mon     Calories       174644
    ## # ℹ 39 more rows

##### Weekday Frequency of Smart Features

``` r
mod_smart %>% 
  mutate(Weekday=wday(Date, label=TRUE)) %>%
  group_by(Weekday, SmartFeatures) %>%
  summarise(TotalCount = n()) %>%
  arrange(Weekday, desc(TotalCount)) 
```

    ## # A tibble: 184 × 3
    ## # Groups:   Weekday [7]
    ##    Weekday SmartFeatures       TotalCount
    ##    <ord>   <chr>                    <int>
    ##  1 Sun     HeartRate               287147
    ##  2 Sun     Calories                176777
    ##  3 Sun     Intensity               173760
    ##  4 Sun     METs                    173760
    ##  5 Sun     Steps                   173760
    ##  6 Sun     Sleep                    28762
    ##  7 Sun     TotalSteps                3017
    ##  8 Sun     AverageIntensity          2896
    ##  9 Sun     TotalIntensity            2896
    ## 10 Sun     FairlyActiveMinutes        121
    ## # ℹ 174 more rows

##### Weekday Frequency of Minute-Wide Features

``` r
mod_minWide %>% 
  mutate(Weekday=wday(Date, label=TRUE)) %>%
  group_by(Weekday, Features) %>%
  summarise(TotalCount=n()) %>% 
  arrange(Weekday, desc(TotalCount)) 
```

    ## # A tibble: 21 × 3
    ## # Groups:   Weekday [7]
    ##    Weekday Features  TotalCount
    ##    <ord>   <chr>          <int>
    ##  1 Sun     Calories      173760
    ##  2 Sun     Intensity     173760
    ##  3 Sun     Steps         173760
    ##  4 Mon     Calories      171660
    ##  5 Mon     Intensity     171660
    ##  6 Mon     Steps         171660
    ##  7 Tue     Calories      168480
    ##  8 Tue     Intensity     168480
    ##  9 Tue     Steps         168480
    ## 10 Wed     Calories      214440
    ## # ℹ 11 more rows

#### Time Frequency

##### Hourly Frequency of Smart Features

``` r
mod_smart %>%
  filter(Interval=="Hourly") %>%
  mutate(Hour=hours(Time)) %>%
  group_by(Hour, SmartFeatures) %>%
  summarise(Total_Count=n()) %>%
  arrange(Hour, desc(Total_Count))
```

    ## # A tibble: 96 × 3
    ## # Groups:   Hour [24]
    ##     Hour SmartFeatures    Total_Count
    ##    <dbl> <chr>                  <int>
    ##  1     0 AverageIntensity         939
    ##  2     0 Calories                 939
    ##  3     0 TotalIntensity           939
    ##  4     0 TotalSteps               939
    ##  5     1 AverageIntensity         933
    ##  6     1 Calories                 933
    ##  7     1 TotalIntensity           933
    ##  8     1 TotalSteps               933
    ##  9     2 AverageIntensity         933
    ## 10     2 Calories                 933
    ## # ℹ 86 more rows

##### Minute Frequency of Smart Features

``` r
mod_smart %>%
  filter(Interval=="Minute") %>%
  mutate(Minutes=minutes(Time)) %>%
  group_by(Minutes, SmartFeatures) %>%
  summarise(Total_Count=n()) %>%
  arrange(Minutes, desc(Total_Count))
```

    ## # A tibble: 300 × 3
    ## # Groups:   Minutes [60]
    ##    Minutes SmartFeatures Total_Count
    ##      <dbl> <chr>               <int>
    ##  1       0 Calories            22106
    ##  2       0 Intensity           22106
    ##  3       0 METs                22106
    ##  4       0 Steps               22106
    ##  5       0 Sleep                3131
    ##  6       1 Calories            22102
    ##  7       1 Intensity           22102
    ##  8       1 METs                22102
    ##  9       1 Steps               22102
    ## 10       1 Sleep                3127
    ## # ℹ 290 more rows

##### Minute Frequency & Average Value of Minute-Wide Features

``` r
mod_minWide %>%
  mutate(Minutes=minutes(Time)) %>%
  group_by(Minutes, Features) %>%
  summarise(Total_Count = n(), 
            Average = round(mean(Value), 2)) %>%
  arrange(Minutes, desc(Total_Count))
```

    ## # A tibble: 180 × 4
    ## # Groups:   Minutes [60]
    ##    Minutes Features  Total_Count Average
    ##      <dbl> <chr>           <int>   <dbl>
    ##  1       0 Calories        21536    1.62
    ##  2       0 Intensity       21536    0.2 
    ##  3       0 Steps           21536    5.33
    ##  4       1 Calories        21536    1.63
    ##  5       1 Intensity       21536    0.2 
    ##  6       1 Steps           21536    5.36
    ##  7       2 Calories        21536    1.64
    ##  8       2 Intensity       21536    0.21
    ##  9       2 Steps           21536    5.55
    ## 10       3 Calories        21536    1.64
    ## # ℹ 170 more rows

#### Rank Analysis

##### Top 5 Most Used Features

``` r
mod_smart %>%
  group_by(SmartFeatures) %>%
  summarise(Total=n()) %>%
  arrange(desc(Total)) %>%
  slice(1:5)
```

    ## # A tibble: 5 × 2
    ##   SmartFeatures   Total
    ##   <chr>           <int>
    ## 1 HeartRate     2483659
    ## 2 Calories      1349176
    ## 3 Intensity     1326127
    ## 4 METs          1326127
    ## 5 Steps         1326127

##### Bottom 5 Least Used Features

``` r
mod_smart %>%
  group_by(SmartFeatures) %>%
  summarise(Total=n()) %>%
  arrange(Total) %>%
  slice(1:5)
```

    ## # A tibble: 5 × 2
    ##   SmartFeatures  Total
    ##   <chr>          <int>
    ## 1 Fat                2
    ## 2 BMI               67
    ## 3 IsManualReport    67
    ## 4 WeightKg          67
    ## 5 WeightPounds      67

### Step 6 - Visualizations

![](Bellabeat-CS_files/figure-gfm/unnamed-chunk-73-1.png)<!-- -->

The Heart Rate category saw the most use during the month-long period.
The Weight and Sleep categories saw less than 5% of use combined during
the period. Since the categories reflect their related smart features,
we can assume that the heart rate feature saw the most use during the
period while both weight-related and sleep-related features saw the
least.

![](Bellabeat-CS_files/figure-gfm/unnamed-chunk-74-1.png)<!-- -->

The top 5 most used features recorded over 1,300,000 use count over the
course of the month. The heart rate feature saw nearly twice as much use
as the other four, demonstrating its preference among users.

![](Bellabeat-CS_files/figure-gfm/unnamed-chunk-75-1.png)<!-- -->

The bottom five least used features recorded under 100 use count during
the month, indicating low engagement by the users. Important to note
that all five of these features are related to weight.

![](Bellabeat-CS_files/figure-gfm/unnamed-chunk-76-1.png)<!-- -->

The highest frequency of use was recorded by minutes with over 150,000
uses steadily throughout most of the month. This shows that users
greatly preferred tracking their activities first by minute, then by
seconds compared to the other time intervals.

![](Bellabeat-CS_files/figure-gfm/unnamed-chunk-77-1.png)<!-- -->

The Intensity category saw the most daily use, while the Weight category
saw the least. The Hourly interval recorded use from 3 feature
categories, with the Intensity category made up half of the total use.
The Minute interval recorded use from 5 categories, with the Sleep
category recording the less than 4% of use. The only feature tracked
during seconds was heart rate. With the exception of Seconds, as the
time interval increased, so did the number of categories (and related
features) use among users.

![](Bellabeat-CS_files/figure-gfm/unnamed-chunk-78-1.png)<!-- -->

There are three distinct layers of frequency among the smart features
over the month. The Calories features and all the intensity-related
features had the highest daily use out of all of the daily-tracked
features. The bottom layer was made up of all of the weight-related
features and saw a daily use of less than 5.

![](Bellabeat-CS_files/figure-gfm/unnamed-chunk-79-1.png)<!-- -->

A consistent frequency under 1,000 uses every hour was recorded for the
hourly smart features. The four features that were recorded hourly saw
an equal amount of use from hours ‘0-23’. The Sleep feature had the
lowest frequency of use compared to the other four features recorded
every minute. The minute frequencies of these features had over 20,000
uses steadily from minutes ‘0-59’.

### Top Recommendations

**Drop Weight Features**

Bellabeat should consider removing or not developing any weight-related
features since the five least used features were all related to weight.
By shifting resources to other features that are actually popular among
users, the company would be able to better market their products and
attract potential customers.

**Develop Features Related to Most Popular Features**

Bellabeat should create features related to the top five most used
features (Heart Rate, Calories, Intensity, Steps and METs). For example,
the company could develop cardio-related features such as a blood
pressure recorder or pulse monitor to reflect the high use in the Heart
Rate feature. These new features could then be added to existing
Bellabeat products to the benefit of existing and potential customers.
The Time wellness watch would be the perfect product to track these new
features.

**Track Features by Minutes/Seconds**

The company should ensure that their features can be tracked by the
minutes or seconds as the highest use were recorded during these time
intervals. This was illustrated when analyzing the highly consistent
minute frequency of smart features and the Minute-Wide features. If
Bellabeat develops products to track features at these time intervals,
it should match the high use seen during the month while also appealing
to potential customers.
