Google analytics
================
Ignacio
27/4/2022

![](G:/My%20Drive/Python/R/archive/Fitabase%20Data%204.12.16-5.12.16/bellabeat.PNG)

# Google Analytics project: How Can a Wellness Technology Company Play It Smart?

For this project we will analyze data for a wellness technology company
in order to help them take informed decitions. We will follow the 6
steps of data analysis from the Google Analytics course: Ask, Prepare,
Process, Analyse, Share and Act.

# 1 Ask

Bellabeat is a health smart product, marketed for women, to inspire and
empower them with knowledge of their health and habits. The company
wants us to analyze smart device usage to gain consumer use knowledge.
We then need to apply that onto a selected Bellabeat product.

The main questions are:

1.  What are some trends in smart device usage?
2.  How could these trends apply to Bellabeat customers?
3.  How could these trends help influence Bellabeat marketing strategy?

# 2 Prepare

In order to answer the main questions we are encouraged to use publicly
available data from smart device users.

  - The data selected for this project is the FitBit Fitness Tracker
    Data (CC0: Public Domain, dataset made available through Möbius).
  - In this dataset 30 FitBit users consented to the submission of their
    personal tracker data, including minute-level output for physical
    activity, heart rate, sleep monitoring and weight tracking.
  - The data comprehends a month of use of the tracker, beween 12th of
    April and 12th of May, 2016.
  - There is a variability in the output collected due to the different
    type of FitBit tracker and the individual tracking preference.

Using the ROCCC method to assess the credibility of the data:

**R**eliable: The data presents different formats (due to different
trackers), it is not complete (different tracking preferences) and one
of the files presents repeated values.

**O**riginal: The data comes from a third party provider.

**C**omprehensive: The parameters measured are quite comprehensive when
it comes to a fitness device tracker. However, it only includes 30
participants.

**C**urrent: The data is from 2016. We would not expect a change in the
overall health of the population measured by the devices since, but
maybe an improvement in the devices or the parameters they can measure.

**C**ited: As a third party data we have no information regarding a
credible source.

# 3 Process

As the Google Analytics course focuses on R as the programming language,
I will use it for the further analysis.

From a previous inspection of the data, there is some repetition on the
content. The file with daily Activity contains merged data from daily
Caolories, daily Intensities and daily Steps. There is also detailed
data by the hour and by the minute of calories, steps and intensities,
that are summed up in the daily activity file. Moreover, there is sleep,
weight, and heart rate tracks. We will be using the daily activity, as
well as the sleep and weight. \#\# Loading the data and libraries

``` r
library(tidyverse)
library(lubridate)
library(data.table)
library(ggpmisc)
library(patchwork)

daily_activity <- read.csv("dailyActivity_merged.csv")
weight_log <- read.csv("weightLogInfo_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")
```

Converting Date and time value to a correct datetime format and added
weekday name for more information

``` r
Sys.setlocale("LC_TIME", "C")
```

    ## [1] "C"

``` r
daily_activity <- mutate(daily_activity, ActivityDate = mdy(ActivityDate))
daily_activity <- daily_activity %>% mutate(weekday = weekdays(ActivityDate))
daily_activity$weekday <- ordered(daily_activity$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))
sleep_day <- mutate(sleep_day, Date = mdy_hms(SleepDay))
sleep_day <- sleep_day %>% mutate(weekday = weekdays(Date))
sleep_day$weekday <- ordered(sleep_day$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))
weight_log <- mutate(weight_log, Date = mdy_hms(Date))
weight_log <- weight_log %>% mutate(weekday = weekdays(Date))
weight_log$weekday <- ordered(weight_log$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))

weight_log <- mutate(weight_log, Time = format(Date, "%H:%M:%S"))
weight_log <- mutate(weight_log, Date = format(Date, "%Y-%m-%d"))
```

### Data summary

``` r
#Added a palet of colors for the graphs
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

merge_act_sleep_weight <-unique(merge(daily_activity, sleep_day, by.x = c("Id","ActivityDate"),by.y=c("Id","Date"), all=TRUE) %>%  merge(weight_log, by.x = c("Id","ActivityDate"),by.y=c("Id","Date"),all=TRUE))
merge_act_sleep_weight %>% 
  select(Diastance = TotalDistance,Steps = TotalSteps, Calories, SedentaryTime = SedentaryMinutes, Sleep_minutes = TotalMinutesAsleep, WeightKg, BMI) %>%  
  summary()
```

    ##    Diastance          Steps          Calories    SedentaryTime   
    ##  Min.   : 0.000   Min.   :    0   Min.   :   0   Min.   :   0.0  
    ##  1st Qu.: 2.620   1st Qu.: 3790   1st Qu.:1828   1st Qu.: 729.8  
    ##  Median : 5.245   Median : 7406   Median :2134   Median :1057.5  
    ##  Mean   : 5.490   Mean   : 7638   Mean   :2304   Mean   : 991.2  
    ##  3rd Qu.: 7.713   3rd Qu.:10727   3rd Qu.:2793   3rd Qu.:1229.5  
    ##  Max.   :28.030   Max.   :36019   Max.   :4900   Max.   :1440.0  
    ##                                                                  
    ##  Sleep_minutes      WeightKg           BMI       
    ##  Min.   : 58.0   Min.   : 52.60   Min.   :21.45  
    ##  1st Qu.:361.0   1st Qu.: 61.40   1st Qu.:23.96  
    ##  Median :432.5   Median : 62.50   Median :24.39  
    ##  Mean   :419.2   Mean   : 72.04   Mean   :25.19  
    ##  3rd Qu.:490.0   3rd Qu.: 85.05   3rd Qu.:25.56  
    ##  Max.   :796.0   Max.   :133.50   Max.   :47.54  
    ##  NA's   :530     NA's   :873      NA's   :873

#### Distribution of the data from a daily tracking

``` r
p1 <- merge_act_sleep_weight %>% ggplot() + geom_histogram(aes(x=TotalSteps),fill = cbPalette[1]) + labs(title = "Daily steps", x="Daily steps")
p2 <- merge_act_sleep_weight %>% ggplot() + geom_histogram(aes(x=TotalDistance),fill = cbPalette[2]) + labs(title = "Daily km", x="Daily km distance")
p3 <- merge_act_sleep_weight %>% ggplot() + geom_histogram(aes(x=Calories),fill = cbPalette[3]) + labs(title = "Daily calories", x="Daily calories burnt")
p4 <- merge_act_sleep_weight %>% ggplot() + geom_histogram(aes(x=TotalMinutesAsleep/60),fill = cbPalette[4]) + labs(title = "Hours of sleep", x="Number of daily records")
p5 <- merge_act_sleep_weight %>% ggplot() + geom_histogram(aes(x=SedentaryMinutes/60),fill = cbPalette[5]) + labs(title = "Daily sedentary hours", x="Number of daily records")
p1+p2+p3+p4 +p5
```

![Fig 1. Histogram
distribution](Google_analytics_files/figure-gfm/histogram%20distribution-1.png)

We can see in this summary the distribution from some of the data which
can lead us to some preliminary conclusions.

  - We can see a correlation between steps and distance in the histogram
    plots.
  - The average steps recommended for adults is 10000 according to the
    CDC, being about 8km. According to this data the average among the
    participants is 7638 steps and 5.49 km, way below that
    recommendation.
  - On the same note, the time the users have spent in inactivity is 991
    minutes in average a day, more than 16.5 hours.
  - The average sleep recorded is og 419.2 minuts, about 7 hours.
  - We can see the presence of NA’s in the merge data for sleep and
    weight, meaning that many of the participants don’t present daily
    records of those activities.

### Data completness

``` r
#Performing a pivot table to check if there's step activity for each user each day
pivot_daily_steps <- daily_activity %>%  select(Id,ActivityDate,TotalSteps) %>% pivot_wider(names_from = ActivityDate, values_from=TotalSteps, values_fill = NaN)
pivot_daily_steps$num_na <- rowSums(is.na(pivot_daily_steps))
pivot_daily_steps %>% select(Id,num_na) %>%  arrange(desc(num_na))
```

    ## # A tibble: 33 x 2
    ##            Id num_na
    ##         <dbl>  <dbl>
    ##  1 4057192912     27
    ##  2 2347167796     13
    ##  3 8253242879     12
    ##  4 3372868164     11
    ##  5 6775888955      5
    ##  6 7007744171      5
    ##  7 6117666160      3
    ##  8 6290855005      2
    ##  9 8792009665      2
    ## 10 1644430081      1
    ## # ... with 23 more rows

From the daily activity there is one user that only has 4 days with
recorded data and 27 missing values.

#### Data recorded per activity

We can see here how much of the data was recorded from the users in the
30 day period.

``` r
p1 <- unique(sleep_day) %>%  group_by(Id) %>% summarise(n= n()) %>% ggplot(aes(x=n)) + geom_histogram(fill = cbPalette[2]) + labs(title = "Sleep records per user") + xlab("Days with activity") + ylab("Number of users")
p2 <- weight_log %>% group_by(Id) %>% summarise(n=n()) %>% ggplot(aes(x=n)) + geom_histogram(fill = cbPalette[3]) + labs(title = "Weight records per user") + xlab("Days with activity") + ylab("Number of users")
daily_min_pivot <- daily_activity %>%  group_by(Id,ActivityDate) %>%  summarise(min_record = sum(SedentaryMinutes+LightlyActiveMinutes+FairlyActiveMinutes+VeryActiveMinutes)) 
p3 <- daily_min_pivot %>%  group_by(Id) %>% summarise(daily_use = mean(min_record), n=n()) %>%  arrange(n)  %>%  ggplot(aes(x=n)) + geom_histogram(fill = cbPalette[4]) + labs(title = "Number of days with recorded activity per user") + xlab("Days with activity") + ylab("Number of users")

p1+p2+p3
```

![Fig 2. Number of days with activity
tracked](Google_analytics_files/figure-gfm/daily%20use-1.png)

We can see as most of the participants have recorded steps, calories and
activity for most of the month time, the recorded weight and sleep is
not kept as precise.

#### Weight track

We can see from the histogram in Fig 2 that the weight is tracked by the
least amount of users with only 8 participants.

``` r
weight_log %>%  ggplot(aes(x=day(Date),y=WeightKg)) + geom_col() + facet_wrap(~Id) 
```

![Fig 3. Weight tracking per user, per
day](Google_analytics_files/figure-gfm/weigth%20records-1.png)

  - Only 8 out of 30 participants have a minimum of 1 weight input
  - Only 1 of those 8 uses an automatic method for a periodic input of
    weight.
  - Only 2 of those 8 have more than 5 weight inputs in 30 days.

# 4 Analyse and Share

### Study the correlation between steps, distance and calories

``` r
p1 <- daily_activity %>% 
  ggplot(aes(x=TotalDistance,y=Calories)) + geom_point() + labs(title = "Correlation between calories and distance") + xlab("Average distance") + ylab("Average calories") +geom_smooth(method=lm, se=FALSE, col='red', size=1) 
p2 <- daily_activity %>%  
  ggplot(aes(x=TotalDistance,y=TotalSteps)) + geom_point() + labs(title = "Correlation between steps and distance") + xlab("Average distance") + ylab("Average steps") +geom_smooth(method=lm, se=FALSE, col='red', size=1)
p3 <- daily_activity %>%  
  ggplot(aes(x=TotalSteps,y=Calories)) + geom_point() + labs(title = "Correlation between steps and calories") + xlab("Average steps") + ylab("Average calories") +geom_smooth(method=lm, se=FALSE, col='red', size=1)
p1+p2+p3
```

![Fig 4. Correlation between calories, distance and
steps](Google_analytics_files/figure-gfm/calories%20steps%20distance%20correlation%20-1.png)

As speculated before, there is a strong correlation between steps and
distance. This correlation is not as strong between steps and distance
with the calories.

### Distribution of time per intensity

``` r
#Type of activity in average from all users
daily_pivot <- daily_activity %>%  group_by(Id) %>% summarise(sedentary=mean(SedentaryMinutes),lightly=mean(LightlyActiveMinutes),fairly = mean(FairlyActiveMinutes),very=mean(VeryActiveMinutes))

daily_pivot <- as.data.frame(melt(as.data.table(daily_pivot),id.vars= "Id",variable.name = "category", value.name = "value"))

daily_pivot %>%  group_by(category) %>% summarise(mean=mean(value)) %>% ggplot(aes(x=category,y=mean/60, fill=category)) + geom_bar(position="dodge", stat = "identity") + labs(title = "Average time spent for each activity intensity", x="Activity", y="Average time in hours") + scale_fill_manual(name="Type of activity", values = cbPalette)
```

![Fig 5. Intensity distribution throughout the
day](Google_analytics_files/figure-gfm/activity%20intensity-1.png)

The distribution of time used per activity intensity is very much skewed
to the sedentary activity, with an average of 16 hours, with very little
time used for fairly active or very active excersise.

### User clustering by average steps

From the participants we can group them according to their average steps
in 4 different categories to further study this: Bellow 5000 steps:
Sedentary Between 5000 and 8000 steps: Lightly active Between 8000 and
10000 steps: Fairly active Above 10000 steps: Very active

``` r
daily_activity %>% group_by(Id) %>%filter(Id!=4057192912) %>%  summarise(mean_steps = mean(TotalSteps), mean_distance = mean(TotalDistance), distance_per_step = mean(TotalDistance)*1000/mean(TotalSteps),n=n()) %>% arrange(n)
```

    ## # A tibble: 32 x 5
    ##            Id mean_steps mean_distance distance_per_step     n
    ##         <dbl>      <dbl>         <dbl>             <dbl> <int>
    ##  1 2347167796      9520.          6.36             0.668    18
    ##  2 8253242879      6482.          4.67             0.720    19
    ##  3 3372868164      6862.          4.71             0.686    20
    ##  4 6775888955      2520.          1.81             0.720    26
    ##  5 7007744171     11323.          8.02             0.708    26
    ##  6 6117666160      7047.          5.34             0.758    28
    ##  7 6290855005      5650.          4.27             0.756    29
    ##  8 8792009665      1854.          1.19             0.640    29
    ##  9 1644430081      7283.          5.30             0.727    30
    ## 10 3977333714     10985.          7.52             0.684    30
    ## # ... with 22 more rows

``` r
daily_active <- unique(daily_activity) %>% group_by(Id) %>%  summarise(mean_steps = mean(TotalSteps), mean_distance = mean(TotalDistance), mean_calories = mean(Calories),sd_step = sd(TotalSteps), sd_distance=sd(TotalDistance),sd_calories = sd(Calories),n=n()) %>% arrange(n) %>% mutate(user = case_when(
  mean_steps >= 10000~"very active", mean_steps < 10000 & mean_steps >=8000~"fairly active", mean_steps <8000 & mean_steps>=5000~"lightly active", mean_steps <5000~"sedentary"
)) %>% merge(daily_activity,by="Id")
daily_active$user <- ordered(daily_active$user, levels=c("sedentary", "lightly active", "fairly active", "very active"))

daily_active %>%  group_by(user) %>%  summarise(n=n(), labels = paste(toString(round((n/940)*100)),"%",sep="")) %>% ggplot(aes(x="", y=n, fill=user)) + geom_bar(stat = "identity", width = 1) +coord_polar("y", start=0) + theme_minimal() + theme(axis.title.x= element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +scale_fill_manual(values =cbPalette) +geom_text(aes(label = labels), position = position_stack(vjust = 0.5))+ labs(title="User type distribution")
```

![Fig 6. Distribution of the type of users based on their average
steps](Google_analytics_files/figure-gfm/user%20typing-1.png) \#\#\#\#
After clustering by average steps, we can see if we find any pattern for
activity level.

``` r
daily_pivot2 <- daily_active %>%  group_by(user) %>% arrange(mean_steps) %>% summarise(sedentary=mean(SedentaryMinutes)/60,lightly=mean(LightlyActiveMinutes)/60,fairly = mean(FairlyActiveMinutes)/60,very=mean(VeryActiveMinutes)/60) %>% arrange()
daily_pivot2 <- as.data.frame(melt(as.data.table(daily_pivot2),id.vars= "user",variable.name = "category", value.name = "value"))
daily_pivot2 %>% mutate(name= fct_relevel(user,"sedentary","lightly active","fairly active","very active")) %>% ggplot(aes(x=name,y=value, fill=name)) + geom_bar(position="dodge", stat = "identity") + facet_wrap(~category, scales = "free") +theme_minimal() + labs(title="Type of activity per user type", x= "User type", y= "Hours spent") +  scale_fill_manual(name="Type of user", values = cbPalette) + theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))
```

![Fig 7. Intensity level per user
type](Google_analytics_files/figure-gfm/user%20activity-1.png) \#\#\#\#
Activity per weeekday

After that we can hypothesize if there is any correlation in tracking
data according to the day of the week.

``` r
p1 <- daily_active %>% group_by(weekday) %>% summarise(mean_steps = mean(TotalSteps)) %>% ggplot(aes(x=weekday,y=mean_steps)) + geom_col(fill=cbPalette[7]) +theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)) +labs(title="Steps per day", x= "Day", y="Steps")
p2 <- sleep_day %>% group_by(weekday) %>% summarise(mean_sleep = mean(TotalMinutesAsleep)/60) %>% ggplot(aes(x=weekday,y=mean_sleep)) + geom_col(fill=cbPalette[8]) +theme(axis.text.x = element_text(angle = 45,vjust = 0.5, hjust = 1)) +labs(title="Hours sleep per day", x= "Day", y="Hours slept")
p1+p2
```

![Fig 8. Recorded steps and sleep per
weekday](Google_analytics_files/figure-gfm/weekday%20record-1.png)

Interestingly, but maybe not surprising, Sundays are the days with less
steps and more hours of sleep.

Checking the sleep record we see only 15 participants (out of 33) have
records with more 10 or more days of sleep recorded from the 30
possible. There is also 9 users with less than 9 records, and other 9
with no sleep record from their FitBit. Only considering the regular
users, we see an average sleep time of 7 hours and 16 minutes, with a
standard deviation of 53 minutes.

#### Activity distribution along the day

``` r
heartrate_seconds <- read.csv("heartrate_seconds_merged.csv")
heartrate_seconds <- mutate(heartrate_seconds, Time = mdy_hms(Time))
heartrate_seconds  <- heartrate_seconds  %>% mutate(weekday = weekdays(Time))
heartrate_seconds$weekday <- ordered(heartrate_seconds$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))

weekday_p <- heartrate_seconds %>% filter(weekday==c("Monday","Tuesday","Wednesday", "Thursday","Friday")) %>% group_by(hour=format(Time,"%H")) %>% summarise(n(), mean_val=mean(Value)) %>% ggplot(aes(x=hour,y=mean_val, fill=mean_val)) + geom_col() + labs(title = "Weekday average heartbeat",x="Hour of the day",y="Average pulse") + theme_minimal() +scale_fill_gradient(name="Average heartbeat",low = "blue", high = "red")

weekend_p <- heartrate_seconds %>% filter(weekday==c("Saturday","Sunday")) %>%  group_by(hour=format(Time,"%H")) %>% summarise(n(), mean_val=mean(Value)) %>%   ggplot(aes(x=hour,y=mean_val, fill=mean_val)) + geom_col() + labs(title = "Weekend average heartbeat", x="Hour of the day",y="Average pulse") + theme_minimal() +scale_fill_gradient(name="Average heartbeat",low = "blue", high = "red")

weekday_p / weekend_p
```

![Fig 9. Intensity along the
day](Google_analytics_files/figure-gfm/heartrate-1.png)

#### Sleep tracking from users

``` r
#sleep_day %>%  ggplot(aes(x=TotalTimeInBed,y=TotalMinutesAsleep)) + geom_point() + facet_wrap(~Id)

unique(sleep_day) %>% group_by(Id) %>% summarise(sleep = mean(TotalMinutesAsleep), n=n()) %>% ggplot(aes(x=n,y=sleep/60, color=sleep/60)) + geom_point() + annotate("text",x=25, y=10 ,label = "Daily users") + annotate("rect",xmin=22, xmax=32,ymin=4.5, ymax=9, alpha=0.2, fill="Green") + annotate("text",x=15, y=10 ,label = "Frequent users") + annotate("rect",xmin=14, xmax=19,ymin=7, ymax=8.5, alpha=0.2, fill="Yellow") + annotate("text", x=5, y=10, label = "Rare users") + labs(title = "Average sleep vs use of the FitBit", color="Hours slept") + xlab("Days recording sleep") + ylab("Average sleep in hours")
```

![Fig 10. Sleep track per
user](Google_analytics_files/figure-gfm/sleep-1.png)

#### Use of the tracker for activity, sleep and weight, according to the average step count

``` r
p1 <- daily_active %>%  group_by(Id) %>% summarise(n=n(), user ) %>% group_by(user) %>% summarise(mean_days=mean(n))  %>% ggplot(aes(x=user,y=mean_days)) + geom_col(fill=cbPalette[3]) + labs(title="Days with tracked activity by user's activity", x="Type of user", y="Days with activity") + theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))
merge_act_sleep_weight <-unique(merge(daily_active, sleep_day, by.x = c("Id","ActivityDate"),by.y=c("Id","Date"), all=TRUE) %>%  merge(weight_log, by.x = c("Id","ActivityDate"),by.y=c("Id","Date"),all=TRUE)) 
p2 <- merge_act_sleep_weight[!is.na(merge_act_sleep_weight$TotalMinutesAsleep),] %>% group_by(Id) %>% summarise(n= n(),user) %>% group_by(user) %>% summarise(mean_sleep = mean(n)) %>% ggplot(aes(x=user,y=mean_sleep)) + geom_col(fill=cbPalette[2]) + labs(title="Days with tracked sleep by user's activity", x="Type of user", y="Days with activity") + theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))
p3 <- merge_act_sleep_weight[!is.na(merge_act_sleep_weight$WeightKg),] %>% group_by(Id) %>% summarise(n= n(),user) %>% group_by(user) %>% summarise(mean_weight = mean(n)) %>% ggplot(aes(x=user,y=mean_weight)) + geom_col(fill=cbPalette[2]) + labs(title="Days with tracked weight by user's activity", x="Type of user", y="Days with activity") + theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))

p1+p2+p3
```

![Fig 11. Tracking amount per type of
user](Google_analytics_files/figure-gfm/track%20per%20user-1.png)

While we don’t see a major difference in the amount of tracked data for
daily steps according to the user activity, we do see a difference in
the sleep tracking. Users that have a more sedentary life have a
tendency to track less their sleep.

## 5 Act

As early mentioned, Bellabeat is a company that has since 2013
contributed into empowering women giving them knowledge about their
activity, sleep, stress and reproductive health. After analyzing data
from a 3rd party source from a smart tracker, we can present some
conclusions regarding our analysis.

1 - From the participants in the study we can see an average of 16 hours
of sedentary activity daily (Fig 5). Those hours could definitely be
improved by reducing them to increase the activity. However, as we could
see when sorting the data by step count, the user that managed higher
number of steps did not do that by reducing so much the sedentary time,
but rather by performing a higher activity during the other time of the
day (Fig 7). For this, we could send a reminder before the highest peaks
of activity, being 16:00 on weekdays, and 13:00 on weekends (Fig 9). A
reward system could be implemented for encouraging this.

2 - There is very few tracks of sleep or weight throughout the users of
this study. In the case of weight, it had to be tracked manually, or by
a device compatible with FitBit (Fig 3). However, sleep could be tracked
with the same smart tracker, and only around 40% had records for more
than 20 days (Fig 10). Despite having no information on why the users
decided to not track their sleep, Bellabeat could improve the
information they give their users about their sleeping behavior to
improve this. Size and comfort of the smart tracked could be important
for its use during the night.

3 - While Bellabeat claims to inform their users about stress, there was
no calculated parameter for stress in the data analyzed from FitBit
users. Stress could be potentially calculated with the differential
increase in heartbeat, together with the pedometer track. Implementing a
stress alert could be an improvement on the device that could separate
them from their competitors in the market.

4 - Last but not least, when it comes to women reproductive health we
cannot forget the menstrual cycle. Despite not knowing whether the
FitBit data corresponded to male or female users, there was no tracking
of menstrual cycle. Adding this feature could be useful for Bellabeat
users, therefore we suggest incorporating it in the device’s options.
