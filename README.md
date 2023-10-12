# Data Analytics Portfolio
## Case study: How Does a Bike-Share Navigate Speedy Success?
#### Madhavi Alla
#### 04-10-2023
Introduction
Case study is part of the journey to earn a Google Data Analytics Professional Certificate. We’re diving into the data of a bikeshare company and exploring their customer’s trip details over the year 2022. For this case study, I used R for the data preparation, visualization and analysis.
The data has been made available by Motivate International Inc. under this [license](https://ride.divvybikes.com/data-license-agreement).
# *ASK*
## Scenario
The marketing team at Cyclistic, a bike-share company located in Chicago, has the task of devising marketing strategies to turn casual riders into annual members. However, in order to achieve this goal, the marketing analyst team must gain a clearer understanding of the differences between annual members and casual riders.  Cyclistic executive team approves my recommendations. By compelling data insights and visualizations. 
## Stakeholders:
* Director of marketing 
* Cyclistic executive team
* Cyclistic marketing analytics team                   
## Deliverables
Understanding the different usage patterns of annual members and casual riders with regards to Cyclistic bikes
Presenting insightful visuals and data to validate these differences
Presenting these insights to offer three actionable recommendations to convert casual riders into annual members.


# *PREPARE*
* The data set is publicly available on [divvy-tripdata](https://divvy-tripdata.s3.amazonaws.com/index.html) , index of bucket.
## Dataset :
* The data has been made available by
Motivate International Inc, a third party company.
* Data Collected include :
   1. ride_id,
   2. rideable_type
   3. started_at
   4. ended_at
   5. start_station_name
   6. start_station_id
   7. end_startion_name
   8. end_station_id
   9. member_causal (membership type)

## Dataset Limitations :
1. Data used was collected from Q1  2022. User's bike share and usage activity may have changed since then, perhaps influenced by external factors.
2. As per the data licence agreement, Divvy system data agreed with the Cuty of Chicago to only make the certain data owned by the City available to the public, hence some parameters may be missed which could affect the accuracy of our analysis.

## Is Data ROCCC? :
A reliable dataset or data-source should ROCCC which is an abbriviation for **R**eliable, **O**riginal, **C**omprehensive, **C**urrent, and **C**ited.
 1. Reliable - HIGH - Reliable as the data comes from the secondary data source, which would be DivvyBikes (which happens to be a reliable data source)
 2. Original - MEDIUM - Second party data source (DivvyBikes Data)
 3. Comprehensive - HIGH - Parameters match Cyclistic Bike-Share inventory parameters
 4. Current - HIGH - Data is from 2021's first and second quarter
 5. Cited - MEDIUM - Data collected from second party, hence a fair level of uncertainty is observed and expected.

The dataset is good quality data, and is recommended to produce business recommendations based on the insights drawn from it.
## Data Selection :
The following files are selected and copied for analysis.
 * 202201-divvy-tripdata
 * 202202-divvy-tripdata
 * 202203-divvy-tripdata
 * 202204-divvy-tripdata
# *PROCESS*
Using R to process, transform and process the data.

## Environment Setup :
Import packages
```{r}
library(tidyverse)
library(lubridate)
library(skimr)
library(dplyr)
```

## Load or Import Datasets :
Reading required CSV files
```{r}
jan_2022 <- read.csv("202101-divvy-tripdata.csv")
feb_2022 <- read.csv("202102-divvy-tripdata.csv")
mar_2022 <- read.csv("202103-divvy-tripdata.csv")
apr_2022 <- read.csv("202104-divvy-tripdata.csv")
```
Merge CSV files into a single data-frame
```{r}

all_trips <- rbind(jan_2022, feb_2022, mar_2022, apr_2022)
```

## Data Cleaning and Manipulation:

### Data Cleaning Steps :
1. Define and count null or missing values and, then remove them
2. Check for duplicated entries, and amend
3. Define incorrect values and clean up incorrect values (ex. data with started_at > ended_at)

```{r}
null_values <-sapply(all_trips, function(x) sum(is.na(x))) %>%
  as.data.frame()
null_values %>%
  ggplot(aes(x = rownames(missing_values), y = .)) + geom_bar(stat = "identity")
 colSums(is.na(all_trips))
 
 trips_Clean <- all_trips[complete.cases(all_trips), ]
 
 trips_Clean <- trips_Clean %>% 
  filter(trips_frameClean$started_at < all_trips$ended_at)
```
  



### Data Transformation/Manipulation Steps :
1. Create new coulmn (ride_length)
2. Create new column (day_of_week)
3. Removing bad data
   

```{r}
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)/60
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
 all_trips_stname <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

```


 # *ANALYSIS*
 
 ## Perform Calculations :
  * count number of rides
  * averages (ride_length, ride_length for members & casual riders)
  * min and max ride lengths
  * number of rides for member_casual number
  * average ride length by each day for member_casual
  * observe data by membership type and day_of_week
 
 
  
```{r}
  aggregate(all_trips_stname$ride_length ~ all_trips_v2$member_casual, FUN = mean)
  aggregate(all_trips_stname$ride_length ~ all_trips_v2$member_casual, FUN = median)
  aggregate(all_trips_stname$ride_length ~ all_trips_v2$member_casual, FUN = max)
   aggregate(all_trips_stname$ride_length ~ all_trips_v2$member_casual, FUN = min)
  all_trips_stname$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", 
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
 ````


# *SHARE*
   *  By comparing how many trips each group has taken
  ```{r}
  all_trips_stname %>%  
  drop_na(member_casual) %>%
  group_by(member_casual) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = `n` / sum(`n`)) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>%
  ggplot(aes(x="", y=perc, fill=member_casual)) +
  geom_bar(stat="identity", width=1, color = "white") +
  geom_text(aes(label = labels), color = "white",
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start=0) +
  theme_void()
  ```
  * Customer Type vs. Day of the week
```{r}
   all_trips_stname %>%  
   drop_na(member_casual) %>%
   group_by(member_casual, day_of_week) %>% 
   summarise(average_trip_duration = mean(ride_length), .groups = 'keep') %>%
   ggplot(aes(x = day_of_week, y = average_trip_duration, fill = member_casual)) +
   geom_col(width=0.5, position = position_dodge(width=0.5)) + 
   labs(title ="Average trip duration by customer type Vs. Day of the week")
```
   * Total Trips By Customer Type Vs. Day of the Week
  ```{r}
   all_trips_stname%>%  
  drop_na(member_casual) %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'keep') %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
  ```
   * Average trip duration by customer type Vs. Month
  ```{r}
  all_trips_stname %>%  
  drop_na(member_casual) %>%
  group_by(member_casual, month) %>% 
  summarise(average_trip_duration = mean(ride_length), .groups = 'keep') %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month")
   ```
 * Total trips by customer type Vs. Month
 ```{r}
   all_trips_stname %>%  
  drop_na(member_casual) %>%
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), .groups = 'keep') %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

 ```
 * Trips Demand over 24 hours of a Day
```{r}
 all_trips_stname %>%
  drop_na(member_casual) %>%
  group_by(member_casual, time) %>% 
  summarise(number_of_trips = n(), .groups = 'keep') %>%
  ggplot(aes(x = time, y = number_of_trips, color = member_casual, group = member_casual)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")

```
# *ACT*   
* Key Points
```{r}

1) Annual members take more trips while casual riders take longer trips.

2) Casual customers use bikeshare services more during weekends, while members use 
   them consistently over the entire week.

3) Both types, but especially casual riders bike significantly less during the winter 
   months

4) Average trip duration of casual riders is more than twice that of member rider over 
  any given day of the week cumulatively.

5) Casual riders ride longer during first half of the year compared to the second half,while members clock relatively similar average trip duration month over month.

6) Casual riders prefer docked bikes the most while classic bikes are popular among members.
```
 * Recommendations
```{r}
 1) Introduce a member only rewards program based on trip duration to incentivize 
  casual riders to sign up as members and be eligible for the rewards.

 2) Offer discounted pricing during non-busy hours so that casual riders might choose 
   to use bikes more often and level out demand over the day.
```
