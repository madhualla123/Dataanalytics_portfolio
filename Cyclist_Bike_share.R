# Install packages and Loading library
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("magrittr")
library(magrittr)

# Importing Files
Reading required CSV files
jan_2022 <- read.csv("202101-divvy-tripdata.csv")
feb_2022 <- read.csv("202102-divvy-tripdata.csv")
mar_2022 <- read.csv("202103-divvy-tripdata.csv")
apr_2022 <- read.csv("202104-divvy-tripdata.csv")

#Merge CSV files into a single data-frame

   all_trips <- rbind(jan_2022, feb_2022, mar_2022, apr_2022)

# Data Cleaning and Manipulation:
## Data Cleaning Steps :
1. Define and count null or missing values and, then remove them
2. Check for duplicated entries, and amend
3. Define incorrect values and clean up incorrect values (ex. data with started_at > ended_at)
null_values <-sapply(all_trips, function(x) sum(is.na(x))) %>%
  as.data.frame()
null_values %>%
  ggplot(aes(x = rownames(missing_values), y = .)) + geom_bar(stat = "identity")
 colSums(is.na(all_trips))
  trips_Clean <- all_trips[complete.cases(all_trips), ]
    trips_Clean <- trips_Clean %>% 
  filter(trips_frameClean$started_at < all_trips$ended_at)

### Data Transformation/Manipulation Steps :
1. Create new coulmn (ride_length)
2. Create new column (day_of_week)
3. Removing bad data
   
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)/60
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
 all_trips_stname <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]


 ## Perform Calculations :
  * count number of rides
  * averages (ride_length, ride_length for members & casual riders)
  * min and max ride lengths
  * number of rides for member_casual number
  * average ride length by each day for member_casual
  * observe data by membership type and day_of_week
 
 
  aggregate(all_trips_stname$ride_length ~ all_trips_v2$member_casual, FUN = mean)
  aggregate(all_trips_stname$ride_length ~ all_trips_v2$member_casual, FUN = median)
  aggregate(all_trips_stname$ride_length ~ all_trips_v2$member_casual, FUN = max)
   aggregate(all_trips_stname$ride_length ~ all_trips_v2$member_casual, FUN = min)
  all_trips_stname$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", 
  "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
 
   # By comparing how many trips each group has taken
 
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
  
  # Customer Type vs. Day of the week

   all_trips_stname %>%  
   drop_na(member_casual) %>%
   group_by(member_casual, day_of_week) %>% 
   summarise(average_trip_duration = mean(ride_length), .groups = 'keep') %>%
   ggplot(aes(x = day_of_week, y = average_trip_duration, fill = member_casual)) +
   geom_col(width=0.5, position = position_dodge(width=0.5)) + 
   labs(title ="Average trip duration by customer type Vs. Day of the week")

# Total Trips By Customer Type Vs. Day of the Week
  
   all_trips_stname%>%  
  drop_na(member_casual) %>%
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n(), .groups = 'keep') %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  labs(title ="Total trips by customer type Vs. Day of the week") +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

# Average trip duration by customer type Vs. Month
  
  all_trips_stname %>%  
  drop_na(member_casual) %>%
  group_by(member_casual, month) %>% 
  summarise(average_trip_duration = mean(ride_length), .groups = 'keep') %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month")

# Total trips by customer type Vs. Month
 
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
 
 # Trips Demand over 24 hours of a Day

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
                   
