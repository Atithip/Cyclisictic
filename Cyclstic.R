
#Loading Packages & Libraries
library("dplyr")
library("tidyverse")
library("lubridate")
library("readr")

#Loading 12 months of cylistic data
jan<-read.csv("202301-divvy-tripdata.csv")
feb<-read.csv("202302-divvy-tripdata.csv")
mar<-read.csv("202303-divvy-tripdata.csv")
apr<-read.csv("202304-divvy-tripdata.csv")
may<-read.csv("202305-divvy-tripdata.csv")
jun<-read.csv("202306-divvy-tripdata.csv")
jul<-read.csv("202307-divvy-tripdata.csv")
aug<-read.csv("202308-divvy-tripdata.csv")
sep<-read.csv("202309-divvy-tripdata.csv")
oct<-read.csv("202310-divvy-tripdata.csv")
nov<-read.csv("202311-divvy-tripdata.csv")
dec<-read.csv("202312-divvy-tripdata.csv")
cyclistic_2023<- rbind(jan, feb, mar, apr, may, jun, jul, aug, sep,oct,nov,dec)

# taking a look at the dataframe

View(cyclistic_2023)
str(cyclistic_2023)
#EDA
### converting the wrong data type for started_at and ended_At
cyclistic_2023$start_at<- as.Date(cyclistic_2023$started_at)
cyclistic_2023$end_at<- as.Date(cyclistic_2023$ended_at)
## adding additional 
cyclistic_2023$ride_length<-difftime(cyclistic_2023$ended_at,cyclistic_2023$started_at,units=c("mins"))
## calculating the day of the week
cyclistic_2023$month <- format(as.Date(cyclistic_2023$start_at), "%m")
cyclistic_2023$day <- format(as.Date(cyclistic_2023$start_at), "%d")
cyclistic_2023$year <- format(as.Date(cyclistic_2023$start_at), "%Y")
cyclistic_2023$day_of_week<- weekdays(cyclistic_2023$start_at)
### Removing unwated columns
cyclistic_2023 <- cyclistic_2023 %>% 
  select(-c(started_at, ended_at, start_station_id, end_station_id)) %>% 
  na.omit(cyclistic_2023)
### Counting the number of times "member" and "casual" riders have taken Cyclistic services
rider_counts<- cyclistic_2023 %>% 
  group_by(cyclistic_2023$member_casual) %>% 
  summarize(count=n())
print(rider_counts)
###ANALYZE
summary(cyclistic_2023)
### Calculating average duration
mean(cyclistic_2023$ride_length)
### Calculating average duration for both casual and member
rider_duration<- cyclistic_2023 %>% 
  group_by(cyclistic_2023$member_casual) %>% 
  summarize(mean_ride_length= mean(ride_length,na.rm=TRUE))
print(rider_duration)
## Calculating highest number of days
rider_Day<- cyclistic_2023 %>% 
  group_by(cyclistic_2023$day_of_week) %>% 
  summarize(count=n())
print(rider_Day)
## Aggregating
# Average ride duration by day of the week for casual and member riders
avg_ride_duration_day <- cyclistic_2023 %>% 
  group_by(day_of_week, member_casual) %>% 
  summarize(mean_ride_length = mean(ride_length, na.rm = TRUE)) %>% 
  arrange(factor(day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

print(avg_ride_duration_day)

# Visualizing the average ride duration by day of the week
ggplot(avg_ride_duration_day, aes(x=day_of_week, y=mean_ride_length, fill=member_casual)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Average Ride Duration by Day of the Week", x="Day of the Week", y="Average Ride Length (mins)") +
  theme_minimal()

# Number of rides by day of the week for casual and member riders
rides_per_day <- cyclistic_2023 %>% 
  group_by(day_of_week, member_casual) %>% 
  summarize(count = n()) %>% 
  arrange(factor(day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

print(rides_per_day)

# Visualizing the number of rides by day of the week
ggplot(rides_per_day, aes(x=day_of_week, y=count, fill=member_casual)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Number of Rides by Day of the Week", x="Day of the Week", y="Number of Rides") +
  theme_minimal()

# Number of rides by month for casual and member riders
rides_per_month <- cyclistic_2023 %>% 
  group_by(month, member_casual) %>% 
  summarize(count = n()) %>% 
  arrange(month)

print(rides_per_month)

# Visualizing the number of rides by month
ggplot(rides_per_month, aes(x=month, y=count, fill=member_casual)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Number of Rides by Month", x="Month", y="Number of Rides") +
  theme_minimal()

# Average ride duration by month for casual and member riders
avg_ride_duration_month <- cyclistic_2023 %>% 
  group_by(month, member_casual) %>% 
  summarize(mean_ride_length = mean(ride_length, na.rm = TRUE)) %>% 
  arrange(month)

print(avg_ride_duration_month)

# Visualizing the average ride duration by month
ggplot(avg_ride_duration_month, aes(x=month, y=mean_ride_length, fill=member_casual)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Average Ride Duration by Month", x="Month", y="Average Ride Length (mins)") +
  theme_minimal()
# Distribution of rideable type 
ride_type_distribution <- cyclistic_2023 %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(count = n()) %>% 
  mutate(percentage = (count / sum(count)) * 100)

print(ride_type_distribution)

# Visualizing ride type distribution
ggplot(ride_type_distribution, aes(x=rideable_type, y=percentage, fill=member_casual)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Ride Type Distribution Between Casual and Member Riders", x="Ride Type", y="Percentage") +
  theme_minimal()


write.csv(cyclistic_2023,file= "Cyclistic.csv")





