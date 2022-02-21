#01- downloaded all the data related to 2021

#02- added ride_length & day_of_week cells to each sheet

#03- load required libraries
library(tidyverse)
library(lubridate)

#03- import all the CSV Files into R
tripdata_202101 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202101-divvy-tripdata.csv")
tripdata_202102 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202102-divvy-tripdata.csv")
tripdata_202103 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202103-divvy-tripdata.csv")
tripdata_202104 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202104-divvy-tripdata.csv")
tripdata_202105 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202105-divvy-tripdata.csv")
tripdata_202106 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202106-divvy-tripdata.csv")
tripdata_202107 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202107-divvy-tripdata.csv")
tripdata_202108 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202108-divvy-tripdata.csv")
tripdata_202109 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202109-divvy-tripdata.csv")
tripdata_202110 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202110-divvy-tripdata.csv")
tripdata_202111 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202111-divvy-tripdata.csv")
tripdata_202112 <- read_csv("D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/202112-divvy-tripdata.csv")

#04- Merge all the files in to one
tripdata <- rbind(tripdata_202101, 
                  tripdata_202102, 
                  tripdata_202103, 
                  tripdata_202104, 
                  tripdata_202105, 
                  tripdata_202106, 
                  tripdata_202107, 
                  tripdata_202108, 
                  tripdata_202109, 
                  tripdata_202110, 
                  tripdata_202111, 
                  tripdata_202112)

#05- change started_at & ended_at into time and date
tripdata$started_at <- as.factor(tripdata$started_at)
tripdata$started_at <- as.POSIXct(tripdata$started_at, format= "%m/%d/%Y %H:%M")
tripdata$ended_at <- as.factor(tripdata$ended_at)
tripdata$ended_at <- as.POSIXct(tripdata$ended_at, format= "%m/%d/%Y %H:%M")

#06- split started_at in to date - day - month - year
#    transfer day to be text
#    calculate Ride_Length
tripdata$date <- as.Date(tripdata$started_at)
tripdata$month <- format(as.Date(tripdata$date), "%B")
tripdata$day <- format(as.Date(tripdata$date), "%d")
tripdata$year <- format(as.Date(tripdata$date), "%Y")
tripdata$day_of_week <- format(as.Date(tripdata$date), "%A")
tripdata$ride_length <- difftime(tripdata$ended_at,tripdata$started_at)

#07- create updated data set where ride_length < 0
tripdata_v2 <- tripdata[!(tripdata$ride_length<0),]

#08- calculate mean - Median - max - min for Ride_length
mean(tripdata_v2$ride_length)
median(tripdata_v2$ride_length)
max(tripdata_v2$ride_length)
min(tripdata_v2$ride_length)

#09- calculate mean - Median - max - min for Ride_length for different Member_casual
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual, FUN = mean)
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual, FUN = median)
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual, FUN = max)
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual, FUN = min)

#10- sort weekdays & months in order 

tripdata_v2$day_of_week <- ordered(tripdata_v2$day_of_week, 
                                   levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
tripdata_v2$month <- ordered(tripdata_v2$month,
                             levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

#11- calculate mean for different ride_length based on members and different days 
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual + tripdata_v2$day_of_week, FUN = mean)
aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual + tripdata_v2$month, FUN = mean)

#12- Visualize the number_of_rides split into different days and member types 
tripdata_v2 %>%
  group_by(member_casual, day_of_week) %>%  
  summarise(number_of_rides = n()) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides)) + geom_col() + facet_wrap(~member_casual) +
  labs(title= "Number of Rides by Days and Rider Type",subtitle= "Members versus Casual Users")

#13- Visualize the number of rides split into different months and member types
tripdata_v2 %>%
  group_by(member_casual, month) %>% 
  summarise(number_or_rides= n()) %>% 
  ggplot(aes(x= month, y= number_or_rides)) + geom_col() + facet_wrap(~member_casual) +
  labs(title= "Number of Rides by months and Rider Type",subtitle= "Members versus Casual Users")



#14- Visualize the average_duration split into different days and member types 
tripdata_v2 %>%
  group_by(member_casual, day_of_week) %>%  
  summarise(average_duration= mean(ride_length)) %>% 
  ggplot(aes(x = day_of_week, y = average_duration)) + geom_col() + facet_wrap(~member_casual) +
  labs(title= "Average duration per ride by Days and Rider Type",subtitle= "Members Versus casual Users")

#15- export data for number_of_rides per day_of_week 7 member type
No_of_rides_per_day <- aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual+ tripdata_v2$day_of_week, FUN = length)
write.csv(No_of_rides_per_day, file ="D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/No_of_rides_per_day.csv")


#16- export data for number_of_rides per month & member type
No_of_rides_per_month<- aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual+ tripdata_v2$month, FUN = length)
write.csv(No_of_rides_per_month, file ="D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/No_of_rides_per_month.csv")

#16- export data shows average_duration per day_of_week & member type
avr_duration_per_ride_per_day <- aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual+ tripdata_v2$day_of_week, FUN = mean)
write.csv(avr_duration_per_ride_per_day, file ="D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/avr_duration_per_ride_per_day.csv")

#17- export data shows average_duration per Month & member type
avr_duration_per_ride_per_month<- aggregate(tripdata_v2$ride_length ~ tripdata_v2$member_casual+ tripdata_v2$month, FUN = mean)
write.csv(avr_duration_per_ride_per_month, file ="D:/Case Studies/track 1/How does a bike-share navigate speedy success/Data/avr_duration_per_ride_per_month.csv")



glimpse(tripdata)
glimpse(tripdata_v2)
glimpse(No_of_rides_per_day)
view(tripdata_v2$month)
colSums(is.na(tripdata))

