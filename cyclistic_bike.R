# CYCLISTIC BIKE SHARING ANALYSIS

# Installing packages

install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("readr")
install.packages("skimr")
install.packages("janitor")
install.packages("readxl")

# Loading packages

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(skimr)
library(janitor)
library(lubridate)
library(readxl)
library(tibble)
library(ggmap)

# Importing xls data 
# I'm using the last 12 month data, from May 2022 to April 2023
# this data has already been cleaned in excel, just to make sure we clean more in R

cycli0522 <- read_excel("202205-divvy-tripdata.xlsx", sheet = 1)
cycli0622 <- read_excel("202206-divvy-tripdata.xlsx", sheet = 1)
cycli0722 <- read_excel("202207-divvy-tripdata.xlsx", sheet = 1)
cycli0822 <- read_excel("202208-divvy-tripdata.xlsx", sheet = 1)
cycli0922 <- read_excel("202209-divvy-tripdata.xlsx", sheet = 1)
cycli1022 <- read_excel("202210-divvy-tripdata.xlsx", sheet = 1)
cycli1122 <- read_excel("202211-divvy-tripdata.xlsx", sheet = 1)
cycli1222 <- read_excel("202212-divvy-tripdata.xlsx", sheet = 1)
cycli0123 <- read_excel("202301-divvy-tripdata.xlsx", sheet = 1)
cycli0223 <- read_excel("202302-divvy-tripdata.xlsx", sheet = 1)
cycli0323 <- read_excel("202303-divvy-tripdata.xlsx", sheet = 1)
cycli0423 <- read_excel("202304-divvy-tripdata.xlsx", sheet = 1)

# Combining the data

cyclistic_combined <- rbind(cycli0522,cycli0622,cycli0722,cycli0822,cycli0922,cycli1022,
                            cycli1122,cycli1222,cycli0123,cycli0223,cycli0323,cycli0423)

# Removing monthly data to clearup the environment
to_remove <- c("cycli0522","cycli0622","cycli0722","cycli0822","cycli0922","cycli1022",
               "cycli1122","cycli1222","cycli0123","cycli0223","cycli0323","cycli0423")

rm(list = to_remove)
rm(to_remove)

# Checking the data
head(cyclistic_combined)
str(cyclistic_combined)
View(cyclistic_combined)
colnames(cyclistic_combined)

# Cleaning the data and Manipulate

## Changing ride_duration datetime format into time

cyclistic_combined$ride_length <- format(as.POSIXct(cyclistic_combined$ride_duration), "%H:%M:%S")

current_order <- names(cyclistic_combined)
desired_order <- c("ride_id", "rideable_type", "started_at", "ended_at", "ride_length", "ride_duration", "day_of_week", "start_station_name", "start_station_id", "end_station_name", "end_station_id", "start_lat", "start_lng", "end_lat", "end_lng", "member_casual")
cyclistic_combined <- cyclistic_combined[, desired_order] ##changing the position of the new column

cyclistic_combined$ride_duration <- NULL
colnames(cyclistic_combined)[5] <- "ride_duration" ##changing back the name column to ride_duration

## Removing 0 minute and above 2 hour rides for possible false data

time_in_seconds <- sapply(strptime(cyclistic_combined$ride_duration, format = "%H:%M:%S"), function(x) x$hour * 3600 + x$min * 60 + x$sec)
cyclistic_combined <- cyclistic_combined[time_in_seconds >= 60, ]
cyclistic_combined <- cyclistic_combined[time_in_seconds <= 7200, ]

## Creating new columns for time of the day, day of the week, and season

### days
mapping_weekday <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

cyclistic_combined <- cyclistic_combined %>% 
  mutate(weekday = mapping_weekday[day_of_week])

### time
categories_time <- c("morning", "afternoon", "evening", "night")

cyclistic_combined <- cyclistic_combined %>% 
  mutate(time_category = case_when(
    hour(started_at) >= 4 & hour(started_at) < 12 ~ categories_time[1],
    hour(started_at) >= 12 & hour(started_at) < 18 ~ categories_time[2],
    hour(started_at) >= 18 & hour(started_at) < 22 ~ categories_time[3],
    TRUE ~ categories_time[4]
  ))

### season

categories_season <- c("summer", "autumn", "winter", "spring")

cyclistic_combined <- cyclistic_combined %>% 
  mutate(season_category = case_when(
    month(started_at) %in% c(6, 7, 8) ~ categories_season[1],
    month(started_at) %in% c(9, 10, 11) ~ categories_season[2],
    month(started_at) %in% c(12, 1, 2) ~ categories_season[3],
    TRUE ~ categories_season[4]
  ))

rm(categories_season, categories_time, mapping_weekday)

## Removing NA

cy_clean <- cyclistic_combined[!is.na(cyclistic_combined$member_casual), ]### Excluding the NA value from member_casual column

## Checking duplicate

duplicate_rows <- cy_clean[duplicated(cy_clean), ]### No duplicates
 
# Analyzing the data and visualization

# Rides Count Analysis

## Summary of the data 

str(cy_clean)
summary(cy_clean)

## Counting number of riders between each type of member

count_rider <- cy_clean %>%
  group_by(member_casual) %>%
  summarize(count_ride = length(ride_id), percentage = (length(ride_id) / nrow(cy_clean)) * 100 )

View(count_rider)

ggplot(data = cy_clean) +
  geom_bar(mapping = aes(x = member_casual, fill = member_casual))+
  labs(title = "Number of Rides Between Casual and Member", x = "casual vs member", y = "rides count",
       subtitle = "Total rides between each users in a year", caption = "Data from May 22 to Apr 23")

## Most used type of bikes between member

count_bike <- cy_clean %>% 
  group_by(rideable_type, member_casual) %>% 
  summarize(rider = length(ride_id))

View(count_bike)

ggplot(data = cy_clean) +
  geom_bar(mapping = aes(x = rideable_type, fill = member_casual),  width = 0.4, position = position_dodge(width = 0.4)) +
  labs(title = "Most Popular Type of Bikes", x = "type of bikes", y = "rides count",
       subtitle = "Most used type of bikes between each users in a year", caption = "Data from May 22 to Apr 23")

## Count of rides base on time

### Ordering the time
cy_clean$time_category <- ordered(cy_clean$time_category,
                                  levels = c("morning", "afternoon", "evening", "night"))

count_time <- cy_clean %>% 
  group_by(member_casual, time_category) %>% 
  summarize(rides = n()) %>% 
  arrange(member_casual, time_category)

View(count_time)

ggplot(data = count_time) +
  geom_col(mapping = aes(x = time_category, y = rides, fill = member_casual), width = 0.4, position = position_dodge(width = 0.4)) +
  labs(title = "Rides between casual vs member in time", x = "time of the day",
       subtitle = "Total rides in a year based on time", caption = "Data from May 22 to Apr 23")

## Count of rides base on day

### Ordering the day
cy_clean$weekday <- ordered(cy_clean$weekday,
                                      levels = c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"))

count_day <- cy_clean %>% 
  group_by(member_casual, weekday) %>% 
  summarize(rides = n()) %>% 
  arrange(member_casual, weekday)
  
View(count_day)

ggplot(data = count_day) +
  geom_col(mapping = aes(x = weekday, y = rides, fill = member_casual),  width = 0.4, position = position_dodge(width = 0.4)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Rides between casual vs member in weekday", 
       subtitle = "Total rides each type of users in a year based on day", caption = "Data from May 22 to Apr23")

## Count of rides base on season

### Ordering the season
cy_clean$season_category <- ordered(cy_clean$season_category,
                                    levels = c("spring", "summer", "autumn", "winter"))

count_season <- cy_clean %>% 
  group_by(member_casual, season_category) %>% 
  summarize(rides = n()) %>% 
  arrange(member_casual, season_category)

View(count_season)

ggplot(data = count_season) +
  geom_col(mapping = aes(x = season_category, y = rides, fill = member_casual), width = 0.4, position = position_dodge(width = 0.4)) +
  labs(title = "Rides between casual vs member in season", x = "Season",
       subtitle = "Total rides each type of users in a year based on season", caption = "Data from May 22 to Apr 23")

# Rides Duration Analysis

## Average, maximum, and minimum ride duration between casual and member

### Creating a function to convert time to seconds
time_to_seconds <- function(time) {
  parts <- strsplit(time, ":")[[1]]
  hours <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  total_seconds <- hours * 3600 + minutes * 60 + seconds
  return(total_seconds)
}

### Making new column with ride_duration values in seconds
cy_clean$seconds <- sapply(cy_clean$ride_duration, time_to_seconds)

avg_sum <- cy_clean %>%
  group_by(member_casual) %>%
  summarize(avg_duration = mean(seconds), max_duration = max(ride_duration),
            min_duration = min(ride_duration))

### creating function to convert seconds to time format
seconds_to_time <- function(seconds) {
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  seconds <- seconds %% 60
  time <- sprintf("%02d:%02d:%02d", as.integer(hours), as.integer(minutes), as.integer(seconds))
  return(time)
}

### Converting average results to time format for better viewing

avg_result <- avg_sum %>%
  summarize(member_casual = member_casual, mean_duration = seconds_to_time(avg_sum$avg_duration), max_duration = max_duration, min_duration = min_duration)

## Average ride duration base on time 

avg_time <- cy_clean %>%
  group_by(member_casual, time_category) %>%
  summarize(avg_duration = mean(seconds)) %>%
  arrange(member_casual, time_category)

### Converting average results to time format for better viewing

avg_time <- avg_time %>%
  summarize(member_casual = member_casual, time_category = time_category, 
            avg_duration = seconds_to_time(avg_duration))

View(avg_time)

ggplot(data = avg_time) +
  geom_col(mapping = aes(x = time_category, y = avg_duration, fill = member_casual),  
           width = 0.4, position = position_dodge(width = 0.4)) +
  labs(title = "Average ride duration by each users based on time", x = "Time of the day", y = "Ride duration",
       caption = "Data from May 22 to Apr 23")

## Average ride duration base on day

avg_day <- cy_clean %>%
  group_by(member_casual, weekday ) %>%
  summarize(avg_duration = mean(seconds)) %>%
  arrange(member_casual, weekday)

### Converting average results to time format for better viewing

avg_day <- avg_day %>%
  summarize(member_casual = member_casual, weekday = weekday, 
            avg_duration = seconds_to_time(avg_duration)) %>%
  arrange(member_casual, weekday)

View(avg_day)

ggplot(data = avg_day) +
  geom_col(mapping = aes(x = weekday, y = avg_duration, fill = member_casual),  
           width = 0.4, position = position_dodge(width = 0.4)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Average ride duration by each users based on day", y = "Ride duration",
       caption = "Data from May 22 to Apr 23")

## Average ride duration base on season

avg_season <- cy_clean %>%
  group_by(member_casual, season_category ) %>%
  summarize(avg_duration = mean(seconds)) %>%
  arrange(member_casual, season_category)

### Converting average results to time format for better viewing

avg_season <- avg_season %>%
  summarize(member_casual = member_casual, season_category = season_category, 
            avg_duration = seconds_to_time(avg_duration)) %>%
  arrange(member_casual, season_category)

View(avg_season)

ggplot(data = avg_season) +
  geom_col(mapping = aes(x = season_category, y = avg_duration, fill = member_casual),  
           width = 0.4, position = position_dodge(width = 0.4)) +
  labs(title = "Average ride duration by each users based on season", x = "Season", y = "Ride duration",
       caption = "Data from May 22 to Apr 23")

## Top 10 busiest stations

### Casual riders top 10 station
top_station_cas <- cy_clean %>%
  group_by(start_station_name, member_casual) %>%
  filter(member_casual == "casual", !is.na(start_station_name)) %>%
  summarize(count = n(), .groups="drop") %>%
  arrange(member_casual, desc(count)) %>%
  top_n(10)

### Member riders top 10 station
top_station_mem <- cy_clean %>%
  group_by(start_station_name, member_casual) %>%
  filter(member_casual == "member", !is.na(start_station_name)) %>%
  summarize(count = n(), .groups="drop") %>%
  arrange(member_casual, desc(count)) %>%
  top_n(10)

## Map Visualization

### Preparing coordinates for the most rides
cor_df <- cy_clean %>%
  filter(start_lng != end_lng & start_lat != end_lat) %>%
  group_by(start_lng, start_lat, end_lng, end_lat, member_casual, rideable_type) %>%
  summarize(total_rides = n(), .groups = "drop") %>%
  filter(total_rides > 100)

### Creating dataframes for each type of user
casual_users <- filter(cor_df, member_casual == "casual")
member_users <- filter(cor_df, member_casual == "member")

### Setting up the map
chicago <- get_stamenmap(bbox = c(left = -88.0, bottom = 41.5, right = -87.3, top = 42.1), maptype = "terrain")

### Plotting the map for casual riders
ggmap(chicago, darken = c(0.1, "white")) +
  geom_point(data = casual_users, aes(x = as.numeric(start_lng), y = as.numeric(start_lat), color = rideable_type), size = 2) +
  coord_fixed(0.8) +
  labs(title = "Most used routes by Casual riders", x = NULL, y = NULL) +
  theme(legend.position = "none")

### Plotting the map for member riders
ggmap(chicago, darken = c(0.1, "white")) +
  geom_point(data = member_users, aes(x = as.numeric(start_lng), y = as.numeric(start_lat), color = rideable_type), size = 2) +
  coord_fixed(0.8) +
  labs(title = "Most used routes by Member riders", x = NULL, y = NULL) +
  theme(legend.position = "none")
