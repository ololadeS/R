
##  Cyclists Data Analysis
Analysis to identify trends and pattern on how annual members and casual riders use of bikes

# Steps:
- Data preparation: Reading and understanding data
- Data Processing: Data cleaning, data manipulation
- Analyzing data: visualizing data
 

# Import libraries 
library ('lubridate')
library(ggplot2)
library(dplyr)
library(readr)


# Set the directory 
setwd("C:/Users/Admin/Desktop/Data") 

# Import datasets
bike_202108 <- read.csv("202108-divvy-tripdata.csv")
bike_202109 <- read.csv("202109-divvy-tripdata.csv")
bike_202110 <- read.csv("202110-divvy-tripdata.csv")
bike_202111 <- read.csv("202111-divvy-tripdata.csv")
bike_202112 <- read.csv("202112-divvy-tripdata.csv")
bike_202201 <- read.csv("202201-divvy-tripdata.csv")
bike_202202 <- read.csv("202202-divvy-tripdata.csv")
bike_202203 <- read.csv("202203-divvy-tripdata.csv")
bike_202204 <- read.csv("202204-divvy-tripdata.csv")
bike_202205 <- read.csv("202205-divvy-tripdata.csv")
bike_202206 <- read.csv("202206-divvy-tripdata.csv")
bike_202207 <- read.csv("202207-divvy-tripdata.csv")


# Merge all the CSV files into one dataframe
riders <- list.files(path="C:/Users/Admin/Desktop/Portfolio/Cyclistic_Bike-sharing/Data") %>% 
  lapply(read_csv) %>% 
  bind_rows 


# Read the first 3 rows of data
head(riders,3) 

# check the dimension/ number of rows and columns 
dim(riders) 


# Verify columns names 
colnames(riders) 

# check the structure/ datatype of the dataframe
str(riders)

# check for missing values
missing_num <- colSums(is.na(riders))
missing_num

# print the names of the columns with missing values
names(missing_num)[missing_num > 0]

# compute the percentage of missing values
percent_missing <- colSums(is.na(riders)) / nrow(riders) * 100
percent_missing


# delete rows with missing values
riders <- na.omit(riders)


# Extract year, month, and day from started_at column                       
riders$date <- as.Date(riders$started_at)
riders$month <- format(as.Date(riders$date), "%m")
riders$day <- format(as.Date(riders$date), "%d")
riders$year <- format(as.Date(riders$date), "%Y")
riders$day_of_week <- format(as.Date(riders$date), "%A")
riders$monthName <- format(riders$date, "%B")


# create a new column with the time in format "HMS"
riders$started_HMS <- paste0(sprintf("%02d", hour(riders$started_at)), ":",
                         sprintf("%02d", minute(riders$started_at)), ":",
                         sprintf("%02d", second(riders$started_at)))

riders$ended_HMS <- paste0(sprintf("%02d", hour(riders$ended_at)), ":",
                             sprintf("%02d", minute(riders$ended_at)), ":",
                             sprintf("%02d", second(riders$ended_at)))

# Check for missing values 'started_HMS'
sum(is.na(riders$started_HMS))
sum(is.na(riders$ended_HMS))

# Convert the HMS to seconds
riders$started_HMS <- period_to_seconds(hms(riders$started_HMS))
riders$ended_HMS <- period_to_seconds(hms(riders$ended_HMS))


# Check the shape after extracting
dim(riders) 


# calculate trip length
riders$trip_length <- riders$ended_HMS - riders$started_HMS 


# Drop columns with several missing values
riders <- riders %>%  
  select(-c(start_station_id, start_station_name, end_station_id, end_station_name))


# check the structure of the data frame
str(riders)


# Generate a statistical summary of the data
summary(riders)



--------# Exploratory Data Analysis

# Get the total number of each riders
ggplot(riders, aes(x = member_casual)) +
  geom_bar(stat = "count")

  
# Check how the trip length vary by user type
ggplot(riders, aes(x = member_casual, y = trip_length)) +
  geom_boxplot()


# Create a bar chart of the number of trips in a year by user type
ggplot(riders, aes(x = year, fill = member_casual)) +
  geom_bar(stat = "count") +
  facet_wrap(~ member_casual, ncol = 1)


# Compute the average trip duration by user type
avg_duration <- aggregate(trip_length ~ member_casual, riders, mean)


# Create a bar chart of the average trip duration by user type
ggplot(avg_duration, aes(x = member_casual, y = trip_length)) +
  geom_bar(stat = "identity")


# Create a chart of the number of trips by day of the week and by user type 
ggplot(riders, aes(x = day_of_week, fill = member_casual)) +
  geom_bar(stat = "count") +
  facet_wrap(~ member_casual, ncol = 1)


# Create a chart of the number of trips by month and by user type
ggplot(riders, aes(x = monthName, fill = member_casual)) +
  geom_bar(stat = "count") +
  facet_wrap(~ member_casual, ncol = 1)


# Create a stacked bar chart of the rideable type by user type
ggplot(riders, aes(x = member_casual, fill = rideable_type)) +
  geom_bar(stat = "count")



# Export data frame
write.table(riders, file='C:/Users/Admin/Desktop/Data/riders.csv')

