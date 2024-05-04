library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(readxl)
library(dplyr)
library(sparklyr)
library(tidyselect)
library(lubridate)
library(tidyr)
library(reshape2)
library(ggmap)
library(maps)
library(leaflet)
library(leaflet.extras)

setwd("C:/Users/Gdot7/OneDrive/Documents/RScripts/UberProject")

dfapril <- read.csv("uber-raw-data-apr14.csv")
dfaugust <- read.csv("uber-raw-data-aug14.csv")
dfjuly <- read.csv("uber-raw-data-jul14.csv")
dfjune <- read.csv("uber-raw-data-jun14.csv")
dfmay <- read.csv("uber-raw-data-may14.csv")
dfseptember <- read.csv("uber-raw-data-sep14.csv")

merged_data <- bind_rows(dfapril, dfaugust, dfjuly, dfjune, dfmay, dfseptember)

# Extracting day, month, and year from Date.Time Column and putting them into seperate columns
merged_data$Date.Time <- mdy_hms(merged_data$Date.Time)
merged_data$Day <- day(merged_data$Date.Time)
merged_data$Month <- month(merged_data$Date.Time, label = TRUE)
merged_data$Year <- year(merged_data$Date.Time)

# Changing Date.Time to just display time + Changing Name
merged_data$Date.Time <- format(merged_data$Date.Time, "%H:%M:%S")

colnames(merged_data)[colnames(merged_data) == "Date.Time"] <- "Time"

# Adjusting data frame in copy and making pivot table showing trip by hour
adj_tripbyhour <- merged_data

adj_tripbyhour <- adj_tripbyhour %>%
  mutate(hour = as.integer(substr(Time, 1, 2)))

tripbyhour_pt <- adj_tripbyhour %>%
  group_by(hour) %>%
  summarize(trips_count = n())

# Making chart to display trip by hour and month
adj_tripbyhour <- adj_tripbyhour %>%
  mutate(hour = as.integer(substr(Time, 1, 2)))

ggplot(adj_tripbyhour, aes(x = hour, fill = Month)) +
  geom_bar(position = "dodge") +
  labs(title = "Trips by Hour and Month",
       x = "Hour of the Day",
       y = "Number of Trips",
       fill = "Month") +
  theme_minimal()

# Chart that shows trips every hour
ggplot(adj_tripbyhour, aes(x = hour)) +
  geom_bar(stat = "count") +
  labs(title = "Trips by Hour",
       x = "Hour of the Day",
       y = "Number of Trips") +
  scale_y_continuous(labels = scales::number_format()) + 
  theme_minimal()

# Plot data showing trips every day of month
ggplot(adj_data, aes(x = Day)) +
  geom_bar(stat = "count") +
  labs(title = "Trips by Day of the Month",
       x = "Day of the Month",
       y = "Number of Trips") +
  facet_wrap(~ Month) +
  theme_minimal()

trips_countplot <- adj_tripbyhour %>%
  group_by(Month, Day) %>%
  summarise(Trip_Count = n())

ggplot(trips_countplot, aes(x = Day, y = Trip_Count, group = Month, color = Month)) +
  geom_line() +
  labs(title = "Trips by Day of the Month",
       x = "Day of the Month",
       y = "Number of Trips") +
  theme_minimal()
   # saving table
write.csv(trips_countplot, "tripsdaymonth.csv", row.names = FALSE)

#  Chart by trips day and month, creating day of the week column
adj_tripbyhour$Day <- factor(adj_tripbyhour$Day)
adj_tripbyhour$Day_of_Week <- weekdays(as.Date(paste(adj_tripbyhour$Month, adj_tripbyhour$Day), format = "%B %d"))

trips_summary <- adj_tripbyhour %>%
  group_by(Month, Day_of_Week) %>%
  summarise(Trip_Count = n())

ggplot(trips_summary, aes(x = Month, y = Trip_Count, fill = Day_of_Week)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trips by Month and Day of the Week",
       x = "Month",
       y = "Number of Trips",
       fill = "Day of the Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Last chart
trips_summary2 <- adj_tripbyhour %>%
  group_by(Base, Month) %>%
  summarise(Trip_Count = n())

ggplot(trips_summary2, aes(x = Base, y = Trip_Count, fill = Month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trips by Bases and Month",
       x = "Base",
       y = "Number of Trips",
       fill = "Month") +
  scale_y_continuous(labels = scales::number_format()) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


        # Heat Maps
# Heat map that displays by hour and day

tripcounts1 <- heat_data1 %>%
  group_by(hour, Day) %>%
  summarise(trips = n())


ggplot(tripcounts1, aes(x = hour, y = Day, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  
  labs(title = "Heatmap of Trips by Hour and Day",
       x = "Hour",
       y = "Day") +
  theme_minimal()




heat_data1 <- adj_tripbyhour %>%
  group_by(Lat, Lon, hour, Day) %>%
  summarise(Density = n())

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = heat_data1, radius = 10, blur = 15)

# Heat map by month and day

tripcounts2 <- heat_data2 %>%
  group_by(Month, Day) %>%
  summarise(trips = n())

ggplot(tripcounts2, aes(x = Month, y = Day, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust the color scale as needed
  labs(title = "Heatmap of Trips by Month and Day",
       x = "Month",
       y = "Day") +
  theme_minimal()

heat_data2 <- adj_tripbyhour %>%
  group_by(Lat, Lon, Month, Day) %>%
  summarise(Density = n())

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = heat_data2, radius = 10, blur = 15)

# Heat Map by week and month
    # Fixing existing table to make week and month column to make map
        # Functions below make week and month into numerical values, easier to make heat map
tripcounts3 <- heat_data3
tripcounts3$Month <- month.abb[tripcounts3$Month]

tripcounts3 <- tripcounts3 %>%
  group_by(Month, Week) %>%
  summarise(trips = n())

ggplot(tripcounts3, aes(x = Month, y = Week, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  
  labs(title = "Heatmap of Trips by Month and Week",
       x = "Month",
       y = "Week") +
  theme_minimal()

merged_data$Date <- as.Date(merged_data$Date.Time, format = "%m/%d/%Y")
merged_data$Week <- week(merged_data$Date)
merged_data$Month <- month(merged_data$Date)

heat_data3 <- merged_data %>%
  group_by(Lat, Lon, Week, Month) %>%
  summarise(Density = n())

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = heat_data3, radius = 10, blur = 15)

# Heat map of Base and Day_Of_Week

tripcounts4 <- heat_data4 %>%
  group_by(Base, Day_of_Week) %>%
  summarise(trips = n())

ggplot(tripcounts4, aes(x = Base, y = Day_of_Week, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  
  labs(title = "Heatmap of Trips by Base and Day of the Week",
       x = "Base",
       y = "Day of the Week") +
  theme_minimal()

heat_data4 <- adj_tripbyhour %>%
  group_by(Lat, Lon, Base, Day_of_Week) %>%
  summarise(Density = n())

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = heat_data4, radius = 10, blur = 15)


trips_table <- adj_data
trips_table$Day <- factor(trips_table$Day, levels = 1:31)
daily_entry_count <- aggregate(. ~ Day, data = trips_table, FUN = length)
daily_entry_summary <- data.frame(
  Day = 1:31, 
  Total_Entries = 0  
)
daily_entry_summary$Total_Entries[daily_entry_summary$Day %in% daily_entry_count$Day] <- daily_entry_count$Day


trips_everyday <- adj_data %>%
  group_by(Day, Month) %>%
  summarise(Trip_Count = n())
# Predictive Model
day_of_week_data <- trips_summary %>%
  group_by(Day_of_Week) %>%
  summarise(avg_trip_count = mean(Trip_Count))

model <- lm(avg_trip_count ~ Day_of_Week, data = day_of_week_data)

november_days <- data.frame(Day_of_Week = levels(day_of_week_data$Day_of_Week))

day_of_week_levels <- levels(trips_summary$Day_of_Week)

november_synthetic <- data.frame(
  Day_of_Week = sample(day_of_week_levels, n, replace = TRUE),  
  Month = rep("Nov", n), 
)

  ggplot(day_of_week_data, aes(x = Day_of_Week, y = avg_trip_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Number of Trips by Day of the Week Prediction for November",
       x = "Day of the Week",
       y = "Average Number of Trips") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # P Model2
  
nov_prediction <- trips_summary

nov_prediction$Month <- as.factor(nov_prediction$Month)
nov_prediction$Day_of_Week <- as.factor(nov_prediction$Day_of_Week)

model <- lm(Trip_Count ~ Month + Day_of_Week, data = nov_prediction)


months <- levels(nov_prediction$Month)
days_of_week <- levels(nov_prediction$Day_of_Week)

months <- levels(nov_prediction$Month)
days_of_week <- levels(nov_prediction$Day_of_Week)

predicted_counts <- array(NA, dim = c(length(months), length(days_of_week)))

for (m in 1:length(months)) {
  for (d in 1:length(days_of_week)) {
    new_data <- data.frame(Month = factor(months[m], levels = levels(nov_prediction$Month)),
                           Day_of_Week = factor(days_of_week[d], levels = levels(nov_prediction$Day_of_Week)))
    predicted_counts[m, d] <- predict(model, newdata = new_data)
  }
}

predicted_counts

predicted_table <- data.frame(
  Month = character(), 
  Day_of_Week = character(),  
  Predicted_Count = numeric(),  
  stringsAsFactors = FALSE  
)

for (m in 1:length(months)) {
  for (d in 1:length(days_of_week)) {
    new_data <- data.frame(
      Month = factor(months[m], levels = levels(nov_prediction$Month)),
      Day_of_Week = factor(days_of_week[d], levels = levels(nov_prediction$Day_of_Week))
    )
    predicted_count <- predict(model, newdata = new_data)
    predicted_table <- rbind(predicted_table, data.frame(Month = months[m], Day_of_Week = days_of_week[d], Predicted_Count = predicted_count))
  }
}

ggplot(predicted_table, aes(x = Day_of_Week, y = Predicted_Count, fill = Month)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Predicted Trip Counts by Day of the Week for Each Month for the Next Year",
       x = "Day of the Week",
       y = "Predicted Trip Count",
       fill = "Month") +
  theme_minimal()

saveRDS(merged_data, "merged_data.rds")

merged_data <- readRDS("merged_data.rds")
write.csv(predicted_table, file = "predicted_table.csv", row.names = FALSE)
write.csv(tripcounts1, file = "tripcounts1.csv", row.names = FALSE)
write.csv(tripcounts2, file = "tripcounts2.csv", row.names = FALSE)
write.csv(tripcounts3, file = "tripcounts3.csv", row.names = FALSE)
write.csv(tripcounts4, file = "tripcounts4.csv", row.names = FALSE)
write.csv(trips_everyday, file = "trips_everyday.csv", row.names = FALSE)
write.csv(trips_summary, file = "trips_summary.csv", row.names = FALSE)
write.csv(trips_summary2, file = "trips_summary2.csv", row.names = FALSE)
write.csv(heat_data1, file = "heat_data1.csv", row.names = FALSE)
write.csv(heat_data2, file = "heat_data2.csv", row.names = FALSE)
write.csv(heat_data3, file = "heat_data3.csv", row.names = FALSE)
write.csv(heat_data4, file = "heat_data4.csv", row.names = FALSE)
write.csv(merged_data, file = "merged_data.csv", row.names = FALSE)
write.csv(adj_tripbyhour, file = "adj_tripbyhour.csv", row.names = FALSE)
write.csv(tripbyhour_pt, file = "tripbyhour_pt.csv", row.names = FALSE)