# Uber Project
## Author: Gianni G.
## Overview
  In this project I preformed analysis on raw Uber data provided to me by creating charts and geospatial heat maps to find any significance in the data. All heat maps and charts used for analysis can be found in the shiny app program.

### Cleaning Data

**1. Inserted data filed and combined them

```r
dfapril <- read.csv("uber-raw-data-apr14.csv")
dfaugust <- read.csv("uber-raw-data-aug14.csv")
dfjuly <- read.csv("uber-raw-data-jul14.csv")
dfjune <- read.csv("uber-raw-data-jun14.csv")
dfmay <- read.csv("uber-raw-data-may14.csv")
dfseptember <- read.csv("uber-raw-data-sep14.csv")

merged_data <- bind_rows(dfapril, dfaugust, dfjuly, dfjune, dfmay, dfseptember)
```
**2. Fixed the date and time format and put variables like week, day, month, and year into seperate columns to prepare for analysis. This left just the time in the "Date.Time" column which I changed to time. Adjusting the dates was easier with the lubridates package which can be installed in R Studio

```r
merged_data$Date.Time <- mdy_hms(merged_data$Date.Time)
merged_data$Day <- day(merged_data$Date.Time)
merged_data$Month <- month(merged_data$Date.Time, label = TRUE)
merged_data$Year <- year(merged_data$Date.Time)

merged_data$Date.Time <- format(merged_data$Date.Time, "%H:%M:%S")

colnames(merged_data)[colnames(merged_data) == "Date.Time"] <- "Time"
```
**3. Made a copy of data frame to manipulate to make a pivot table to display trips each hour by month

```r
adj_tripbyhour <- merged_data

adj_tripbyhour <- adj_tripbyhour %>%
  mutate(hour = as.integer(substr(Time, 1, 2)))

tripbyhour_pt <- adj_tripbyhour %>%
  group_by(hour) %>%
  summarize(trips_count = n())
```
**4. Created Day of the Week column

```r
adj_tripbyhour$Day_of_Week <- weekdays(as.Date(paste(adj_tripbyhour$Month, adj_tripbyhour$Day), format = "%B %d"))
```

### Creating Tables, Charts, and Maps necessary for analysis

**1. Created pivot table to show trips by hour. Kept in 24-hour time so there isn't overlap in time
```r
tripbyhour_pt <- adj_tripbyhour %>%
  group_by(hour) %>%
  summarize(trips_count = n())
```

**2. Created bar chart to show trips by hour and month
```r
ggplot(adj_tripbyhour, aes(x = hour, fill = Month)) +
  geom_bar(position = "dodge") +
  labs(title = "Trips by Hour and Month",
       x = "Hour of the Day",
       y = "Number of Trips",
       fill = "Month") +
  theme_minimal()
```

**3. Created chart that shows number of trips each our
```r
ggplot(adj_tripbyhour, aes(x = hour)) +
  geom_bar(stat = "count") +
  labs(title = "Trips by Hour",
       x = "Hour of the Day",
       y = "Number of Trips") +
  scale_y_continuous(labels = scales::number_format()) + 
  theme_minimal()
```

**4. Created chart to show number of trips each day of each month
```r
ggplot(adj_tripbyhour, aes(x = Day)) +
  geom_bar(stat = "count") +
  labs(title = "Trips by Day of the Month",
       x = "Day of the Month",
       y = "Number of Trips") +
  facet_wrap(~ Month) +
  theme_minimal()
```

**5. Created chart of trips of day of the week and month
```r
ggplot(trips_summary, aes(x = Month, y = Trip_Count, fill = Day_of_Week)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trips by Month and Day of the Week",
       x = "Month",
       y = "Number of Trips",
       fill = "Day of the Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

**6. Created chart by Bases and Month
```r
ggplot(trips_summary2, aes(x = Base, y = Trip_Count, fill = Month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trips by Bases and Month",
       x = "Base",
       y = "Number of Trips",
       fill = "Month") +
  scale_y_continuous(labels = scales::number_format()) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
### Heat Maps - Geospatial with the help of leaflet package
**7. Heat Map of hour by day
```r
heat_data1 <- adj_tripbyhour %>%
  group_by(Lat, Lon, hour, Day) %>%
  summarise(Density = n())

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = heat_data1, radius = 10, blur = 15)
```

**8. Heat Map by month and day
```r
heat_data2 <- adj_tripbyhour %>%
  group_by(Lat, Lon, Month, Day) %>%
  summarise(Density = n())

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = heat_data2, radius = 10, blur = 15)
```

**9. Heat map by week and month
```r
heat_data3 <- merged_data %>%
  group_by(Lat, Lon, Week, Month) %>%
  summarise(Density = n())

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = heat_data3, radius = 10, blur = 15)
```

**10. Heat map of Base and Day of the week
```r
heat_data4 <- adj_tripbyhour %>%
  group_by(Lat, Lon, Base, Day_of_Week) %>%
  summarise(Density = n())

leaflet() %>%
  addTiles() %>%
  addHeatmap(data = heat_data4, radius = 10, blur = 15)
```
### Creating Prediction Model
**Description: I decided to build a predictive model for the amount of trips every weekday for each month, but for the year of 2015. I used an existing table that I used to analyze the same idea but for the existing data.

**1. After making a copy of the table so I can manipulate it, I made sure the day of the week and the month columns were encoded correctly
```r
nov_prediction$Month <- as.factor(nov_prediction$Month)
nov_prediction$Day_of_Week <- as.factor(nov_prediction$Day_of_Week)
```

**2. I then used linear regression for my model selection to keep things simple
```r
model <- lm(Trip_Count ~ Month + Day_of_Week, data = nov_prediction)
```

**3. I then made a prediction for each day of the week, for each month. This prediction is based on the linear regression model ran on the existing data for the year of 2014.
```r
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
```

**4. I created a data frame and put the prediction data into it.
```r
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
```

**5. Lastly I created a chart of the prediction data similar to the original using ggplot()
```r
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
```

### Analysis of charts can be found on shiny app



