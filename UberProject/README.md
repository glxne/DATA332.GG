# Uber Project
## Author: Gianni G.
## Overview
  In this project I preformed analysis on raw Uber data provided to me by creating charts and geospatial heat maps to find any significance in the data.

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
**2. Fixed the date and time format and put variables like week, day, month, and year into seperate columns to prepare for analysis. This left just the time in the column

```r
merged_data$Date.Time <- mdy_hms(merged_data$Date.Time)
merged_data$Day <- day(merged_data$Date.Time)
merged_data$Month <- month(merged_data$Date.Time, label = TRUE)
merged_data$Year <- year(merged_data$Date.Time)
```
