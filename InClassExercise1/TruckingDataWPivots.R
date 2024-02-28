library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(stringr)
library(stringi)

rm(list=ls())

#setting up directory
setwd('/Users/Gdot7/OneDrive/Documents/RScripts/Trucker') 

df_truck <- read_excel('NP_EX_1-2.xlsx', sheet = 2, skip = 3, .name_repair = 'universal')

# selecting columns
df <- df_truck[, c(4:15)]
# deselect columns
df <- subset(df, select = -c(...10))

# getting difference in days within a column
date_min <- min(df$Date)
date_max <- max(df$Date)

number_of_days_on_the_road <- date_max - date_min
print(number_of_days_on_the_road)

days <- difftime(max(df$Date), min(df$Date))
print(days)
number_of_days_recorded <- nrow(df)

total_hours = sum(df$Hours)
avg_hrs_per_day_rec <- round(total_hours / number_of_days_recorded, digits = 3)

df$fuel_cost <- df$Gallons * df$Price.per.Gallon

df$total_expenses <- df$fuel_cost + df$Misc + df$Tolls
total_expenses_sum <- sum(df$total_expenses)

total_gallons_consumed <- sum(df$Gallons)

df$miles_driven <- df$Odometer.Ending - df$Odometer.Beginning
total_miles_driven <- sum(df$miles_driven)

avg_mpg <- total_miles_driven / total_gallons_consumed

df$cost_per_mile <- df$miles_driven / df$total_expenses

total_cost_per_mile <- sum(df$cost_per_mile)

df[c('start_warehouse', 'start_city_state')] <- str_split_fixed(df$Starting.Location, ',', 2)

df_starting_pivot <- df %>%
  group_by(start_city_state) %>%
  summarize(count=n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm= TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_starting_pivot, aes(x = start_city_state , y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))

df[c('delivery_warehouse', 'delivery_city_state')] <- str_split_fixed(df$Delivery.Location, ',', 2)

df_delivery_pivot <- df %>%
  group_by(delivery_city_state) %>%
  summarize(count=n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm= TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_delivery_pivot, aes(x = delivery_city_state , y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))

df_dwarehouse_pivot <- df %>%
  group_by(delivery_warehouse) %>%
  summarize(count=n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm= TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_dwarehouse_pivot, aes(x = delivery_warehouse , y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))

df_startwarehouse_pivot <- df %>%
  group_by(start_warehouse) %>%
  summarize(count=n(),
            mean_size_hours = mean(Hours, na.rm = TRUE),
            sd_hours = sd(Hours, na.rm= TRUE),
            total_hours = sum(Hours, na.rm = TRUE),
            total_gallons = sum(Gallons, na.rm = TRUE)
  )

ggplot(df_startwarehouse_pivot, aes(x = start_warehouse , y = count)) +
  geom_col() +
  theme(axis.text = element_text(angle = 45, vjust = .5, hjust = 1))
