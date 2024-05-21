# Final Project - DATA-332 Spring 2024
## Authors: Gianni G., Sam M.
## Overview
  In this project we preformed analysis on country data provided to us via Kaggle. The two main tables include many variables with one containing data such as average IQ and literacy rate. The second contained random data from each country where we were able to run analysis to see if there was any correlation between the random data and IQ/Literacy rate. We created a ShinyApp to display our findings and short analysis.

# Cleaning Data

**1. Inserted data files. We read them in UTF-8 format for a reason we will explain later.
```r
CountryIQ <- read.csv("avgIQpercountry.csv", fileEncoding = "UTF-8")
countrydata <- read.csv("world-data-2023.csv", fileEncoding = "UTF-8")
```
**2. Changed the names of the countries to all lower case to prepare for an inner join
```r
countrydata <- countrydata %>%
  mutate(Country = trimws(tolower(Country)))

CountryIQ <- CountryIQ %>%
  mutate(Country = trimws(tolower(Country)))
```
**3. We changed some of the country names that were spelled different in the tables. Ex. Turkiye/Turkey or Gambia/The Gambia. We ran into a problem with the format of the .csv files which resulted in one of the names to be displayed with the "�" character due to the accents over the letters in the real name. Reading them in UTF-8 format allowed us to copy the name to change it using the mutate function. 
```r
CountryIQ <- CountryIQ %>%
  mutate(Country = ifelse(Country == "türkiye", "turkey", Country))

CountryIQ <- CountryIQ %>%
  mutate(Country = ifelse(Country == "grenade", "grenada", Country))

CountryIQ <- CountryIQ %>%
  mutate(Country = ifelse(Country == "moldavia", "moldova", Country))

CountryIQ <- CountryIQ %>%
  mutate(Country = ifelse(Country == "congo republic", "democratic republic of the congo", Country))

CountryIQ <- CountryIQ %>%
  mutate(Country = ifelse(Country == "gambia", "the gambia", Country))

countrydata <- countrydata %>%
  mutate(Country = ifelse(Country == "s�����������", "sao tome and principe", Country))

countrydata <- countrydata %>%
  mutate(Country = ifelse(Country == "the bahamas", "bahamas", Country))

countrydata <- countrydata %>%
  mutate(Country = ifelse(Country == "palestinian national authority", "palestine", Country))

countrydata <- countrydata %>%
  mutate(Country = ifelse(Country == "republic of the congo", "congo", Country))

countrydata <- countrydata %>%
  mutate(Country = ifelse(Country == "republic of ireland", "ireland", Country))
```
**4. The tables had a different amount/selection of countries. They both contained most of the countries in the world but not all. Both contained countries the other didn't so we used the following function to print the missing countries from each table when compared to each other.
```r
missing_in_table2 <- setdiff(countrydatafilter$Country, countryiqfilter$Country)
missing_in_table1 <- setdiff(countryiqfilter$Country, countrydatafilter$Country)

cat("Countries in table1 but not in table2:\n")
print(missing_in_table2)

cat("\nCountries in table2 but not in table1:\n")
print(missing_in_table1)
```
**5. After finding all of the countries that weren't listed on both, we wrote a function to remove the country and the whole row of data correlating with that country.
```r
countries_to_remove1 <- c("ivory coast", "guinea-bissau", "monaco", "saint kitts and nevis", "tonga",
                          "cape verde", "vatican city", "nauru", "samoa", "tuvalu", "equitorial guinea",
                          "kiribati", "palau", "san marino", "equatorial guinea")

countrydatafilter <- countrydata %>%
  filter(!Country %in% countries_to_remove1)

countries_to_remove_2 <- c("taiwan", "new caledonia", "cayman islands", "british virgin islands",
                           "hong kong", "bermuda", "puerto rico", "saint helena", "macao", 
                           "turks and caicos islands", "northern mariana islands", "costa do marfim")

countryiqfilter <- CountryIQ %>%
  filter(!Country %in% countries_to_remove_2)
```
**6. Now that the tables had the same amount and same values in the Country columns, we inner joined the tables using the Country column.
```r
countrycombined <- inner_join(countryiqfilter, countrydatafilter, by = "Country")
```
## Analysis
We ran analysis using the variables in the table we created. All of the graphs can be found on the ShinyApp, including a short analysis of the graph. Here is how we created the graphs.
**1. Average IQ by Continent
```r
continent_iq <- countrycombined %>%
  group_by(Continent) %>%
  summarize(average_IQ = mean(Average.IQ))

ggplot(continent_iq, aes(x = Continent, y = average_IQ, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Average IQ by Continent",
       x = "Continent",
       y = "Average IQ") +
  theme_minimal()
```
**2. Literacy Rate by country, Top and Bottom 10
```r
countrycombined <- countrycombined[order(countrycombined$Literacy.Rate),]

top_10 <- head(countrycombined, 10)
bottom_10 <- tail(countrycombined, 10)

subset_data <- rbind(top_10, bottom_10)

ggplot(subset_data, aes(x = reorder(Country, Literacy.Rate), y = Literacy.Rate)) +
  geom_point(size = 3, color = "skyblue") +  
  geom_segment(aes(xend = Country, yend = 0), color = "gray", size = 0.2) +  
  coord_flip() +  
  labs(x = "Country", y = "Literacy Rate", title = "Top and Bottom 10 Literacy Rates by Country") +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 8))
```
**3. Average Consumer Price Index by Continent. We ran into an issue with the formatting of the numbers in the CPI column so we made sure that the value type was numeric.
```r
avg_cpi_by_continent <- aggregate(CPI ~ Continent, data = countrycombined, FUN = mean)

countrycombined$CPI <- as.numeric(gsub(",", "", countrycombined$CPI))

avg_cpi_by_continent <- aggregate(CPI ~ Continent, data = countrycombined, FUN = mean)

ggplot(avg_cpi_by_continent, aes(x = Continent, y = CPI)) +
  geom_bar(stat = "identity", fill = "skyblue") +  
  labs(x = "Continent", y = "Average CPI", title = "Average CPI by Continent") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```
**4. Nobel Prizes comapred to mean years of school, by country (only countries that have won nobel prizes were included).
```r
filterednobel <- countrycombined[countrycombined$Nobel.Prices > 0, ]

countrycombined <- countrycombined %>%
  rename(Mean_Years_of_Schooling = "Mean.years.of.schooling...2021")

ggplot(filterednobel, aes(x = Nobel.Prices, y = Mean_Years_of_Schooling, label = Country)) +
  geom_point(color = "skyblue") +  
  geom_text(hjust = -0.1, vjust = 0.5, size = 3) +  
  labs(x = "Number of Nobel Prizes", y = "Mean Years of Schooling", title = "Nobel Prizes vs. Mean Years of Schooling") +
  theme_minimal()
```
**5. Comparing countries who have won nobel prizes and their GDP, follow-up to previous graph.
```r
filterednobel$GDP <- as.numeric(gsub("[^0-9\\.]", "", filterednobel$GDP))

ggplot(filterednobel, aes(x = GDP, y = Country)) +
  geom_point(color = "skyblue") +  
  labs(x = "GDP", y = "Country", title = "GDP by Country") +
  scale_x_continuous(labels = scales::comma_format())
```
## All data visualizations can be found on the ShinyApp, FinalRandApp.R file to run.
