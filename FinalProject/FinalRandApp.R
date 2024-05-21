library(dplyr)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(readxl)

setwd("C:/Users/Gdot7/OneDrive/Documents/RScripts/FinalProject")

CountryIQ <- read.csv("avgIQpercountry.csv", fileEncoding = "UTF-8")
incarceration <- read.csv("incarceration.csv", fileEncoding = "UTF-8")
countrydata <- read.csv("world-data-2023.csv", fileEncoding = "UTF-8")

countrydata <- countrydata %>%
  mutate(Country = trimws(tolower(Country)))

CountryIQ <- CountryIQ %>%
  mutate(Country = trimws(tolower(Country)))

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



missing_in_table2 <- setdiff(countrydatafilter$Country, countryiqfilter$Country)


missing_in_table1 <- setdiff(countryiqfilter$Country, countrydatafilter$Country)


cat("Countries in table1 but not in table2:\n")
print(missing_in_table2)

cat("\nCountries in table2 but not in table1:\n")
print(missing_in_table1)

#removing countries that tables don't have in common

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

countrycombined <- inner_join(countryiqfilter, countrydatafilter, by = "Country")

# Average IQ by continent
continent_iq <- countrycombined %>%
  group_by(Continent) %>%
  summarize(average_IQ = mean(Average.IQ))

ggplot(continent_iq, aes(x = Continent, y = average_IQ, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Average IQ by Continent",
       x = "Continent",
       y = "Average IQ") +
  theme_minimal()

# Literacy Rate by country top 10 and bottom 10
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

# Average CPI by continent
avg_cpi_by_continent <- aggregate(CPI ~ Continent, data = countrycombined, FUN = mean)

countrycombined$CPI <- as.numeric(gsub(",", "", countrycombined$CPI))

avg_cpi_by_continent <- aggregate(CPI ~ Continent, data = countrycombined, FUN = mean)

ggplot(avg_cpi_by_continent, aes(x = Continent, y = CPI)) +
  geom_bar(stat = "identity", fill = "skyblue") +  
  labs(x = "Continent", y = "Average CPI", title = "Average CPI by Continent") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Nobel Prizes compared to mean years of schooling
filterednobel <- countrycombined[countrycombined$Nobel.Prices > 0, ]

countrycombined <- countrycombined %>%
  rename(Mean_Years_of_Schooling = "Mean.years.of.schooling...2021")

ggplot(filterednobel, aes(x = Nobel.Prices, y = Mean_Years_of_Schooling, label = Country)) +
  geom_point(color = "skyblue") +  
  geom_text(hjust = -0.1, vjust = 0.5, size = 3) +  
  labs(x = "Number of Nobel Prizes", y = "Mean Years of Schooling", title = "Nobel Prizes vs. Mean Years of Schooling") +
  theme_minimal()

# Comparing countries who have won Nobel Prizes GDP
filterednobel$GDP <- as.numeric(gsub("[^0-9\\.]", "", filterednobel$GDP))

ggplot(filterednobel, aes(x = GDP, y = Country)) +
  geom_point(color = "skyblue") +  
  labs(x = "GDP", y = "Country", title = "GDP by Country") +
  scale_x_continuous(labels = scales::comma_format())

# Creating Shiny App

textnum1 <- "I utilized the ggplot package to graph the average IQ by continent. Europe and North America are very close, with Europe just a couple points over. Africa is the lowest falling below 75"
textnum2 <- "I next created a table of the top and bottom 10 countries with the highest/lowest literacy rate which ranges between 0.00-1.00. All of the countries with the highest have the same literacy rate, while the bottom 10 countries has more variable literacy rates."
textnum3 <- "I took the average Consumer Price Index which essentially is the index of variation in prices paid by consumers for retail goods. South America by far has the highest CPI with Africa coming in close 2nd. The Europe/Asia variable is for countries in West Asia such as Armenia, Georgia, Turkey, etc. where they can apply to either continents"
textnum4 <- "I made a scatter plot to show the number of Nobel Prizes compared to the Mean years of schooling in that country. More than half of the countries in the data frame haven't won nobel prizes so I excluded them. I labeled the dots which is why some overlap but I did it mainly to show outliers such as the US as they have won the most amount of Nobel Prizes by a longshot."
textnum5 <- "In relation to the last graph, I created another scatter plot of the countries who have won nobel prizes to show their GDP which shows similarity to the last plot where the US was also an outlier."


### APP

ui<-fluidPage( 
  
  tabsetPanel(
    
    tabPanel("chart1",
             fluidRow(
               column(7, textOutput("text_output1")),
               column(8,plotOutput('plot_01', width = '1000px')))),
    
    tabPanel("chart2",
             fluidRow(
               column(7, textOutput("text_output2")),
               column(8,plotOutput('plot_02', width = '1000px')))),
    
    tabPanel("chart3",
             fluidRow(column(12, textOutput("text_output3")),
                      column(12,plotOutput('plot_03', width = '1000px')))),
    
    tabPanel("chart4",
             fluidRow(column(12, textOutput("text_output4")),
                      column(12,plotOutput('plot_04', width = '1000px')))),
    
    tabPanel("chart5",
             fluidRow(column(12, textOutput("text_output5")),
                      column(12,plotOutput('plot_05', width = '1000px')))),
                      
    
  )
)
  server <- function(input, output) {
    
    output$text_output1 <- renderText({ textnum1 })
    
    output$plot_01 <- renderPlot({
      ggplot(continent_iq, aes(x = Continent, y = average_IQ, fill = Continent)) +
        geom_bar(stat = "identity") +
        labs(title = "Average IQ by Continent",
             x = "Continent",
             y = "Average IQ") +
        theme_minimal()
    })
    
    output$text_output2 <- renderText({ textnum2 })
    
    output$plot_02 <- renderPlot({
      ggplot(subset_data, aes(x = reorder(Country, Literacy.Rate), y = Literacy.Rate)) +
        geom_point(size = 3, color = "skyblue") +  
        geom_segment(aes(xend = Country, yend = 0), color = "gray", size = 0.2) +  
        coord_flip() +  
        labs(x = "Country", y = "Literacy Rate", title = "Top and Bottom 10 Literacy Rates by Country") +
        theme_minimal() + 
        theme(axis.text.y = element_text(size = 8))
    })
    
    output$text_output3 <- renderText({ textnum3 })
    
    output$plot_03 <- renderPlot({
      ggplot(avg_cpi_by_continent, aes(x = Continent, y = CPI)) +
        geom_bar(stat = "identity", fill = "skyblue") +  
        labs(x = "Continent", y = "Average CPI", title = "Average CPI by Continent") +
        theme_minimal() +  
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$text_output4 <- renderText({ textnum4 })
    
    output$plot_04 <- renderPlot({
      ggplot(filterednobel, aes(x = Nobel.Prices, y = Mean_Years_of_Schooling, label = Country)) +
        geom_point(color = "skyblue") +  
        geom_text(hjust = -0.1, vjust = 0.5, size = 3) +  
        labs(x = "Number of Nobel Prizes", y = "Mean Years of Schooling", title = "Nobel Prizes vs. Mean Years of Schooling") +
        theme_minimal()
    })
    
    output$text_output5 <- renderText({ textnum5 })
    
    output$plot_05 <- renderPlot({
      ggplot(filterednobel, aes(x = GDP, y = Country)) +
        geom_point(color = "skyblue") +  
        labs(x = "GDP", y = "Country", title = "GDP by Country") +
        scale_x_continuous(labels = scales::comma_format())
    })
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)