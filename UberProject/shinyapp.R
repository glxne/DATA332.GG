library(dplyr)
library(shiny)
library(lubridate)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(shiny)
library(png)

setwd("C:/Users/Gdot7/OneDrive/Documents/RScripts/UberProject")


merged_data <- read.csv("merged_data.csv")
adj_data <- read.csv("adj_tripbyhour.csv")
trips_by_hour <- read.csv("tripbyhour_pt.csv")
heat_data1 <- read.csv("heat_data1.csv")
heat_data2 <- read.csv("heat_data2.csv")
heat_data3 <- read.csv("heat_data3.csv")
heat_data4 <- read.csv("heat_data4.csv")
trips_summary <- read.csv("trips_summary.csv")
trips_summary2 <- read.csv("trips_summary2.csv")
trips_everyday <- read.csv("trips_everyday.csv")
tripcounts1 <- read.csv("tripcounts1.csv")
tripcounts2 <- read.csv("tripcounts2.csv")
tripcounts3 <- read.csv("tripcounts3.csv")
tripcounts4 <- read.csv("tripcounts4.csv")
predicted_table <- read.csv("predicted_table.csv")

textnum1 <- "For each graph I produced, I utilized the geom_col function. I generated a table that displays the number of trips for each hour, covering the time frame of hour 0 to 23.  "
textnum2 <- "I generated a chart to display the number of trips per hour per month, with each month represented by a different color for ease of differentiation. The month of Septmber seems to have a way higher amount of trips which is seen in the pink bars. "
textnum3 <- "I generated a graph that displays the number of trips by hour. Although similar to the previous graph that showed trips by hour and month, this one doesn't include the months. Instead, it shows the total number of trips for each hour across each month. "
textnum4 <- "Chart that illustrates the number of trips by day and month, There seems to be a trend in the bars. I would assume this is due to a spike in trips towards the end of the week, Thursday, Friday, Saturday, then dips back down on Sunday/Monday. "
textnum5 <- "6 graphs that depict the number of trips taken each day of the week in comparison to each month. "
textnum6 <- "Table that presents the number of trips by days and months. This table can be filtered to display days, trips, or months in a specific order "
textnum7 <- "A visualization to compare the number of trips with their corresponding bases. The graph features five columns representing each month and shows the number of trips associated with each specific base. It seems Base B02617 has the most trips overall, especially in August and September. "
textnum8 <- "One heatmap displays the number of trips per hour by day. The highest heat map values are for the later hours. However, the beginning and end of the heatmap appear to be reversed. "
textnum9 <- "This Heatmap displays the number of trips taken per day across different months. Compared to the heatmap showing trips per hour by day, this heatmap exhibits more scattered data points. "
textnum10 <- "The busiest months are the later ones, with higher trip numbers in the later weeks. "
textnum11 <- "This means that the number of trips taken from the first and last bases is lower compared to the number of trips taken from the bases in the middle. "
textnum12 <- "I created a leaflet that shows the geospatial location of the trips with the Longitude and Latitude provided in the data."
textnum13 <- "I created a prediction model that predicted trip count by day of week for each month for the year of 2015."



ui<-fluidPage( 
  
  tabsetPanel(
    
    tabPanel("Table1",
             fluidRow(
               column(2.5, textOutput("text_output1")),
               column(3.5,DT::dataTableOutput("table", width = "100%")))),
    
    tabPanel("chart1",
             fluidRow(
               column(7, textOutput("text_output2")),
               column(8,plotOutput('plot_01', width = '1000px')))),
    
    tabPanel("chart2",
             fluidRow(column(12, textOutput("text_output3")),
                      column(12,plotOutput('plot_02', width = '1000px')))),
    
    tabPanel("chart3",
             fluidRow(column(12, textOutput("text_output4")),
                      column(12,plotOutput('plot_03', width = '1000px')))),
    
    tabPanel("chart4",
             fluidRow(column(12, textOutput("text_output5")),
                      column(12,plotOutput('plot_04', width = '1000px')),
                      column(12,plotOutput('plot_05', width = '1000px')),
                      column(12,plotOutput('plot_06', width = '1000px')), 
                      column(12,plotOutput('plot_07', width = '1000px')), 
                      column(12,plotOutput('plot_08', width = '1000px')),
                      column(12,plotOutput('plot_09', width = '1000px')))), 
    
    
    tabPanel("Table2",
             fluidRow(
               column(12, textOutput("text_output6")),
               column(12,DT::dataTableOutput("table2", width = "100%")))),
    
    tabPanel("chart5",
             fluidRow(column(12, textOutput("text_output7")),
                      column(12,plotOutput('plot_10', width = '1000px')))), 
    
    tabPanel("heatmap1", 
             fluidRow(column(12, textOutput("text_output8")),
                      column(12,plotOutput('heatmap_1', width = '1000px')))),
    
    tabPanel("heatmap2",
             fluidRow(column(12, textOutput("text_output9")),
                      column(12,plotOutput('heatmap_2', width = '1000px')))),
    
    tabPanel("heatmap3", 
             fluidRow(column(12, textOutput("text_output10")),
                      column(12,plotOutput('heatmap_3', width = '1000px')))),
    
    tabPanel("heatmap4",
             fluidRow(column(12, textOutput("text_output11")),
                      column(12,plotOutput('heatmap_4', width = '1000px')))),
    
    tabPanel("MAP",
             fluidRow(column(12, textOutput("text_output12")),
                      column(12, leafletOutput("leaf")))),
    
    tabPanel("Predictive model", 
             fluidRow(column(12, textOutput("text_output13"))),
                 column(12, plotOutput("model1"))))
    
    
    
  )


server<-function(input,output){
  
  output$text_output1 <- renderText({ textnum1 })
  
  output$table <- DT::renderDataTable(trips_by_hour[,c("hour","trips_count")],options = list(pageLength = 4))
  
  output$text_output2 <- renderText({ textnum2 })
  
  output$plot_01 <- renderPlot({
    ggplot(adj_data, aes(x = hour, fill = Month)) +
      geom_bar(position = "dodge") +
      labs(title = "Trips by Hour and Month",
           x = "Hour of the Day",
           y = "Number of Trips",
           fill = "Month") +
      theme_minimal()
    
    
  })
  
  output$text_output3 <- renderText({ textnum3 })
  
  output$plot_02 <- renderPlot({
    ggplot(adj_data, aes(x = hour)) +
      geom_bar(stat = "count") +
      labs(title = "Trips by Hour",
           x = "Hour of the Day",
           y = "Number of Trips") +
      scale_y_continuous(labels = scales::number_format()) + 
      theme_minimal()
    
  })
  
  output$text_output4 <- renderText({ textnum4 })
  
  output$plot_03 <- renderPlot({
    ggplot(adj_data, aes(x = Day)) +
      geom_bar(stat = "count") +
      labs(title = "Trips by Day of the Month",
           x = "Day of the Month",
           y = "Number of Trips") +
      facet_wrap(~ Month) +
      theme_minimal()
  })
  
  output$text_output5 <- renderText({ textnum5 })
  
  output$plot_04 <- renderPlot({
  ggplot(trips_summary, aes(x = Month, y = Trip_Count, fill = Day_of_Week)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Trips by Month and Day of the Week",
         x = "Month",
         y = "Number of Trips",
         fill = "Day of the Week") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  output$text_output6 <- renderText({ textnum6 })
  
  output$table2 <- DT::renderDataTable(trips_everyday[,c("Day","Trip_Count", "Month")],options = list(pageLength = 4)) 
  
  output$text_output7 <- renderText({ textnum7 })
  
  output$plot_10 <- renderPlot({
    ggplot(trips_summary2, aes(x = Base, y = Trip_Count, fill = Month)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Trips by Bases and Month",
           x = "Base",
           y = "Number of Trips",
           fill = "Month") +
      scale_y_continuous(labels = scales::number_format()) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  }) 
  
  output$text_output8 <- renderText({ textnum8 })
  
  output$heatmap_1 <-renderPlot({
    ggplot(tripcounts1, aes(x = hour, y = Day, fill = trips)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +  
      labs(title = "Heatmap of Trips by Hour and Day",
           x = "Hour",
           y = "Day") +
      theme_minimal()  
  })
  
  output$text_output9 <- renderText({ textnum9 })
  
  output$heatmap_2 <- renderPlot({
    ggplot(tripcounts2, aes(x = Month, y = Day, fill = trips)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +  # Adjust the color scale as needed
      labs(title = "Heatmap of Trips by Month and Day",
           x = "Month",
           y = "Day") +
      theme_minimal()
  })
  
  output$text_output10 <- renderText({ textnum10 })
  
  output$heatmap_3 <- renderPlot({
    ggplot(tripcounts3, aes(x = Month, y = Week, fill = trips)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +  
      labs(title = "Heatmap of Trips by Month and Week",
           x = "Month",
           y = "Week") +
      theme_minimal()
  })
  
  output$text_output11 <- renderText({ textnum11 })
  
  output$heatmap_4 <- renderPlot({
    ggplot(tripcounts4, aes(x = Base, y = Day_of_Week, fill = trips)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +  # Adjust the color scale as needed
      labs(title = "Heatmap of Trips by Base and Day of the Week",
           x = "Base",
           y = "Day of the Week") +
      theme_minimal()
  })
  
  output$text_output12 <- renderText({ textnum12 })
  
  output$leaf <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addHeatmap(data = heat_data1, radius = 10, blur = 15)
  })
  
  output$text_output13 <- renderText({ textnum13 })
  
  output$model1 <- renderPlot({
    ggplot(predicted_table, aes(x = Day_of_Week, y = Predicted_Count, fill = Month)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Predicted Trip Counts by Day of the Week for Each Month for the Next Year",
           x = "Day of the Week",
           y = "Predicted Trip Count",
           fill = "Month") +
      theme_minimal()
  })
} 

shinyApp(ui=ui, server=server)