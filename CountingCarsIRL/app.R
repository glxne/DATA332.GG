library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(readxl)

df <- readRDS("Combined_Car_Data.rds")
df["Collector.Name"] <- NULL 
df["Date"] <- NULL

column_names <- colnames(df)

ui <- dashboardPage(
  dashboardHeader(title = "Counting Cars IRL"), 
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), 
      menuItem("Data Set", tabName = "data", icon = icon("database")), 
      menuItem("Analysis", tabName = "analysis", icon = icon("signal")) 
    )
  ), 
  
  dashboardBody(
    tabItems(
      # Dashboard Items
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 4, 
                       box(title = "About", status = "primary", solidHeader = TRUE, width = NULL,
                           HTML('<div style="font-size: 20px;">
                Radar speed signs track the speed that drivers are going to encourage safe 
                driving practices, such as driving within the speed limit and keeping pedestrians safe. 
                Many statitical analyses of these signs have shown that drivers decrease their speeds when
                radar speeds are installed. Our goal in this project was to determine if these claims from the radar 
                companies and other analyses of the technology are true.
                </div>'), 
                           br(),
                           br(),
                           HTML('<div style="font-size: 20px;">
                For this project, our group, along with the rest of our Data-332 class hand collected data about the 
                speed of the cars, in Miles Per Hour, the time of day the car was observed, the weather conditions, 
                temperature, the day of the week, and the other variables mentioned above. Our group collected 
                150 car data points, 50 per person. 
                </div>'), 
                           br(), 
                           br(),
                           HTML('<div style="font-size: 20px;">
                The data collected can be observed in the “Data Set” page, and an analysis of the data can be lookd 
                at on the “Analysis” page.
                </div>'), 
                           
                       ) 
                ),
                column(width = 4, 
                       valueBoxOutput("num_data_points", width = NULL),
                       valueBoxOutput("num_variables", width = NULL), 
                       valueBoxOutput("max_mph", width = NULL), 
                       valueBoxOutput("min_mph", width = NULL), 
                       valueBoxOutput("mean_mph", width = NULL), 
                       valueBoxOutput("median_mph", width = NULL)
                       
                ),
                column(width = 4, 
                       box(title = "Authors", status = "primary", width = NULL, solidHeader = TRUE, 
                           HTML('<div style="font-size: 20px;">Gavin McCorry</div>'), 
                           br(),
                           HTML('<div style="font-size: 20px;">Gianni Gubbins</div>'), 
                           br(),
                           HTML('<div style="font-size: 20px;">Sam Mulugeta</div>')
                       )
                )
              )
      ),
      
      # Data Set Items
      tabItem(tabName = "data",
              titlePanel(title = "Explore IRL Cars Data Set"),
              DT::dataTableOutput("table")
      ),
      
      tabItem(tabName = "analysis", 
              fluidRow(
                column(3, 
                       selectInput('X', 'Choose X', column_names, selected = column_names[1]), 
                       selectInput('Y', 'Choose Y', column_names, selected =   column_names[4]), 
                       selectInput('Splitby', 'Split By', column_names, column_names[1])
                ),
                column(6,
                       box(plotOutput('plot_01'), width = NULL), 
                       box(plotOutput('plot_02'), width = NULL)
                )
              )
      )
    ),
    
    tags$head(
      tags$style(
        HTML(
          ".selectize-control { width: 100% !important; }"
        )
      )
    )
  )
)

server <- function(input, output) {
  output$num_data_points <- renderValueBox({
    valueBox(nrow(df), "Number of Observations", icon = icon("eye"), color = "light-blue")
  })
  
  output$num_variables <- renderValueBox({
    valueBox(ncol(df), "Number of Variables", icon = icon("list"), color = "light-blue")
  })
  
  output$max_mph <- renderValueBox({
    valueBox(max(df$MPH, na.rm = T), "Max MPH", icon = icon("maximize"), color = "light-blue")
  })
  
  output$min_mph <- renderValueBox({
    valueBox(min(df$MPH, na.rm = T), "Min MPH", icon = icon("minimize"), color = "light-blue")
  })
  
  output$mean_mph <- renderValueBox({
    valueBox(round(mean(df$MPH, na.rm = T), 2), "Mean MPH", icon = icon("percent"), color = "light-blue")
  })
  
  output$median_mph <- renderValueBox({
    valueBox(median(df$MPH, na.rm = T), "Median MPH", icon = icon("plus"), color = "light-blue")
  })
  
  # For Table displaying data set
  output$table <- DT::renderDataTable(df, options = list(pagelength = 4))
  
  # Displaying chart in analysis
  output$plot_01 <- renderPlot({
    if (input$X == "Time") {
      
      df$Time <- as.numeric(format(as.POSIXct(df$Time, format = "%I:%M %p"), "%H")) +
        as.numeric(format(as.POSIXct(df$Time, format = "%I:%M %p"), "%M")) / 60
      
      df <- df[!is.na(df$Time) & is.finite(df$Time), ]
      
      ggplot(df, aes_string(x = df$Time, y = input$Y, colour = input$Splitby)) + 
        geom_point() +
        scale_x_continuous(breaks = seq(min(df$Time), max(df$Time), by = 1), labels = function(x) sprintf("%.1f", x)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        geom_smooth(method = "lm", na.rm = T) 
      
      
    } else {
      df <- df[!is.na(df[[input$X]]), ]
      ggplot(df, aes_string(x = input$X, y = input$Y, colour = input$Splitby)) + 
        geom_point() +
        labs(x = input$X, y = input$Y) + 
        geom_smooth(method = "lm", na.rm = T) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  
  output$plot_02 <- renderPlot({
    if(input$X == "Time"){
      df$Time <- as.POSIXct(df$Time, format = "%I:%M %p")
      
      df <- df[!is.na(df$Time) & is.finite(df$Time), ]
      
      ggplot(df, aes_string(x = input$X)) + 
        geom_bar() + 
        scale_x_datetime(date_breaks = "1 hour", date_labels = "%I:%M %p") + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      
    }
    else{
      df <- df[!is.na(df[[input$X]]), ]
      ggplot(df, aes_string(x = input$X)) + 
        geom_bar() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    }
  })
  
}

shinyApp(ui, server)