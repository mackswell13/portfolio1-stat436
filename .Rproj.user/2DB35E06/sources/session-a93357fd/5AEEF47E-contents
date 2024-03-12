# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)

# Load data from CSV
data <- read.csv("dummy_data.csv", header = TRUE)

# Define UI
ui <- fluidPage(
  titlePanel("User Social Media Usage"),
  sidebarLayout(
    sidebarPanel(
      selectInput("interest", "Select Interest:",
                  choices = c("All", unique(data$interests))),
      selectInput("platform", "Select Platform:",
                  choices = c("All", unique(data$platform))),
      sliderInput("age_range", "Select Age Range:",
                  min = min(data$age),
                  max = max(data$age),
                  value = c(min(data$age), max(data$age)),
                  step = 1),
      checkboxGroupInput("gender", "Select Gender(s):",
                         choices = unique(data$gender),
                         selected = unique(data$gender),
                         inline = TRUE),
      checkboxInput("showAverage", "Show Average Line", value = TRUE),
      tags$div(
        style = "padding: 10px",
        h3("Instructions:"),
        p("Use the filters on the left to explore the user interests analysis based on the dataset."),
        p("Check the 'Show Average Line' box to toggle the display of the average line on the plot."),
        p("The data below the graph should respond accordingly to the input that you give it.")
      ),
    ),
    mainPanel(
      plotOutput("interestPlot"),
      tags$h3("Averages"),
      verbatimTextOutput("averageTime"),
      tags$h3("Most Popular Platform"),
      verbatimTextOutput("mostPopularPlatform")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(if (input$interest != "All") interests == input$interest else TRUE) %>%
      filter(if (input$platform != "All") platform == input$platform else TRUE) %>%
      filter(age >= input$age_range[1] & age <= input$age_range[2]) %>%
      filter(gender %in% input$gender)
  })
  
  output$interestPlot <- renderPlot({
    p <- ggplot(filtered_data(), aes(x = age, y = time_spent, color = gender, shape = platform)) +
      geom_count() +
      labs(x = "Age", y = "Time Spent (hours)", color = "Gender", shape = "Platform", n="count") +
      theme_minimal()
    
    if (input$showAverage) {
      p <- p + stat_summary(fun.y = "mean", geom = "line", aes(group = gender), size = 1.5)
    }
    
    p
  })
  
  output$averageTime <- renderPrint({
    avg_time <- tapply(filtered_data()$time_spent, filtered_data()$gender, mean)
    avg_time
  })
  
  output$mostPopularPlatform <- renderPrint({
    most_popular_platform <- c()
    for (selected_gender in input$gender) {
      gender_data <- data %>%
        filter(if (input$interest != "All") interests == input$interest else TRUE) %>%
        filter(age >= input$age_range[1] & age <= input$age_range[2]) %>%
        filter(gender == selected_gender)
      most_popular_platform <- c(most_popular_platform, paste(selected_gender, ":", names(sort(table(gender_data$platform), decreasing = TRUE))[1]))
    }
    most_popular_platform
  })
}

# Run the application
shinyApp(ui = ui, server = server)



