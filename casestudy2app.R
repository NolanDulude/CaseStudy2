library(shiny)
library(ggplot2)
library(dplyr)
library(aws.s3)
library("RCurl")

casestudy2  <- read.table(textConnection(getURL("https://s3.us-east-2.amazonaws.com/msds.ds.6306.2/CaseStudy2-data.csv")), sep=",", header=TRUE)

# Sample dataset, replace this with your data
data <- casestudy2
ui <- fluidPage(
  titlePanel("Employee Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("plotType", "Choose Plot:", 
                   choices = c("Years Since Last Promotion", "Job Satisfaction", "Attrition Vs JobRole", "Stock Option Level")),
      selectInput("jobRole", "Select Job Role:", choices = c("All", unique(data$JobRole))),
      width = 3
    ),
    mainPanel(
      plotOutput("histogram"),
      plotOutput("scatterplot")
      
    )
  )
)

# Define server
server <- function(input, output) {
  output$histogram <- renderPlot({
    filteredData <- data
    
    if (input$jobRole != "All") {
      filteredData <- subset(data, JobRole == input$jobRole)
    }
    
    if (input$plotType == "Years Since Last Promotion") {
      ggplot(filteredData, aes(x = YearsSinceLastPromotion, fill = Attrition)) + 
        geom_histogram(binwidth = 1, color = "black") +
        labs(x = "Years Since Last Promotion", y = "Count", title = "Histogram of Years Since Last Promotion")
    } else if (input$plotType == "Job Satisfaction") {
      ggplot(filteredData, aes(x = JobSatisfaction, fill = Attrition)) + 
        geom_histogram(binwidth = 1, color = "black") +
        labs(x = "Job Satisfaction", y = "Count", title = "Histogram of Job Satisfaction")
    } else if (input$plotType == "Attrition Vs JobRole") {
      ggplot(data, aes(x = JobRole, fill = Attrition)) + 
        geom_bar() +
        labs(x = "Job Role", y = "Count", title = "Histogram of Attrition vs Job Roles")+  theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$plotType == "Stock Option Level") {
      ggplot(filteredData, aes(x = StockOptionLevel,  fill = interaction(Attrition, OverTime))) + 
        geom_histogram(binwidth = 1, color = "black") +
        labs(x = "Stock Option Level", y = "Count", title = "Histogram of Stock Option Level vs OverTime")
    }
  })
  
  output$scatterplot <- renderPlot({
    filteredData <- data
  
    
    if (input$jobRole != "All") {
      filteredData <- subset(data, JobRole == input$jobRole)
    }
    
    if (input$plotType == "Years Since Last Promotion") {
      ggplot(filteredData, aes(x = YearsSinceLastPromotion, y = Age, color = Attrition)) + 
        geom_point(position = "jitter") +
        labs(x = "Years Since Last Promotion", y = "Age", title = "Scatterplot: Years Since Last Promotion vs Age")
    } else if (input$plotType == "Job Satisfaction") {
      ggplot(filteredData, aes(x = JobSatisfaction, y = Age, color = Attrition)) + 
        geom_point(position = "jitter") +
        labs(x = "Job Satisfaction", y = "Age", title = "Scatterplot: Job Satisfaction vs Age")
    } else if (input$plotType == "JobRoleCount") {
      jobRoleCounts <- filteredData %>%
        group_by(JobRole) %>%
        summarise(JobRoleCount = n()) %>%
        arrange(desc(JobRoleCount))
      
      ggplot(jobRoleCounts, aes(x = JobRole, y = JobRoleCount)) + 
        geom_bar(stat = "identity", fill = "salmon") +
        labs(x = "Job Role", y = "Count", title = "Histogram of Job Roles")
    } else if (input$plotType == "StockOptionLevelVsOverTime_Scatterplot") {
      ggplot(filteredData, aes(x = StockOptionLevel, y = OverTime, color = Attrition)) +
        geom_point(position = "jitter") +
        labs(x = "Stock Option Level", y = "OverTime", title = "Scatterplot: Stock Option Level vs OverTime")
    }
  })
}
# Run the app
shinyApp(ui = ui, server = server)

