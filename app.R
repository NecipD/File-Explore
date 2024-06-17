# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "CSV File Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload and Explore", tabName = "explore", icon = icon("upload"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "explore",
        fluidRow(
          box(
            title = "Upload CSV File",
            width = 4,
            fileInput("file", "Choose CSV File", accept = ".csv")
          ),
          box(
            title = "Data Preview",
            width = 12,
            DTOutput("fileContents")
          ),
          box(
            title = "Variable Distributions",
            width = 12,
            uiOutput("plots")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$fileContents <- renderDT({
    req(data())
    datatable(head(data(), 6))
  })
  
  output$plots <- renderUI({
    req(data())
    num_vars <- sapply(data(), is.numeric)
    cat_vars <- sapply(data(), function(x) is.factor(x) || is.character(x))
    
    plot_output_list <- lapply(names(data())[num_vars], function(var) {
      plotname <- paste0("plot_", var)
      output[[plotname]] <- renderPlot({
        ggplot(data(), aes_string(x = var)) +
          geom_histogram(binwidth = 30, fill = "blue", color = "black") +
          theme_minimal() +
          ggtitle(paste("Histogram of", var))
      })
      plotOutput(plotname, height = "300px")
    })
    
    plot_output_list <- c(plot_output_list, lapply(names(data())[cat_vars], function(var) {
      plotname <- paste0("plot_", var)
      output[[plotname]] <- renderPlot({
        ggplot(data(), aes_string(x = var)) +
          geom_bar(fill = "blue", color = "black") +
          theme_minimal() +
          ggtitle(paste("Bar Chart of", var))
      })
      plotOutput(plotname, height = "300px")
    }))
    
    do.call(tagList, plot_output_list)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
