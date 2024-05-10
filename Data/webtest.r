library(shiny)
library(shinydashboard)  # For dashboard features
library(readxl)          # For reading Excel files
library(DT)              # For DataTables
library(data.table)      # Efficient data handling

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-line")),
      menuItem("Machine Learning", tabName = "ml", icon = icon("robot"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_upload",
              fluidPage(
                fileInput("dataFile", "Upload data", accept = c(".csv", ".xlsx")),
                actionButton("loadData", "Load Data"),
                DTOutput("dataTable")
              )),
      tabItem(tabName = "visualization",
              fluidPage(
                h3("Visualization Tools will be added here"),
                plotOutput("plotOutput")  # Placeholder for plot outputs
              )),
      tabItem(tabName = "ml",
              fluidPage(
                h3("Tic-Tac-Toe Prediction"),
                actionButton("runModel", "Run Model"),
                tableOutput("modelResults")  # Placeholder for model results
              ))
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive value to store the data
  data <- reactiveVal()
  
  # Observe file upload and read data
  observeEvent(input$dataFile, {
    req(input$dataFile)  # Ensure the file is uploaded
    
    # Determine the file extension and read data accordingly
    ext <- tools::file_ext(input$dataFile$datapath)
    switch(ext,
           csv = data(read.csv(input$dataFile$datapath)),
           xlsx = data(as.data.frame(read_excel(input$dataFile$datapath))),
           stop("Unsupported file type")
    )
  })
  
  # Render the data table in the UI using DT::renderDataTable
  output$dataTable <- DT::renderDataTable({
    req(data())  # Render only when data is available
    DT::datatable(data(), options = list(pageLength = 10))
  })
  
  # Placeholder for machine learning model application logic
  observeEvent(input$runModel, {
    req(data())  # Ensure data is loaded
    # Example: Applying a pre-trained model
    # results <- predict(model, newdata = data())
    # output$modelResults <- renderTable(results)
  })
  
  # Placeholder for generating plots based on user input
  output$plotOutput <- renderPlot({
    req(data())  # Ensure data is loaded
    # Example: Generating a plot
    # plot(data()$Variable1, data()$Variable2)
  })
}

# Run the application 
shinyApp(ui, server)