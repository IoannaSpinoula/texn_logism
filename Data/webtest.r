library(shiny)
library(caret)
library(shinydashboard)

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
                fileInput("dataFile", "Upload CSV", accept = ".csv"),
                actionButton("loadData", "Load Data"),
                dataTableOutput("dataTable")
              )
      ),
      tabItem(tabName = "visualization",
              fluidPage(
                # Placeholder for visualization tools
                h3("Visualization Tools will be added here")
              )
      ),
      tabItem(tabName = "ml",
              fluidPage(
                # Display the board as a matrix of buttons or inputs
                fluidRow(
                  column(4, offset = 4,
                         div(class = "grid",
                             lapply(1:9, function(i) {
                               selectInput(inputId = paste0("cell", i),
                                           label = NULL,
                                           choices = c("Blank" = "b", "X" = "x", "O" = "o"),
                                           selected = "b",
                                           width = '80px')
                             }),
                             div(class = "btn-center",
                                 actionButton("predict", "Predict Winner", class = "btn-primary")
                             )
                         )
                  ),
                  # Output the prediction result
                  fluidRow(
                    column(4, offset = 4,
                           wellPanel(textOutput("prediction"), style = "text-align: center;")
                    )
                  )
                )
              )
      )
    )
  )
)
# Define server logic
server <- function(input, output) {
  data <- reactiveVal()
  
  observeEvent(input$loadData, {
    req(input$dataFile)
    data(read.csv(input$dataFile$datapath))
    output$dataTable <- renderDataTable({
      data()
    })
  })
  
  model <- readRDS("C:/Users/HP i7/Documents/tictactoe/tictactoe/tic_tac_toe_model.rds")
  
  observeEvent(input$predict, {
    cells <- sapply(1:9, function(i) input[[paste0("cell", i)]])
    names(cells) <- c("first.left", "first.middle", "first.right",
                      "second.left", "second.middle", "second.right",
                      "third.left", "third.middle", "third.right")
    new_data <- as.data.frame(t(cells), stringsAsFactors = TRUE)
    new_data[] <- lapply(new_data, factor, levels = c("b", "x", "o"))
    
    prediction <- predict(model, new_data)
    output$prediction <- renderText({
      paste("Predicted Winner:", ifelse(prediction == "positive", "X", "No clear winner (O or Tie)"))
    })
  })
}

shinyApp(ui, server)