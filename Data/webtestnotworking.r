library(shiny)
library(shinydashboard)
library(DT)  


# Load the pre-trained model
model_path <- "C:/Users/HP i7/Documents/tictactoe/tictactoe/tic_tac_toe_model.rds"
tic_tac_toe_model <- readRDS(model_path)

predict_winner <- function(game_state) {
  formatted_input <- as.data.frame(t(game_state))
  colnames(formatted_input) <- c("first.left", "first.middle", "first.right",
                                 "second.left", "second.middle", "second.right",
                                 "third.left", "third.middle", "third.right")
  # Make sure to convert strings to factors with the same levels as training
  formatted_input[] <- lapply(formatted_input, function(x) factor(x, levels = c("b", "x", "o")))
  
  if (any(is.na(formatted_input))) {
    return("Data mismatch or NA values detected")
  }
  
  # Attempt to predict and handle possible errors
  tryCatch({
    prediction <- predict(tic_tac_toe_model, formatted_input, type = "raw")
    paste("Predicted Winner:", prediction)
  }, error = function(e) {
    paste("Error in prediction:", e$message)
  })
}


ui <- dashboardPage(
  dashboardHeader(title = "TicTacToe Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-line")),
      menuItem("Machine Learning", tabName = "ml", icon = icon("robot"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        #tic-tac-toe-board .btn { 
          height: 100px; 
          width: 100px; 
          padding: 0; 
          font-size: 2em; 
          line-height: 100px; 
        }
        .btn-center { text-align: center; margin-top: 20px; }
      "))
    ),
    tabItems(
      tabItem(tabName = "data_upload",
              fluidPage(
                fileInput("dataFile", "Upload data", accept = ".data"),
                actionButton("loadData", "Load Data"),
                DTOutput("dataTable")
              )
      ),
      tabItem(tabName = "visualization",
              fluidPage(
                h3("Visualization Tools will be added here")
              )
      ),
      tabItem(tabName = "ml",
              fluidPage(
                h3("Tic-Tac-Toe Prediction"),
                div(id = "tic-tac-toe-board",
                    style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; max-width: 300px; margin: auto;",
                    lapply(1:9, function(i) {
                      actionButton(inputId = paste0("cell", i), label = "", width = "100px", height = "100px",
                                   style = "font-size: 2em; line-height: 100px; vertical-align: middle;")
                    })
                ),
                div(class = "btn-center",
                    actionButton("predict", "Predict Winner", class = "btn-primary"),
                    style = "text-align: center; margin-top: 20px;"),
                verbatimTextOutput("prediction")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  board <- reactiveVal(matrix("", nrow = 3, ncol = 3))
  
  observe({
    for (i in 1:9) {
      local({
        index <- i
        observeEvent(input[[paste0("cell", index)]], {
          current_board <- board()
          if (current_board[index] == "") {
            current_board[index] <- "X"
          } else if (current_board[index] == "O") {
            current_board[index] <- "X"
          } else {
            current_board[index] <- "O"
          }
          board(current_board)
          updateActionButton(session, paste0("cell", index), label = current_board[index])
        }, ignoreInit = TRUE)
      })
    }
  })
  
  observeEvent(input$predict, {
    game_state <- sapply(1:9, function(i) input[[paste0("cell", i)]])
    prediction <- predict_winner(game_state)
    output$prediction <- renderText({
      paste("Predicted Winner:", prediction)
    })
  })
  
  output$dataTable <- renderDT({
    req(input$dataFile)
    read.csv(input$dataFile$datapath)
  })
}

shinyApp(ui, server)
