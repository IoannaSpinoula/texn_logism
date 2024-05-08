library(shiny)

# Define the user interface for the Tic-Tac-Toe app
ui <- fluidPage(
  titlePanel("Tic-Tac-Toe Predictor"),
  sidebarLayout(
    sidebarPanel(
      helpText("Input the current state of the Tic-Tac-Toe board:"),
      selectInput("cell1", "Top Left:", choices = c("b" = "Blank", "x" = "X", "o" = "O")),
      selectInput("cell2", "Top Center:", choices = c("b" = "Blank", "x" = "X", "o" = "O")),
      selectInput("cell3", "Top Right:", choices = c("b" = "Blank", "x" = "X", "o" = "O")),
      selectInput("cell4", "Middle Left:", choices = c("b" = "Blank", "x" = "X", "o" = "O")),
      selectInput("cell5", "Center:", choices = c("b" = "Blank", "x" = "X", "o" = "O")),
      selectInput("cell6", "Middle Right:", choices = c("b" = "Blank", "x" = "X", "o" = "O")),
      selectInput("cell7", "Bottom Left:", choices = c("b" = "Blank", "x" = "X", "o" = "O")),
      selectInput("cell8", "Bottom Center:", choices = c("b" = "Blank", "x" = "X", "o" = "O")),
      selectInput("cell9", "Bottom Right:", choices = c("b" = "Blank", "x" = "X", "o" = "O")),
      actionButton("predict", "Predict Winner"),
      actionButton("suggestMove", "Suggest Best Move")
    ),
    mainPanel(
      textOutput("predictionOutput"),
      textOutput("suggestionOutput")
    )
  )
)
