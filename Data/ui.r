library(shiny)

ui <- fluidPage(
  # Custom CSS to center the title and the button
  tags$head(
    tags$style(HTML("
      .main-header { text-align: center; margin-top: 20px; margin-bottom: 20px; }
      .btn-center { display: flex; justify-content: center; margin-top: 20px; }
      .grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; }
    "))
  ),
  
  # Use div for title to control the style
  div(class = "main-header",
      h1("Tic-Tac-Toe Winner Predictor", windowTitle = "Tic-Tac-Toe Game")
  ),
  
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
               })
           ),
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
