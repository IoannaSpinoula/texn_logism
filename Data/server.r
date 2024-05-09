library(shiny)
library(caret)

server <- function(input, output) {
  # Load the pre-trained model... use your path!!!!
  model <- readRDS("C:/Users/HP i7/Documents/tictactoe/tictactoe/tic_tac_toe_model.rds")
  
  observeEvent(input$predict, {
    # Prepare the input data as a one-row data frame
    cells <- sapply(1:9, function(i) input[[paste0("cell", i)]])
    names(cells) <- c("first.left", "first.middle", "first.right",
                      "second.left", "second.middle", "second.right",
                      "third.left", "third.middle", "third.right")
    new_data <- as.data.frame(t(cells), stringsAsFactors = TRUE)
    new_data[] <- lapply(new_data, factor, levels = c("b", "x", "o"))
    
    # Predict using the model
    prediction <- predict(model, new_data)
    
    # Output the prediction
    output$prediction <- renderText({
      paste("Predicted Winner:", ifelse(prediction == "positive", "X", "No clear winner (O or Tie)"))
    })
  })
}

