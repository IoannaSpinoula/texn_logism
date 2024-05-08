library(shiny)
library(rpart)
library(readr)

server <- function(input, output, session) {
  model_path <- "C:/Users/HP i7/Documents/tictactoe/inst/extdata/tic_tac_toe_model.rds"
  model <- readRDS(model_path)

  board_state <- reactive({
    s <- tolower(c(input$cell1, input$cell2, input$cell3,
                   input$cell4, input$cell5, input$cell6,
                   input$cell7, input$cell8, input$cell9))
    s[s == "blank"] <- "b"
    s
  })

  observeEvent(input$predict, {
    new_data <- as.data.frame(t(board_state()), stringsAsFactors = TRUE)
    colnames(new_data) <- c("top.left.square", "top.middle.square", "top.right.square",
                            "middle.left.square", "middle.middle.square", "middle.right.square",
                            "bottom.left.square", "bottom.middle.square", "bottom.right.square")
    new_data[] <- lapply(new_data, factor, levels = c("x", "o", "b"))

    predictions <- predict(model, new_data, type = "prob")

    # Safely access prediction results
    if (!is.null(predictions) && ncol(predictions) >= 2 && "x" %in% colnames(predictions) && "o" %in% colnames(predictions)) {
      prob_x <- round(predictions[1, "x"] * 100, 2)
      prob_o <- round(predictions[1, "o"] * 100, 2)
      prediction_text <- paste("Probability of 'X' winning:", prob_x,
                               "%\nProbability of 'O' winning:", prob_o, "%")
    } else {
      prediction_text <- "Prediction unavailable. Check model output."
    }

    output$predictionOutput <- renderText(prediction_text)
  })
}
