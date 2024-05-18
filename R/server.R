# Defining the server logic of the app
server <- function(input, output, session) {
  data <- reactiveVal()  # store the uploaded data
  model_results <- reactiveVal(data.frame())  # store the results of classification models
  
  # Load data when the "load Data" button is clicked
  observeEvent(input$loadData, {
    req(input$dataFile)  # Ensuring a file is uploaded
    ext <- tools::file_ext(input$dataFile$datapath)  # Getting the file extension
    df <- switch(ext,  # Read the data based on file extension
                 csv = read.csv(input$dataFile$datapath),
                 xlsx = readxl::read_excel(input$dataFile$datapath),
                 stop("Unsupported file type"))
    
    # Ensuring the dataset has at least two columns (F features + 1 label)
    if (ncol(df) < 2) {
      stop("Dataset must have at least one feature column and one label column.")
    }
    
    # Checking if the last column (F+1) is the label column
    label_col <- tail(names(df), 1)
    
    
    # Storing the data in the reactive value
    data(df)
    
    # Render the data table
    output$dataTable <- renderDT({ datatable(df, options = list(scrollX = TRUE)) })
    
    # Update the choices for various select inputs based on the column names of the uploaded data
    updateSelectInput(session, "classVar", choices = names(df))
    updateSelectInput(session, "xAxis", choices = names(df))
    updateSelectInput(session, "yAxis", choices = names(df))
    updateSelectInput(session, "xClust", choices = names(df))
    updateSelectInput(session, "yClust", choices = names(df))
    updateSelectInput(session, "edaVar", choices = names(df))
  })
  
  # Run classification algorithm when the "runClass" button is clicked
  observeEvent(input$runClass, {
    req(data())  # Ensures data is uploaded
    df <- data()  # Getting the data
    target <- as.factor(df[[input$classVar]])  # Getting the target variable
    predictors <- df[, !(names(df) %in% input$classVar), drop = FALSE]  # Getting the predictor variables
    predictors <- as.data.frame(lapply(predictors, as.numeric))  # Convertig predictors to numeric
    
    if (input$classAlg == "k-NN") {
      # k-NN algorithm
      set.seed(123)  # Set seed for reproducibility
      tryCatch({
        model <- knn(train = predictors, test = predictors, cl = target, k = input$kInput)  # Training the k-NN model
        accuracy <- sum(model == target) / length(target)  # Calculate accuracy
        cm <- confusionMatrix(factor(model), target)  # Compute confusion matrix
        new_row <- data.frame(Model = "k-NN", Target = input$classVar, Accuracy = round(accuracy, 4))  # Create a new row for results
        model_results(rbind(model_results(), new_row))  # Add the new row to model results
        output$classResults <- renderTable({ data.frame(Actual = target, Prediction = model) })  # Render classification results
        output$confMatrix <- renderTable({ cm$table })  # confusion matrix
        output$perfMetrics <- renderTable({
          data.frame(
            Metric = c("Accuracy", "Sensitivity", "Specificity"),
            Value = c(round(cm$overall['Accuracy'], 4), round(cm$byClass['Sensitivity'], 4), round(cm$byClass['Specificity'], 4))
          )
        })  #  performance metrics
        output$classPlot <- renderPlotly({
          ggplot(data.frame(predictors, Actual = target, Prediction = model), aes(x = predictors[,1], y = predictors[,2])) +
            geom_point(aes(color = Prediction)) +
            labs(title = "k-NN Classification", x = "Feature 1", y = "Feature 2")
        })  #  classification plot
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("k-NN failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if (input$classAlg == "Decision Tree") {
      # Decision Tree algorithm
      set.seed(123)  # Set seed for reproducibility
      tryCatch({
        df_combined <- data.frame(predictors, target = target)  # Combining predictors and target into one data frame
        model <- rpart(target ~ ., data = df_combined, method = "class")  # Training the Decision Tree model
        predictions <- predict(model, predictors, type = "class")  # Predict using the trained model
        accuracy <- sum(predictions == target) / length(target)  # Calculate accuracy
        cm <- confusionMatrix(factor(predictions), target)  # Computing confusion matrix
        new_row <- data.frame(Model = "Decision Tree", Target = input$classVar, Accuracy = round(accuracy, 4))  # Create a new row for results
        model_results(rbind(model_results(), new_row))  # Add the new row to model results
        output$classResults <- renderTable({ data.frame(Actual = target, Prediction = predictions) })  # Render classification results
        output$confMatrix <- renderTable({ cm$table })  # Render confusion matrix
        output$perfMetrics <- renderTable({
          data.frame(
            Metric = c("Accuracy", "Sensitivity", "Specificity"),
            Value = c(round(cm$overall['Accuracy'], 4), round(cm$byClass['Sensitivity'], 4), round(cm$byClass['Specificity'], 4))
          )
        })  #  performance metrics
        output$classPlot <- renderPlotly({
          ggplot(data.frame(predictors, Actual = target, Prediction = predictions), aes(x = predictors[,1], y = predictors[,2])) +
            geom_point(aes(color = Prediction)) +
            labs(title = "Decision Tree Classification", x = "Feature 1", y = "Feature 2")
        })  #  classification plot
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Decision Tree failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    }
  })
  
  # Running clustering algorithm when the "runClust" button is clicked
  observeEvent(input$runClust, {
    req(data())  # Ensures data is uploaded
    df <- data()  # Getting the data
    x_var <- input$xClust  # Gets the variable for x-axis
    y_var <- input$yClust  # Gets the variable for y-axis
    req(x_var, y_var)  # Ensures x and y variables are selected
    predictors <- df[, c(x_var, y_var), drop = FALSE]  # Getting the predictor variables
    predictors <- as.data.frame(lapply(predictors, as.numeric))  # Convert predictors to numeric
    
    if (input$clustAlg == "k-means") {
      # k-means clustering
      set.seed(123)  # Set seed for reproducibility
      tryCatch({
        model <- kmeans(predictors, centers = input$numClusters)  # Runs k-means clustering
        output$clustPlot <- renderPlotly({
          ggplot(data.frame(predictors, Cluster = factor(model$cluster)), aes_string(x = x_var, y = y_var)) +
            geom_point(aes(color = Cluster)) +
            labs(title = "k-means Clustering", x = x_var, y = y_var)
        })  # clustering plot
        output$silPlot <- renderPlotly({
          fviz_silhouette(silhouette(model$cluster, dist(predictors)))
        })  # silhouette plot
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("k-means failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if (input$clustAlg == "Hierarchical") {
      # Hierarchical clustering
      tryCatch({
        model <- hclust(dist(predictors))  # Run hierarchical clustering
        clusters <- cutree(model, k = input$numClusters)  # cutting tree into clusters
        output$clustPlot <- renderPlotly({
          ggplot(data.frame(predictors, Cluster = factor(clusters)), aes_string(x = x_var, y = y_var)) +
            geom_point(aes(color = Cluster)) +
            labs(title = "Hierarchical Clustering", x = x_var, y = y_var)
        })  # clustering plot
        output$silPlot <- renderPlotly({
          fviz_silhouette(silhouette(clusters, dist(predictors)))
        })  # silhouette plot
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Hierarchical clustering failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if (input$clustAlg == "Spectral") {
      # Spectral clustering
      tryCatch({
        model <- specc(as.matrix(predictors), centers = input$numClusters)  # Running spectral clustering
        clusters <- model@.Data  # Getting clusters from the model
        output$clustPlot <- renderPlotly({
          ggplot(data.frame(predictors, Cluster = factor(clusters)), aes_string(x = x_var, y = y_var)) +
            geom_point(aes(color = Cluster)) +
            labs(title = "Spectral Clustering", x = x_var, y = y_var)
        })  # clustering plot
        output$silPlot <- renderPlotly({
          fviz_silhouette(silhouette(clusters, dist(predictors)))
        })  #  silhouette plot
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Spectral clustering failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    }
  })
  
  # Running visualization algorithm when the "runVis" button is clicked
  observeEvent(input$runVis, {
    req(data())  # Ensures data is uploaded
    df <- data()  # Gets the data
    predictors <- df[, sapply(df, is.numeric)]  # Getting numeric predictor variables
    
    if (input$visAlg == "PCA") {
      # PCA visualization
      req(input$xAxis, input$yAxis)  # Ensures x and y variables are selected
      tryCatch({
        pca <- prcomp(predictors, scale. = TRUE)  # Running PCA
        pca_df <- data.frame(pca$x)  # Get PCA results
        pca_df <- cbind(pca_df, df)  # Combine PCA results with original data
        output$visPlot <- renderPlotly({
          ggplot(pca_df, aes_string(x = input$xAxis, y = input$yAxis)) +
            geom_point() +
            labs(title = "PCA Visualization", x = input$xAxis, y = input$yAxis)
        })  #  PCA plot
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("PCA failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if (input$visAlg == "t-SNE") {
      # t-SNE visualization
      tryCatch({
        tsne <- Rtsne(as.matrix(predictors), dims = 2, perplexity = 30, verbose = FALSE, max_iter = 500)  # Run t-SNE
        tsne_df <- data.frame(tsne$Y)  # Get t-SNE results
        colnames(tsne_df) <- c("Dim1", "Dim2")  # Set column names for t-SNE results
        tsne_df <- cbind(tsne_df, df)  # Combines t-SNE results with original data
        output$visPlot <- renderPlotly({
          ggplot(tsne_df, aes(x = Dim1, y = Dim2)) +
            geom_point() +
            labs(title = "t-SNE Visualization", x = "Dim1", y = "Dim2")
        })  #  t-SNE plot
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("t-SNE failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    }
  })
  
  # Makes EDA plot when a variable is selected
  observeEvent(input$edaVar, {
    req(data())  # Ensures data is uploaded
    df <- data()  # Getting the data
    var <- input$edaVar  # Gets the selected variable
    plot_type <- input$edaPlotType  # Gets the selected plot type
    output$edaPlot <- renderPlotly({
      if (plot_type == "Histogram") {
        ggplot(df, aes_string(x = var)) +
          geom_histogram(binwidth = 30) +
          labs(title = paste("Histogram of", var), x = var, y = "Frequency")
      } else if (plot_type == "Boxplot") {
        ggplot(df, aes_string(y = var)) +
          geom_boxplot() +
          labs(title = paste("Boxplot of", var), y = var)
      } else if (plot_type == "Density Plot") {
        ggplot(df, aes_string(x = var)) +
          geom_density() +
          labs(title = paste("Density Plot of", var), x = var, y = "Density")
      }
    })  # Renders the appropriate plot type
  })
  
  # Render model comparison table
  output$modelCompare <- renderTable({
    model_results()
  })
  
  # Render accuracy plot for model comparison
  output$accuracyPlot <- renderPlotly({
    results <- model_results()  # Get the model results
    ggplot(results, aes(x = Model, y = Accuracy, fill = Target)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Model Accuracy Comparison", x = "Model", y = "Accuracy") +
      theme_minimal()
  })
  
  # Render text for model comparison
  output$comparisonText <- renderText({
    results <- model_results()  # Gets the model results
    if(nrow(results) > 1) {
      best_model <- results[which.max(results$Accuracy), ]  # Finds the best model
      worst_model <- results[which.min(results$Accuracy), ]  # Finds the worst model
      paste("The best performing algorithm is", best_model$Model, "with an accuracy of", 
            round(best_model$Accuracy * 100, 2), "%. The worst performing algorithm is", 
            worst_model$Model, "with an accuracy of", round(worst_model$Accuracy * 100, 2), "%.")
    } else {
      "Run more models to see the comparison."
    }
  })
}
