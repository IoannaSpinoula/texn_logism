library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(ggplot2)
library(e1071)
library(class)
library(rpart)
library(cluster)
library(kernlab)
library(Rtsne)
library(factoextra)

ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Machine Learning - Classification", tabName = "ml_classification", icon = icon("robot")),
      menuItem("Machine Learning - Clustering", tabName = "ml_clustering", icon = icon("project-diagram")),
      menuItem("2D Visualizations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("Exploratory Data Analysis (EDA)", tabName = "eda", icon = icon("search")),
      menuItem("Results Comparison", tabName = "results_comparison", icon = icon("exchange-alt")),
      menuItem("Information", tabName = "information", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_upload",
              fluidPage(
                h3("Data Upload"),
                p("Upload your CSV or Excel file to get started with data analysis."),
                fileInput("dataFile", "Upload Data", accept = c(".csv", ".xlsx")),
                actionButton("loadData", "Load Data"),
                DTOutput("dataTable")
              )
      ),
      tabItem(tabName = "ml_classification",
              fluidPage(
                titlePanel("Classification"),
                p("This tab allows you to run classification algorithms on your dataset. Classification algorithms are used to predict a categorical label for each sample."),
                p("Algorithms:"),
                tags$ul(
                  tags$li(tags$b("k-Nearest Neighbors (k-NN):"), " A simple, instance-based learning algorithm. It classifies samples based on the majority class among the k-nearest neighbors."),
                  tags$li(tags$b("Support Vector Machine (SVM):"), " A powerful classification algorithm that finds the hyperplane that best separates the classes in the feature space."),
                  tags$li(tags$b("Decision Tree:"), " A tree-based algorithm that splits the data into branches to make decisions based on feature values.")
                ),
                selectInput("classVar", "Select Target Variable:", choices = NULL),
                numericInput("kInput", "Number of Neighbors (k) for k-NN:", value = 3, min = 1),
                selectInput("classAlg", "Select Classification Algorithm:", choices = c("k-NN", "SVM", "Decision Tree")),
                actionButton("runClass", "Run Classifier"),
                plotOutput("classPlot"),
                fluidRow(
                  column(width = 6, tableOutput("classResults")),
                  column(width = 6, tableOutput("confMatrix"))
                )
              )
      ),
      tabItem(tabName = "ml_clustering",
              fluidPage(
                titlePanel("Clustering"),
                p("This tab allows you to run clustering algorithms on your dataset. Clustering algorithms group similar samples together based on feature similarities."),
                p("Algorithms:"),
                tags$ul(
                  tags$li(tags$b("k-means:"), " A partitioning method that divides the data into k clusters by minimizing the variance within each cluster."),
                  tags$li(tags$b("Hierarchical:"), " Builds a hierarchy of clusters either from the bottom-up (agglomerative) or top-down (divisive)."),
                  tags$li(tags$b("Spectral:"), " Uses eigenvalues of a similarity matrix to perform dimensionality reduction before clustering in fewer dimensions.")
                ),
                selectInput("xClust", "Select X-axis Variable:", choices = NULL),
                selectInput("yClust", "Select Y-axis Variable:", choices = NULL),
                numericInput("numClusters", "Number of Clusters:", value = 3, min = 1),
                selectInput("clustAlg", "Select Clustering Algorithm:", choices = c("k-means", "Hierarchical", "Spectral")),
                actionButton("runClust", "Run Clustering"),
                plotOutput("clustPlot"),
                plotOutput("silPlot")
              )
      ),
      tabItem(tabName = "visualizations",
              fluidPage(
                titlePanel("2D Visualizations"),
                p("This tab allows you to visualize your data using dimensionality reduction techniques."),
                p("Algorithms:"),
                tags$ul(
                  tags$li(tags$b("Principal Component Analysis (PCA):"), " Reduces the dimensionality of the data by projecting it onto the principal components."),
                  tags$li(tags$b("t-Distributed Stochastic Neighbor Embedding (t-SNE):"), " A non-linear technique that reduces dimensionality while preserving the local structure of the data.")
                ),
                selectInput("visAlg", "Select Visualization Algorithm:", choices = c("PCA", "t-SNE")),
                conditionalPanel(
                  condition = "input.visAlg == 'PCA'",
                  selectInput("xAxis", "Select X-axis Variable:", choices = NULL),
                  selectInput("yAxis", "Select Y-axis Variable:", choices = NULL)
                ),
                actionButton("runVis", "Run Visualization"),
                plotOutput("visPlot")
              )
      ),
      tabItem(tabName = "eda",
              fluidPage(
                titlePanel("Exploratory Data Analysis (EDA)"),
                p("This tab allows you to perform exploratory data analysis. Select a variable and a type of plot to generate various EDA plots."),
                selectInput("edaVar", "Select Variable for EDA:", choices = NULL),
                selectInput("edaPlotType", "Select Plot Type:", choices = c("Histogram", "Boxplot", "Density Plot")),
                plotOutput("edaPlot")
              )
      ),
      tabItem(tabName = "results_comparison",
              fluidPage(
                h2("Model Comparisons"),
                p("This tab provides a detailed comparison of the performance of different models. The model with the highest accuracy is recommended."),
                tableOutput("modelCompare"),
                plotOutput("accuracyPlot"),
                textOutput("comparisonText")
              )
      ),
      tabItem(tabName = "information",
              fluidPage(
                h2("Application Information"),
                p("This application was developed for data analysis, providing detailed presentations of algorithm results, including performance metrics, and indicating which algorithms perform best for the analyzed data."),
                p("Developed by: Ioanna, Despina, Panagiotis")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal()
  model_results <- reactiveVal(data.frame())
  
  observeEvent(input$loadData, {
    req(input$dataFile)
    ext <- tools::file_ext(input$dataFile$datapath)
    df <- switch(ext,
                 csv = read.csv(input$dataFile$datapath),
                 xlsx = readxl::read_excel(input$dataFile$datapath),
                 stop("Unsupported file type"))
    data(df)
    output$dataTable <- renderDT({ datatable(df) })
    updateSelectInput(session, "classVar", choices = names(df))
    updateSelectInput(session, "xAxis", choices = names(df))
    updateSelectInput(session, "yAxis", choices = names(df))
    updateSelectInput(session, "xClust", choices = names(df))
    updateSelectInput(session, "yClust", choices = names(df))
    updateSelectInput(session, "edaVar", choices = names(df))
  })
  
  observeEvent(input$runClass, {
    req(data())
    df <- data()
    target <- as.factor(df[[input$classVar]])
    predictors <- df[, !(names(df) %in% input$classVar), drop = FALSE]
    predictors <- as.data.frame(lapply(predictors, as.numeric))
    
    if (input$classAlg == "k-NN") {
      set.seed(123)
      tryCatch({
        model <- knn(train = predictors, test = predictors, cl = target, k = input$kInput)
        accuracy <- sum(model == target) / length(target)
        new_row <- data.frame(Model = "k-NN", Target = input$classVar, Accuracy = round(accuracy, 4))
        model_results(rbind(model_results(), new_row))
        output$classResults <- renderTable({ data.frame(Actual = target, Prediction = model) })
        output$confMatrix <- renderTable({ table(Actual = target, Prediction = model) })
        output$classPlot <- renderPlot({
          ggplot(data.frame(predictors, Actual = target, Prediction = model), aes(x = predictors[,1], y = predictors[,2])) +
            geom_point(aes(color = Prediction)) +
            labs(title = "k-NN Classification", x = "Feature 1", y = "Feature 2")
        })
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("k-NN failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if (input$classAlg == "Decision Tree") {
      set.seed(123)
      tryCatch({
        df_combined <- data.frame(predictors, target = target)
        model <- rpart(target ~ ., data = df_combined, method = "class")
        predictions <- predict(model, predictors, type = "class")
        accuracy <- sum(predictions == target) / length(target)
        new_row <- data.frame(Model = "Decision Tree", Target = input$classVar, Accuracy = round(accuracy, 4))
        model_results(rbind(model_results(), new_row))
        output$classResults <- renderTable({ data.frame(Actual = target, Prediction = predictions) })
        output$confMatrix <- renderTable({ table(Actual = target, Prediction = predictions) })
        output$classPlot <- renderPlot({
          ggplot(data.frame(predictors, Actual = target, Prediction = predictions), aes(x = predictors[,1], y = predictors[,2])) +
            geom_point(aes(color = Prediction)) +
            labs(title = "Decision Tree Classification", x = "Feature 1", y = "Feature 2")
        })
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Decision Tree failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if (input$classAlg == "SVM") {
      set.seed(123)
      tryCatch({
        model <- svm(as.matrix(predictors), target, kernel = "radial", cost = 1, gamma = 0.1)
        predictions <- predict(model, as.matrix(predictors))
        accuracy <- sum(predictions == target) / length(target)
        new_row <- data.frame(Model = "SVM", Target = input$classVar, Accuracy = round(accuracy, 4))
        model_results(rbind(model_results(), new_row))
        output$classResults <- renderTable({ data.frame(Actual = target, Prediction = predictions) })
        output$confMatrix <- renderTable({ table(Actual = target, Prediction = predictions) })
        output$classPlot <- renderPlot({
          ggplot(data.frame(predictors, Actual = target, Prediction = predictions), aes(x = predictors[,1], y = predictors[,2])) +
            geom_point(aes(color = Prediction)) +
            labs(title = "SVM Classification", x = "Feature 1", y = "Feature 2")
        })
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("SVM failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    }
  })
  
  observeEvent(input$runClust, {
    req(data())
    df <- data()
    x_var <- input$xClust
    y_var <- input$yClust
    req(x_var, y_var)
    predictors <- df[, c(x_var, y_var), drop = FALSE]
    predictors <- as.data.frame(lapply(predictors, as.numeric))
    
    if (input$clustAlg == "k-means") {
      set.seed(123)
      tryCatch({
        model <- kmeans(predictors, centers = input$numClusters)
        output$clustPlot <- renderPlot({
          ggplot(data.frame(predictors, Cluster = factor(model$cluster)), aes_string(x = x_var, y = y_var)) +
            geom_point(aes(color = Cluster)) +
            labs(title = "k-means Clustering", x = x_var, y = y_var)
        })
        output$silPlot <- renderPlot({
          fviz_silhouette(silhouette(model$cluster, dist(predictors)))
        })
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("k-means failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if (input$clustAlg == "Hierarchical") {
      tryCatch({
        model <- hclust(dist(predictors))
        clusters <- cutree(model, k = input$numClusters)
        output$clustPlot <- renderPlot({
          ggplot(data.frame(predictors, Cluster = factor(clusters)), aes_string(x = x_var, y = y_var)) +
            geom_point(aes(color = Cluster)) +
            labs(title = "Hierarchical Clustering", x = x_var, y = y_var)
        })
        output$silPlot <- renderPlot({
          fviz_silhouette(silhouette(clusters, dist(predictors)))
        })
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Hierarchical clustering failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if (input$clustAlg == "Spectral") {
      tryCatch({
        model <- specc(as.matrix(predictors), centers = input$numClusters)
        clusters <- model@.Data
        output$clustPlot <- renderPlot({
          ggplot(data.frame(predictors, Cluster = factor(clusters)), aes_string(x = x_var, y = y_var)) +
            geom_point(aes(color = Cluster)) +
            labs(title = "Spectral Clustering", x = x_var, y = y_var)
        })
        output$silPlot <- renderPlot({
          fviz_silhouette(silhouette(clusters, dist(predictors)))
        })
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
  
  observeEvent(input$runVis, {
    req(data())
    df <- data()
    predictors <- df[, sapply(df, is.numeric)]
    
    if (input$visAlg == "PCA") {
      req(input$xAxis, input$yAxis)
      tryCatch({
        pca <- prcomp(predictors, scale. = TRUE)
        pca_df <- data.frame(pca$x)
        pca_df <- cbind(pca_df, df)
        output$visPlot <- renderPlot({
          ggplot(pca_df, aes_string(x = input$xAxis, y = input$yAxis)) +
            geom_point() +
            labs(title = "PCA Visualization", x = input$xAxis, y = input$yAxis)
        })
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("PCA failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    } else if (input$visAlg == "t-SNE") {
      tryCatch({
        tsne <- Rtsne(as.matrix(predictors), dims = 2, perplexity = 30, verbose = FALSE, max_iter = 500)
        tsne_df <- data.frame(tsne$Y)
        colnames(tsne_df) <- c("Dim1", "Dim2")
        tsne_df <- cbind(tsne_df, df)
        output$visPlot <- renderPlot({
          ggplot(tsne_df, aes(x = Dim1, y = Dim2)) +
            geom_point() +
            labs(title = "t-SNE Visualization", x = "Dim1", y = "Dim2")
        })
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
  
  observeEvent(input$edaVar, {
    req(data())
    df <- data()
    var <- input$edaVar
    plot_type <- input$edaPlotType
    output$edaPlot <- renderPlot({
      if (plot_type == "Histogram") {
        ggplot(df, aes_string(x = var)) +
          geom_histogram(binwidth = 30) +
          labs(title = paste("Histogram of", var), x = var, y = "Frequency")
      } else if (plot_type == "Boxplot") {
        ggplot(df, aes_string(x = "", y = var)) +
          geom_boxplot() +
          labs(title = paste("Boxplot of", var), x = "", y = var)
      } else if (plot_type == "Density Plot") {
        ggplot(df, aes_string(x = var)) +
          geom_density() +
          labs(title = paste("Density Plot of", var), x = var, y = "Density")
      }
    })
  })
  
  output$modelCompare <- renderTable({
    model_results()
  })
  
  output$accuracyPlot <- renderPlot({
    results <- model_results()
    ggplot(results, aes(x = Model, y = Accuracy, fill = Target)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Model Accuracy Comparison", x = "Model", y = "Accuracy") +
      theme_minimal()
  })
  
  output$comparisonText <- renderText({
    results <- model_results()
    if(nrow(results) > 1) {
      best_model <- results[which.max(results$Accuracy), ]
      worst_model <- results[which.min(results$Accuracy), ]
      paste("The best performing algorithm is", best_model$Model, "with an accuracy of", 
            round(best_model$Accuracy * 100, 2), "%. The worst performing algorithm is", 
            worst_model$Model, "with an accuracy of", round(worst_model$Accuracy * 100, 2), "%.")
    } else {
      "Run more models to see the comparison."
    }
  })
}

shinyApp(ui, server)








