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
library(shinycssloaders)
library(plotly)
library(caret)

# Define the UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Data Analysis Dashboard", titleWidth = 300),
  dashboardSidebar(
    width = 300,
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
    tags$head(tags$style(HTML("
      .box { border-radius: 10px; }
      .content-wrapper { background-color: #f4f4f4; }
      .main-header .logo { font-family: 'Lucida Console', Courier, monospace; font-weight: bold; }
      .dataTables_wrapper .dataTables_scrollBody { width: 100%; }
    "))),
    tabItems(
      tabItem(tabName = "data_upload",
              fluidPage(
                box(
                  title = "Data Upload", status = "primary", solidHeader = TRUE, width = 12,
                  p("Upload your CSV or Excel file to get started with data analysis."),
                  fileInput("dataFile", "Upload Data", accept = c(".csv", ".xlsx")),
                  actionButton("loadData", "Load Data", class = "btn-primary"),
                  DTOutput("dataTable") %>% withSpinner(color="#0dc5c1")
                )
              )
      ),
      tabItem(tabName = "ml_classification",
              fluidPage(
                box(
                  title = "Classification", status = "primary", solidHeader = TRUE, width = 12,
                  p("This tab allows you to run classification algorithms on your dataset."),
                  p("Classification algorithms categorize data points into predefined classes. Select a target variable (the variable you want to predict) and an algorithm, then run the classifier."),
                  fluidRow(
                    column(6, selectInput("classVar", "Select Target Variable:", choices = NULL)),
                    column(6, numericInput("kInput", "Number of Neighbors (k) for k-NN:", value = 3, min = 1))
                  ),
                  fluidRow(
                    column(6, selectInput("classAlg", "Select Classification Algorithm:", choices = c("k-NN", "SVM", "Decision Tree"))),
                    column(6, actionButton("runClass", "Run Classifier", class = "btn-primary"))
                  ),
                  plotlyOutput("classPlot") %>% withSpinner(color="#0dc5c1"),
                  fluidRow(
                    column(width = 6, tableOutput("classResults")),
                    column(width = 6, tableOutput("confMatrix"))
                  ),
                  tableOutput("perfMetrics")
                )
              )
      ),
      tabItem(tabName = "ml_clustering",
              fluidPage(
                box(
                  title = "Clustering", status = "primary", solidHeader = TRUE, width = 12,
                  p("This tab allows you to run clustering algorithms on your dataset."),
                  p("Clustering algorithms group data points into clusters based on their similarity. Select variables for the x and y axes, a clustering algorithm, and the number of clusters, then run the clustering."),
                  fluidRow(
                    column(6, selectInput("xClust", "Select X-axis Variable:", choices = NULL)),
                    column(6, selectInput("yClust", "Select Y-axis Variable:", choices = NULL))
                  ),
                  fluidRow(
                    column(6, numericInput("numClusters", "Number of Clusters:", value = 3, min = 1)),
                    column(6, selectInput("clustAlg", "Select Clustering Algorithm:", choices = c("k-means", "Hierarchical", "Spectral")))
                  ),
                  actionButton("runClust", "Run Clustering", class = "btn-primary"),
                  plotlyOutput("clustPlot") %>% withSpinner(color="#0dc5c1"),
                  plotlyOutput("silPlot") %>% withSpinner(color="#0dc5c1")
                )
              )
      ),
      tabItem(tabName = "visualizations",
              fluidPage(
                box(
                  title = "2D Visualizations", status = "primary", solidHeader = TRUE, width = 12,
                  p("This tab allows you to visualize your data using dimensionality reduction techniques."),
                  p("Dimensionality reduction techniques like PCA and t-SNE reduce the number of variables in your data, making it easier to visualize. Select a technique and run the visualization."),
                  selectInput("visAlg", "Select Visualization Algorithm:", choices = c("PCA", "t-SNE")),
                  conditionalPanel(
                    condition = "input.visAlg == 'PCA'",
                    fluidRow(
                      column(6, selectInput("xAxis", "Select X-axis Variable:", choices = NULL)),
                      column(6, selectInput("yAxis", "Select Y-axis Variable:", choices = NULL))
                    )
                  ),
                  actionButton("runVis", "Run Visualization", class = "btn-primary"),
                  plotlyOutput("visPlot") %>% withSpinner(color="#0dc5c1")
                )
              )
      ),
      tabItem(tabName = "eda",
              fluidPage(
                box(
                  title = "Exploratory Data Analysis (EDA)", status = "primary", solidHeader = TRUE, width = 12,
                  p("This tab allows you to perform exploratory data analysis."),
                  p("EDA helps you understand the distribution and relationships in your data. Select a variable and a plot type to visualize the data."),
                  selectInput("edaVar", "Select Variable for EDA:", choices = NULL),
                  selectInput("edaPlotType", "Select Plot Type:", choices = c("Histogram", "Boxplot", "Density Plot")),
                  plotlyOutput("edaPlot") %>% withSpinner(color="#0dc5c1")
                )
              )
      ),
      tabItem(tabName = "results_comparison",
              fluidPage(
                box(
                  title = "Model Comparisons", status = "primary", solidHeader = TRUE, width = 12,
                  p("This tab provides a detailed comparison of the performance of different models. The model with the highest accuracy is recommended."),
                  tableOutput("modelCompare"),
                  plotlyOutput("accuracyPlot") %>% withSpinner(color="#0dc5c1"),
                  textOutput("comparisonText")
                )
              )
      ),
      tabItem(tabName = "information",
              fluidPage(
                box(
                  title = "About", status = "primary", solidHeader = TRUE, width = 12,
                  p("This application was developed for data analysis, providing detailed presentations of algorithm results, including performance metrics, and indicating which algorithms perform best for the analyzed data."),
                  p("Developed by: Ioanna Spinoula inf2021211 (application and report), Despina Makri inf2021128 (application and report), Panagiotis Traxanas inf2021 (application and report)")
                ),
                box(
                  title="How to use", status = "primary", solidHeader = TRUE, width = 12,
                  p(HTML("<u><b>Data Upload:</b></u>")),
                  p("To upload your data click on the 'Browse' button and select a csv file from yor computer. Then click the next button to see your data and load them on the application."),
                  p(HTML("<u><b>Machine Learning-Classification:</b></u>")),
                  p("This tab allows you to run Classification Algorithms on your data selecting three values. Firstly, select the targeted variable from the dataset's samples. 
                    Then select a Classification Algorithm from the two provided (k-NN, Decision Tree). If you choose the k-NN then you should also choose the number of neighbors 
                    you want. Once you are done with the selection click on the 'Run Classifier' button to see the diagram. "),
                  p(HTML("<u><b>Machine Learning-Clustering:</b></u>")),
                  p("In this section two Clustering Algorithms are provided, k-means and Spectral Clustering. Once you select the x and y variables for the for the axes, the algorithm and the number of clusters, 
                    you can click on the 'Run Clustering' button to see the result."),
                  p(HTML("<u><b>2D Visualizations:</b></u>")),
                  p("In this tab two Visualization Algorithms are provided, which are targeting the better understanding of the data contained in the provided file. Select either of the PCA and t-SNE, along with 
                  the values for x and y axes and then click on the 'Run Visualization' button."),
                  p(HTML("<u><b>EDA:</b></u>")),
                  p("The Explatory Data Analisis tab offers three different types of diagrams icluding Histogram, Box Plot and Density Plot. Firstly you need to select the variable you wish and then the diagram type 
                    you want to see. If you want to see a different diagram for the same variable you should select the variable again."),
                  p(HTML("<u><b>Results Comparison:</b></u>")),
                  p("In this section the results of each Classification Algorithm are presented in a table along with their acurracy and the target value. Underneath the table there is a histogram visualizing the 
                    results with different colors for each value. All you need to do is to run one or more Classification Algorithms on one or more values and then select the 'Results Comparison' tab to view the comparison. ")
                )
              )
      )
    )
  )
)

# Define the server logic
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
    output$dataTable <- renderDT({ datatable(df, options = list(scrollX = TRUE)) })
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
        cm <- confusionMatrix(factor(model), target)
        new_row <- data.frame(Model = "k-NN", Target = input$classVar, Accuracy = round(accuracy, 4))
        model_results(rbind(model_results(), new_row))
        output$classResults <- renderTable({ data.frame(Actual = target, Prediction = model) })
        output$confMatrix <- renderTable({ cm$table })
        output$perfMetrics <- renderTable({
          data.frame(
            Metric = c("Accuracy", "Sensitivity", "Specificity"),
            Value = c(round(cm$overall['Accuracy'], 4), round(cm$byClass['Sensitivity'], 4), round(cm$byClass['Specificity'], 4))
          )
        })
        output$classPlot <- renderPlotly({
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
        cm <- confusionMatrix(factor(predictions), target)
        new_row <- data.frame(Model = "Decision Tree", Target = input$classVar, Accuracy = round(accuracy, 4))
        model_results(rbind(model_results(), new_row))
        output$classResults <- renderTable({ data.frame(Actual = target, Prediction = predictions) })
        output$confMatrix <- renderTable({ cm$table })
        output$perfMetrics <- renderTable({
          data.frame(
            Metric = c("Accuracy", "Sensitivity", "Specificity"),
            Value = c(round(cm$overall['Accuracy'], 4), round(cm$byClass['Sensitivity'], 4), round(cm$byClass['Specificity'], 4))
          )
        })
        output$classPlot <- renderPlotly({
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
        cm <- confusionMatrix(factor(predictions), target)
        new_row <- data.frame(Model = "SVM", Target = input$classVar, Accuracy = round(accuracy, 4))
        model_results(rbind(model_results(), new_row))
        output$classResults <- renderTable({ data.frame(Actual = target, Prediction = predictions) })
        output$confMatrix <- renderTable({ cm$table })
        output$perfMetrics <- renderTable({
          data.frame(
            Metric = c("Accuracy", "Sensitivity", "Specificity"),
            Value = c(round(cm$overall['Accuracy'], 4), round(cm$byClass['Sensitivity'], 4), round(cm$byClass['Specificity'], 4))
          )
        })
        output$classPlot <- renderPlotly({
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
        output$clustPlot <- renderPlotly({
          ggplot(data.frame(predictors, Cluster = factor(model$cluster)), aes_string(x = x_var, y = y_var)) +
            geom_point(aes(color = Cluster)) +
            labs(title = "k-means Clustering", x = x_var, y = y_var)
        })
        output$silPlot <- renderPlotly({
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
        output$clustPlot <- renderPlotly({
          ggplot(data.frame(predictors, Cluster = factor(clusters)), aes_string(x = x_var, y = y_var)) +
            geom_point(aes(color = Cluster)) +
            labs(title = "Hierarchical Clustering", x = x_var, y = y_var)
        })
        output$silPlot <- renderPlotly({
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
        output$clustPlot <- renderPlotly({
          ggplot(data.frame(predictors, Cluster = factor(clusters)), aes_string(x = x_var, y = y_var)) +
            geom_point(aes(color = Cluster)) +
            labs(title = "Spectral Clustering", x = x_var, y = y_var)
        })
        output$silPlot <- renderPlotly({
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
        output$visPlot <- renderPlotly({
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
        output$visPlot <- renderPlotly({
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
    })
  })
  
  output$modelCompare <- renderTable({
    model_results()
  })
  
  output$accuracyPlot <- renderPlotly({
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