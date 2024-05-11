library(shiny)
library(shinydashboard)
library(readxl)
library(DT)
library(data.table)
library(ggplot2)
library(plotly)
library(Rtsne)

ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-line")),
      menuItem("Machine Learning - Classification", tabName = "ml_classification", icon = icon("robot")),
      menuItem("Machine Learning - Clustering", tabName = "ml_clustering", icon = icon("sitemap")),
      menuItem("Results Comparison", tabName = "results_comparison", icon = icon("exchange-alt")),  # Correctly labeled
      menuItem("Information", tabName = "information", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_upload",
              fluidPage(
                fileInput("dataFile", "Upload Data", accept = c(".csv", ".xlsx")),
                actionButton("loadData", "Load Data"),
                DTOutput("dataTable")
              )
      ),
      tabItem(tabName = "visualization",
              fluidPage(
                titlePanel("2D Data Visualizations"),
                selectInput("plotType", "Select Plot Type:", choices = c("PCA", "t-SNE")),
                uiOutput("selectVars"),
                actionButton("runPlot", "Run Plot"),
                plotOutput("plot")
              )
      ),
      tabItem(tabName = "ml_classification",
              fluidPage(
                titlePanel("Classification"),
                selectInput("classVar", "Select Target Variable:", choices = NULL),
                numericInput("kInput", "Number of Neighbors (k):", value = 3, min = 1),
                actionButton("runClass", "Run k-NN"),
                fluidRow(
                  column(width = 6, tableOutput("classResults")),
                  column(width = 6, tableOutput("confMatrix"))
                )
              )
      ),
      tabItem(tabName = "ml_clustering",
              fluidPage(
                titlePanel("Clustering"),
                numericInput("clusters", "Number of Clusters:", value = 3, min = 1),
                actionButton("runCluster", "Run k-means"),
                plotOutput("clusterPlot"),
                plotOutput("silPlot")
              )
      ),
      tabItem(tabName = "results_comparison",
              fluidPage(
                h2("Model Comparisons"),
                tableOutput("modelCompare")
              )
      ),
      tabItem(tabName = "information",
              fluidPage(
                h2("Application Information"),
                p("This application was developed for data analysis, to provide a detailed presentation of the algorithm results, including performance metrics, and indicate which algorithms perform best for the analyzed data."),
                p("Developed by: Ioanna, Despina, Panagiotis")
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive value for storing data
  data <- reactiveVal()
  
  # Reactive value for storing model results
  model_results <- reactiveVal(data.frame())
  
  # Reactive value for storing k-means result for use in multiple places
  kmeans_result <- reactiveVal()
  
  # Update choices for selectInput once data is read
  observe({
    df <- data()
    updateSelectInput(session, "selectVarX", choices = names(df))
    updateSelectInput(session, "selectVarY", choices = names(df))
    updateSelectInput(session, "classVar", choices = names(df))
  })
  
  # Observe file upload and read data
  observeEvent(input$dataFile, {
    req(input$dataFile)
    ext <- tools::file_ext(input$dataFile$datapath)
    df <- switch(ext,
                 csv = read.csv(input$dataFile$datapath),
                 xlsx = readxl::read_excel(input$dataFile$datapath),
                 stop("Unsupported file type"))
    data(df)  # Store the data in reactiveVal
    output$dataTable <- renderDT({ data() })  # Render the data as a DataTable
  })
  
  #drop down menu for visualization
  output$selectVars <- renderUI({
    req(data())
    tagList(
      selectInput("selectVarX", "Select X Variable:", choices = colnames(data())),
      selectInput("selectVarY", "Select Y Variable:", choices = colnames(data()))
    )
  })
  
  observeEvent(input$plotType, {
    if (input$plotType == "PCA") {
      observeEvent(input$runPlot, {
        req(data())
        pca_result <- prcomp(data()[, c(input$selectVarX, input$selectVarY)], center = TRUE, scale. = TRUE)
        output$plot <- renderPlot({
          biplot(pca_result, main = "PCA Plot", xlab = input$selectVarX, ylab = input$selectVarY)
        })
      })
    } else if (input$plotType == "t-SNE") {
      observeEvent(input$runPlot, {
        req(data())
        # Remove duplicate rows to ensure t-SNE runs without issues
        unique_data <- unique(data()[, c(input$selectVarX, input$selectVarY)])
        # Run t-SNE
        tsne_result <- Rtsne(unique_data, dims = 2, check_duplicates = FALSE)  # Additional check for safety
        output$plot <- renderPlot({
          plot(tsne_result$Y, main = "t-SNE Plot", xlab = input$selectVarX, ylab = input$selectVarY)
        })
      })
    }
  })
  # k-NN Classification
  observeEvent(input$runClass, {
    req(data()) # Ensure data is loaded
    df <- data()
    
    target <- factor(df[[input$classVar]], levels = unique(df[[input$classVar]]))
    trainData <- df[, !names(df) %in% input$classVar, drop = FALSE]
    
    set.seed(123)  # For reproducibility
    
    model <- knn(train = trainData, test = trainData, cl = target, k = input$kInput)
    knn_accuracy <- sum(diag(table(target, model))) / nrow(trainData)
    
    new_row <- data.frame(
      Model = "k-NN",
      Variables = paste(input$selectVarX, input$selectVarY, sep=", "),
      Neighbors = input$kInput,
      Accuracy = knn_accuracy
    )
    model_results(rbind(model_results(), new_row))
    
    output$classResults <- renderTable({
      data.frame(Actual = target, Prediction = model)
    })
    output$confMatrix <- renderTable({
      confusionMatrix(table(target, model))$table
    })
  })
  
  # k-Means Clustering
  observeEvent(input$runCluster, {
    req(data())  # Ensure data is loaded
    
    if (input$clusters > nrow(data())) {
      showModal(modalDialog(
        title = "Error",
        "Number of clusters cannot exceed number of observations in the data.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    tryCatch({
      set.seed(123)  # For reproducibility
      result <- kmeans(data(), centers = input$clusters)
      kmeans_result(result)  # Store the k-means result
      output$clusterPlot <- renderPlot({
        cols <- rainbow(length(unique(result$cluster)))
        plot(data(), col = cols[result$cluster])
        points(result$centers, col = 1:input$clusters, pch = 8, cex = 2)
      })
      output$silPlot <- renderPlot({
        sil <- silhouette(result$cluster, dist(data()))
        plot(sil, main = "Silhouette Plot")
      })
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Failed to compute k-means:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  # Model Comparison Table
  output$modelCompare <- renderTable({
    model_results()
  })
}

shinyApp(ui, server)
