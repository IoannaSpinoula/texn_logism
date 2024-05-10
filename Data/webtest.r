library(shiny)
library(shinydashboard)  # For dashboard features
library(readxl)          # For reading Excel files
library(DT)              # For DataTables
library(data.table)      # Efficient data handling
library(ggplot2)
library(plotly)
library(Rtsne)
library(class)           # For k-NN

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-line")),
      menuItem("Machine Learning - Classification", tabName = "ml_classification", icon = icon("robot")),
      menuItem("Machine Learning - Clustering", tabName = "ml_clustering", icon = icon("sitemap")),
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
              )),
      tabItem(tabName = "visualization",
              fluidPage(
                titlePanel("2D Data Visualizations"),
                selectInput("selectVarX", "Select X Variable:", choices = NULL),
                selectInput("selectVarY", "Select Y Variable:", choices = NULL),
                actionButton("runPCA", "Run PCA"),
                actionButton("runTSNE", "Run t-SNE"),
                plotOutput("plotPCA"),
                plotOutput("plotTSNE"),
                plotlyOutput("plotEDA")
              )),
      tabItem(tabName = "ml_classification",
              fluidPage(
                titlePanel("Classification"),
                selectInput("classVar", "Select Target Variable:", choices = NULL),
                numericInput("kInput", "Number of Neighbors (k):", value = 3, min = 1),
                actionButton("runClass", "Run k-NN"),
                tableOutput("classResults")
              )),
      tabItem(tabName = "ml_clustering",
              fluidPage(
                titlePanel("Clustering"),
                numericInput("clusters", "Number of Clusters:", value = 3, min = 1),
                actionButton("runCluster", "Run k-means"),
                plotOutput("clusterPlot")
              )),
      tabItem(tabName = "information",
              fluidPage(
                h2("Application Information"),
                p("This application was developed for data analysis, to provide a detailed presentation of the algorithm results, including performance metrics, and indicate which algorithms perform best for the analyzed data."),
                p("Developed by: Ioanna, Despina, Panagiotis")
              ))
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value for storing data
  data <- reactiveVal()
  
  # Update choices for selectInput once data is read
  observe({
    df <- data()
    updateSelectInput(session, "selectVarX", choices = names(df))
    updateSelectInput(session, "selectVarY", choices = names(df))
    updateSelectInput(session, "classVar", choices = names(df))
  })
  
  # PCA Plot
  observeEvent(input$runPCA, {
    req(data())
    pca_result <- prcomp(data()[, c(input$selectVarX, input$selectVarY)], center = TRUE, scale. = TRUE)
    output$plotPCA <- renderPlot({
      biplot(pca_result, main = "PCA Plot")
    })
  })
  
  # t-SNE Plot
  observeEvent(input$runTSNE, {
    req(data())
    tsne_result <- Rtsne(data()[, c(input$selectVarX, input$selectVarY)], dims = 2)
    output$plotTSNE <- renderPlot({
      plot(tsne_result$Y, main = "t-SNE Plot")
    })
  })
  
  # EDA Plot (Using Plotly for interactive plots)
  output$plotEDA <- renderPlotly({
    req(data())
    p <- ggplot(data(), aes_string(x = input$selectVarX, y = input$selectVarY)) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = "Scatter Plot with Regression Line")
    ggplotly(p)
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
  
  # k-NN Classification
  observeEvent(input$runClass, {
    req(data()) # Ensure data is loaded
    df <- data()
    
    # Ensure `target` is a factor and has the correct length
    target <- factor(df[[input$classVar]], levels = unique(df[[input$classVar]]))
    
    # Prepare the dataset for classification, removing the target variable column
    trainData <- df[, !names(df) %in% input$classVar, drop = FALSE]
    
    set.seed(123)  # For reproducibility
    
    # Perform k-NN classification
    model <- knn(train = trainData, test = trainData, cl = target, k = input$kInput)
    
    # Output the results
    output$classResults <- renderTable({
      data.frame(Actual = target, Prediction = model)
    })
  })
  
  # k-Means Clustering
  observeEvent(input$runCluster, {
    req(data())
    set.seed(123)  # For reproducibility
    result <- kmeans(data(), centers = input$clusters)
    output$clusterPlot <- renderPlot({
      plot(data(), col = result$cluster)
      points(result$centers, col = 1:input$clusters, pch = 8, cex = 2)
    })
  })
}

shinyApp(ui, server)
