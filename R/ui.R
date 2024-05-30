library(shinydashboard)
# Defining the User Interface of the app
ui <- dashboardPage(
  skin = "blue",  # Setting the color of the dashboard
  dashboardHeader(title = "Data Analysis Dashboard", titleWidth = 300),  # Header of the dashboard with the title
  dashboardSidebar(
    width = 300,  # Setting the width of the sidebar
    sidebarMenu(  # Defining the menu items in the sidebar
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
      .box { border-radius: 10px; }                    # style of the boxes
      .content-wrapper { background-color: #f4f4f4; }  # background color of the content wrapper
      .main-header .logo { font-family: 'Lucida Console', Courier, monospace; font-weight: bold; }  # logo font
      .dataTables_wrapper .dataTables_scrollBody { width: 100%; }                                   #  width of the data table scroll body
    "))),
    tabItems(
      tabItem(tabName = "data_upload",
              fluidPage(
                box(
                  title = "Data Upload", status = "primary", solidHeader = TRUE, width = 12,
                  p("Upload your CSV or Excel file to get started with data analysis."),
                  fileInput("dataFile", "Upload Data", accept = c(".csv", ".xlsx")),  # Input for uploading data file
                  actionButton("loadData", "Load Data", class = "btn-primary"),  # Button to load the uploaded data
                  DTOutput("dataTable") %>% withSpinner(color="#0dc5c1")  #  Spinner while loading
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
                    column(6, selectInput("classVar", "Select Target Variable:", choices = NULL)),              # Selecting the target variable for classification
                    column(6, numericInput("kInput", "Number of Neighbors (k) for k-NN:", value = 3, min = 1))  # Input for number of neighbors for k-NN algorithm
                  ),
                  fluidRow(
                    column(6, selectInput("classAlg", "Select Classification Algorithm:", choices = c("k-NN", "Decision Tree"))),  # Selecting the classification algorithm
                    column(6, actionButton("runClass", "Run Classifier", class = "btn-primary"))  # Button to run the classifier
                  ),
                  plotlyOutput("classPlot") %>% withSpinner(color="#0dc5c1"),  # Display spinner while loading
                  fluidRow(
                    column(width = 6, tableOutput("classResults")),  # Display the classification results
                    column(width = 6, tableOutput("confMatrix"))  # Display the confusion matrix
                  ),
                  tableOutput("perfMetrics")  # Display the performance metrics
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
                    column(6, selectInput("xClust", "Select X-axis Variable:", choices = NULL)),  # Selecting the variable for x-axis
                    column(6, selectInput("yClust", "Select Y-axis Variable:", choices = NULL))  # Selecting the variable for y-axis
                  ),
                  fluidRow(
                    column(6, numericInput("numClusters", "Number of Clusters:", value = 3, min = 1)),  # Input  number of clusters
                    column(6, selectInput("clustAlg", "Select Clustering Algorithm:", choices = c("k-means", "Hierarchical")))  # Select the clustering algorithm
                  ),
                  actionButton("runClust", "Run Clustering", class = "btn-primary"),  # Button to run the clustering algorithm
                  plotlyOutput("clustPlot") %>% withSpinner(color="#0dc5c1"),  # Display  spinner while loading
                  plotlyOutput("silPlot") %>% withSpinner(color="#0dc5c1")  # Display spinner while loading
                )
              )
      ),
      tabItem(tabName = "visualizations",
              fluidPage(
                box(
                  title = "2D Visualizations", status = "primary", solidHeader = TRUE, width = 12,
                  p("This tab allows you to visualize your data using dimensionality reduction techniques."),
                  p("Dimensionality reduction techniques like PCA and t-SNE reduce the number of variables in your data, making it easier to visualize. Select a technique and run the visualization."),
                  selectInput("visAlg", "Select Visualization Algorithm:", choices = c("PCA", "t-SNE")),  # Selecting the visualization algorithm
                  conditionalPanel(
                    condition = "input.visAlg == 'PCA'",
                    fluidRow(
                      column(6, selectInput("xAxisPCA", "Select X-axis Variable (PCA):", choices = NULL)),
                      column(6, selectInput("yAxisPCA", "Select Y-axis Variable (PCA):", choices = NULL))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.visAlg == 't-SNE'",
                    fluidRow(
                      column(6, selectInput("xAxisTSNE", "Select X-axis Variable (t-SNE):", choices = NULL)),
                      column(6, selectInput("yAxisTSNE", "Select Y-axis Variable (t-SNE):", choices = NULL))
                    )
                  ),
                  actionButton("runVis", "Run Visualization", class = "btn-primary"),  # Button to run the visualization
                  plotlyOutput("visPlot") %>% withSpinner(color="#0dc5c1")  # Display the visualization plot with a spinner while loading
                )
              )
      ),
      tabItem(tabName = "eda",
              fluidPage(
                box(
                  title = "Exploratory Data Analysis (EDA)", status = "primary", solidHeader = TRUE, width = 12,
                  p("This tab allows you to perform exploratory data analysis."),
                  p("EDA helps you understand the distribution and relationships in your data. Select a variable and a plot type to visualize the data."),
                  selectInput("edaVar", "Select Variable for EDA:", choices = NULL),  # Select the variable for EDA
                  selectInput("edaPlotType", "Select Plot Type:", choices = c("Histogram", "Boxplot", "Density Plot")),  # Select the plot type for EDA
                  plotlyOutput("edaPlot") %>% withSpinner(color="#0dc5c1")  # Display spinner while loading
                )
              )
      ),
      tabItem(tabName = "results_comparison",
              fluidPage(
                box(
                  title = "Model Comparisons", status = "primary", solidHeader = TRUE, width = 12,
                  p("This tab provides a detailed comparison of the performance of different models. The model with the highest accuracy is recommended."),
                  tableOutput("modelCompare"),  # Display the model comparison table
                  plotlyOutput("accuracyPlot") %>% withSpinner(color="#0dc5c1"),  # Display the accuracy plot with a spinner while loading
                  textOutput("comparisonText")  # Display the comparison text
                )
              )
      ),
      tabItem(tabName = "information",
              fluidPage(
                box(
                  title = "About", status = "primary", solidHeader = TRUE, width = 12,
                  p("This application was developed for data analysis, providing detailed presentations of algorithm results, including performance metrics, and indicating which algorithms perform best for the analyzed data."),
                  p("Developed by: Ioanna Spinoula inf2021211 (application and report), Despina Makri inf2021128 (application and report), Panagiotis Traxanas inf2021227 (application and report)")
                ),
                box(
                  title="How to use", status = "primary", solidHeader = TRUE, width = 12,
                  p(HTML("<u><b>Data Upload:</b></u>")),
                  p("To upload your data click on the 'Browse' button and select a csv file from your computer. Then click the next button to see your data and load them on the application."),
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
                  p("The Exploratory Data Analysis tab offers three different types of diagrams including Histogram, Box Plot and Density Plot. Firstly you need to select the variable you wish and then the diagram type 
                    you want to see. If you want to see a different diagram for the same variable you should select the variable again."),
                  p(HTML("<u><b>Results Comparison:</b></u>")),
                  p("In this section the results of each Classification Algorithm are presented in a table along with their accuracy and the target value. Underneath the table there is a histogram visualizing the 
                    results with different colors for each value. All you need to do is to run one or more Classification Algorithms on one or more values and then select the 'Results Comparison' tab to view the comparison. ")
                )
              )
      )
    )
  )
)