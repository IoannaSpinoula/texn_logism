# Load necessary libraries

library(shiny)           #  interactive web applications in R
library(shinydashboard)  # For creating dashboards in Shiny applications
library(readxl)          #  reading Excel files into R
library(DT)              # rendering interactive data tables in Shiny
library(ggplot2)         #  advanced graphics and visualizations
library(class)           # For implementing k-nearest neighbors (k-NN) classification
library(rpart)           # For building decision tree models
library(Rtsne)           # For performing t-distributed Stochastic Neighbor Embedding (t-SNE) for dimensionality reduction
library(shinycssloaders) # For adding CSS loaders (spinners) to Shiny outputs while they are recalculating
library(plotly)          # For creating interactive plots and visualizations
library(caret)           # For streamlining the process of training and evaluating machine learning models
library(cluster)
library(factoextra)
source("R/ui.R")
source("R/server.R")

shinyApp(ui, server)