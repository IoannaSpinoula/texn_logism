# Load necessary libraries
library(readr)
library(caret)
library(rpart)
library(rpart.plot)

# Define the dataset path
dataset_path <- "C:/Users/HP i7/Documents/tictactoe/tic-tac-toe.data"
tic_tac_toe <- read_csv(dataset_path, col_names = c(
  "top.left.square", "top.middle.square", "top.right.square",
  "middle.left.square", "middle.middle.square", "middle.right.square",
  "bottom.left.square", "bottom.middle.square", "bottom.right.square",
  "class"
))

# Display the first few rows of the data to verify it's read correctly
print(head(tic_tac_toe))

# Set factor levels explicitly
levels_set <- c("x", "o", "b")  # 'b' for Blank
class_levels <- c("positive", "negative")  # Levels for the outcome

# Convert all columns to factors with the correct levels
tic_tac_toe[] <- lapply(names(tic_tac_toe), function(col_name) {
  if (col_name == "class") {
    factor(tic_tac_toe[[col_name]], levels = class_levels)
  } else {
    factor(tic_tac_toe[[col_name]], levels = levels_set)
  }
})
# Check the structure of the dataframe to ensure factors are correctly assigned
str(tic_tac_toe)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(tic_tac_toe$class, p = 0.7, list = FALSE, times = 1)
train_set <- tic_tac_toe[trainIndex, ]
test_set <- tic_tac_toe[-trainIndex, ]

# Train the model
model <- rpart(class ~ ., data = train_set, method = "class")

# Optional: Plot the decision tree
rpart.plot(model)

# Save the model to RDS file
model_path <- "C:/Users/HP i7/Documents/tictactoe/inst/extdata/tic_tac_toe_model.rds"
saveRDS(model, model_path)

# Evaluate the model's performance
predictions <- predict(model, test_set, type = "class")
confMat <- confusionMatrix(predictions, test_set$class)
print(confMat)
