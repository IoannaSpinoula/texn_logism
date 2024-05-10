library(caret)

# Load data and set new column names
data <- read.csv("C:/Users/HP i7/Documents/Github/texn_logism/Data/tic-tac-toe.data", header = FALSE)
colnames(data) <- c("first.left", "first.middle", "first.right",
                    "second.left", "second.middle", "second.right",
                    "third.left", "third.middle", "third.right", 
                    "classification")

# Replace 'b' with an explicit factor level across relevant columns
levels_set <- c("x", "o", "b")  # Ensuring 'b' for blank is included
data[, 1:9] <- lapply(data[, 1:9], factor, levels = levels_set)

# Verify new factor levels
print(lapply(data[, 1:9], levels))

# Convert 'classification' into a factor with its unique levels
data$classification <- factor(data$classification, levels = unique(data$classification))

# Set seed for reproducibility
set.seed(123)

# Create a partition to split the data
index <- createDataPartition(data$classification, p = 0.8, list = FALSE)
trainData <- data[index, ]
testData <- data[-index, ]

# Check the distribution of outcomes in each dataset
print(table(trainData$classification))
print(table(testData$classification))

# Train the model using SVM with 10-fold cross-validation
model <- train(classification ~ ., data = trainData, method = "svmRadial", 
               trControl = trainControl(method = "cv", number = 10, classProbs = TRUE),
               tuneLength = 10,
               preProcess = "scale", # Preprocessing can be important for SVM
               probability = TRUE) # This is crucial for getting probability outputs
# Predict on the test data
predictions <- predict(model, testData)

# Calculate accuracy
accuracy <- sum(predictions == testData$classification) / nrow(testData)
print(paste("Accuracy:", accuracy))

# Generate a confusion matrix
confusionMatrix <- confusionMatrix(predictions, testData$classification)
print(confusionMatrix)

# choose your location that you like!!
saveRDS(model, "C:/Users/HP i7/Documents/GitHub/texn_logism/Data/tic_tac_toe_model.rds")


