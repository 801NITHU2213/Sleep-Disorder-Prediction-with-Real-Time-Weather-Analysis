# -----------------------------------------------------------
# Sleep Disorder Prediction using Decision Tree (CART)
# -----------------------------------------------------------

# 1. Install required packages (only if not already installed)
required_packages <- c("tidyverse", "caret", "rpart", "rpart.plot", "reshape2")

installed <- rownames(installed.packages())
to_install <- required_packages[!(required_packages %in% installed)]

if(length(to_install) > 0){
  install.packages(to_install)
}

# 2. Load libraries
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(reshape2)

# -----------------------------------------------------------
# 3. Load the dataset
# -----------------------------------------------------------

data <- read.csv("C:/Users/devar/Downloads/Sleep_health_and_lifestyle_dataset.csv")
print(head(data))

# -----------------------------------------------------------
# 4. Data Preprocessing
# -----------------------------------------------------------

# Remove rows with missing target values
data <- data[!is.na(data$Sleep.Disorder), ]

# Convert categorical columns to factors
data$Gender <- as.factor(data$Gender)
data$Occupation <- as.factor(data$Occupation)
data$BMI.Category <- as.factor(data$BMI.Category)
data$Sleep.Disorder <- as.factor(data$Sleep.Disorder)

# Check target class distribution
print(table(data$Sleep.Disorder))

# -----------------------------------------------------------
# 5. Train-test split (80% train, 20% test)
# -----------------------------------------------------------

set.seed(123)
trainIndex <- createDataPartition(data$Sleep.Disorder, p = 0.8, list = FALSE)

trainData <- data[trainIndex, ]
testData  <- data[-trainIndex, ]

# -----------------------------------------------------------
# 6. Train Decision Tree (CART)
# -----------------------------------------------------------

cart_model <- rpart(Sleep.Disorder ~ ., data = trainData, method = "class")

# -----------------------------------------------------------
# 7. Visualize the Decision Tree
# -----------------------------------------------------------

rpart.plot(cart_model,
           extra = 106,
           main = "Decision Tree for Sleep Disorder Classification")
# -----------------------------------------------------------
# 7.5 Fetch Real-Time Weather Data (OpenWeatherMap API)
# -----------------------------------------------------------

library(httr)
library(jsonlite)

api_key <- "YOUR_API_KEY"   # 🔹 Replace with your actual API key
city <- "Chennai"

url <- paste0(
  "https://api.openweathermap.org/data/2.5/weather?q=",
  city, "&appid=", api_key, "&units=metric"
)

response <- GET(url)
weather_data <- fromJSON(content(response, "text"))

print("Fetched Weather Data:")
print(weather_data)

# Extract features you need (temperature, humidity, pressure, wind speed)
realtime_input <- data.frame(
  Temperature = weather_data$main$temp,
  Humidity = weather_data$main$humidity,
  Pressure = weather_data$main$pressure,
  Wind.Speed = weather_data$wind$speed
)

print("Real-time weather input:")
print(realtime_input)


# -----------------------------------------------------------
# 8. Predictions on Test Data
# -----------------------------------------------------------

predictions <- predict(cart_model, newdata = testData, type = "class")

# -----------------------------------------------------------
# 9. Evaluate Model Performance
# -----------------------------------------------------------

conf_matrix <- confusionMatrix(predictions, testData$Sleep.Disorder)
print(conf_matrix)

# -----------------------------------------------------------
# 10. Visualize Confusion Matrix
# -----------------------------------------------------------

cm <- table(Predicted = predictions, Actual = testData$Sleep.Disorder)
cm_melted <- melt(cm)

ggplot(data = cm_melted,
       aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 5, color = "white") +
  scale_fill_gradient(low = "skyblue", high = "steelblue") +
  theme_minimal() +
  ggtitle("Confusion Matrix: Sleep Disorder Prediction")
