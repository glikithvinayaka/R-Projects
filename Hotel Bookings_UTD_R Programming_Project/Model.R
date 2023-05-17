# Load required packages
library(dplyr)
library(tidyr)
library(caret)
library(FactoMineR)
library(randomForest)
library(pROC)
library(ROCR)
# Read the data
hotel_data <- read.csv("hotel_bookings.csv")
hotel_data$is_canceled <- as.factor(as.character(hotel_data$is_canceled))
class(hotel_data$is_canceled)
# Remove irrelevant columns
hotel_data <- select(hotel_data, -c(agent, company, reservation_status_date,reservation_status))
prop.table(hotel_data$is_canceled)
# Replace missing values with the mode for categorical variables
hotel_data <- hotel_data %>% 
  replace_na(list(children = 0, country = "Unknown", meal = "SC"))

# Fill missing values with the median for numerical variables
hotel_data <- hotel_data %>% 
  fill(lead_time, .direction = "down") %>% 
  fill(required_car_parking_spaces, .direction = "down")

# Combine arrival year, month, and day to create a new variable called arrival_date
hotel_data <- hotel_data %>% 
  unite(arrival_date, arrival_date_year, arrival_date_month, arrival_date_day_of_month, sep = "-")

# Create a new variable called total_guests by adding the number of adults, children, and babies
hotel_data <- hotel_data %>% 
  mutate(total_guests = adults + children + babies)

# Convert categorical variables to factors
hotel_data[, c("hotel", "meal", "country", "market_segment", "distribution_channel",
               "reserved_room_type", "assigned_room_type", "deposit_type", "customer_type",
               "reservation_status")] <- lapply(hotel_data[, c("hotel", "meal", "country",
                                                               "market_segment", "distribution_channel",
                                                               "reserved_room_type", "assigned_room_type",
                                                               "deposit_type", "customer_type")],
                                                as.factor)

# Perform PCA on numeric variables
pca_data <- hotel_data[, c("lead_time", "stays_in_weekend_nights", "stays_in_week_nights",
                           "adults", "children", "babies", "previous_cancellations",
                           "previous_bookings_not_canceled", "booking_changes", "days_in_waiting_list",
                           "adr", "required_car_parking_spaces", "total_of_special_requests")]

pca_result <- PCA(pca_data, graph = FALSE)

# Merge PCA results with categorical variables
hotel_data <- bind_cols(hotel_data[, -c(3:15)], pca_result$ind$coord[, 1:2])

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(hotel_data$is_canceled, p = 0.7, list = FALSE)
train_data <- hotel_data[train_index, ]
test_data <- hotel_data[-train_index, ]

# Fit the model with bi-class statistics and cross-validation
ctrl <- trainControl(method = "cv", number = 5)
model <- train(is_canceled ~ ., data = train_data, method = "glm", family = "binomial", trControl = ctrl)

# Print the model results
print(model)

# Make predictions on test data
predictions <- predict(model, newdata = test_data)

# Create confusion matrix
confusion_matrix <- confusionMatrix(predictions, as.factor(test_data$is_canceled)); confusion_matrix

# Print confusion matrix
print(confusion_matrix$table)

# Compute precision, recall, and F1-score
precision <- confusion_matrix$byClass[1];precision
recall <- confusion_matrix$byClass[2];recall
f1_score <- 2 * precision * recall / (precision + recall);f1_score

# Perform cross-validation
cv_results <- train(is_canceled ~ ., data = test_data, method = "glm", trControl = trainControl(method = "cv", number = 10))
cv_results

#Random Forest Model
rf_model <- randomForest(is_canceled~.,
                         data = train_data,
                         ntree = 500)
rf_model

predict_train<- predict(rf_model,train_data)
confusionMatrix(predict_train, train_data$is_canceled,positive = '1')
#On test data
predict_test<- predict(rf_model,test_data)
conf_matr2<-confusionMatrix(predict_test, test_data$is_canceled,positive = '1')

# Compute precision, recall, and F1-score
precision <- conf_matr2$byClass[1];precision
recall <- conf_matr2$byClass[2];recall
f1_score <- 2 * precision * recall / (precision + recall);f1_score


#ROC & AUC for LOG Reg
# Convert predictions to binary
predictions<-as.numeric(as.character(predictions))
class(predictions)
predictions_binary <- ifelse(predictions < 0.5, 0, 1)

# Calculate ROC curve and AUC
roc_lr <- roc(test_data$is_canceled, predictions)
# Smooth ROC curve
roc_smooth <- smooth(roc_lr, method = "density")
auc_lr <- auc(roc_lr)

# Print AUC
print(paste("AUC:", auc_lr))

# Plot ROC curve
plot(roc_lr)
auc_lr

#ROC & AUC for Rf
# Calculate ROC curve and AUC
class(predict_train)
predict_train<-as.numeric(as.character(predict_train))
roc_rf <- roc(train_data$is_canceled, predict_train,auc=TRUE)
auc_rf <- auc(roc_rf)

# Print AUC
print(paste("AUC:", auc_rf))

# Plot ROC curve
plot(roc_rf,print.auc=TRUE,print.auc.y=.4, col="red")
auc_rf
