# Setup

# common:
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(rpart)
library(rpart.plot)
#install.packages("countrycode")
library(countrycode) #used for converting country names and codes from one format to another
library(lubridate) #Date Manipulation
library(pROC)
#install.packages("ROCR")
library(ROCR)
#install.packages("mice")
library(mice)
# for ML:
library(caret)
library(randomForest)
library(xgboost)


# load data:
df <- read.csv("hotel_bookings.csv",stringsAsFactors = TRUE)
head(df)

#Understanding the data #Converts character columns to factors
df[sapply(df, is.character)] <-
  lapply(df[sapply(df, is.character)], as.factor)
str(df)
summary(df)

# Let us eliminate NA and other undefined value
sum(is.na(df))
#There are totally 4 missing value in the dataset
missfun<- function(x){ #create a function to list each column with the total number of missing values in each
  sum(is.na(x))
}
apply(df,2, missfun) #Call the function
summary(df)
#We can observe that children feature consists of 4 missing value, so let us drop the four records
df<-na.omit(df)
# The 4 records which had NA values for children were all part of the city hotel and have been dropped

# "meal" contains values "Undefined", which is equal to SC.
df$meal <- ifelse(df$meal == "Undefined", "SC", df$meal)

# Some rows contain entreis with 0 adults, 0 children and 0 babies. 
# I'm dropping these entries with no guests.
zero_guests <- which(df$adults + df$children + df$babies == 0)
zero_guests
df <- df[-zero_guests, ]

#Undefined values in market_segment and distribution_channel
df <- subset(df, market_segment!='Undefined')
df<- subset(df, distribution_channel!='Undefined')

#Remaining Data
dim(df)

###########Modelling
#PCA
prop.table(table(df$is_canceled))
#Dataset is kind of Imbalanced (Not completely imbalanced dataset)

#Let us impute the missing values
#Let us extract numeric data
n_data<- df %>% select_if(is.numeric)
n_data<- n_data[-1] #Droping Response Variable
head(n_data)
#Imputing
#Since we have missing values in children feature, so imputing missing values with effective "pmm" method
library(mice)
impute<- mice(n_data,
              m=3,
              method = "pmm",
              maxit = 10)

n_data <- complete(impute,2)
#Now there are no missing values
sum(is.na(n_data))
#Let us perform PCA to reduce dimensions
pc<- prcomp(n_data, scale. = T, center = T)
summary(pc)
# PC1, PC2, PC3 have high proportion of variance & low cumulative frequency
pca <- predict(pc, newdata = n_data)
pca_data<- as.data.frame(pca)
head(pca_data)
#Selecting PC1, PC2, PC3 (They have high proportion of variance & low cumulative frequency)
pca_data<- pca_data[, 1:3]
head(pca_data)

#Extracting Categorical data
c_data<- select_if(df,is.factor)
head(c_data)

#Combining Categorical Variables with our respose variable for feature selection
response_variable<- df[2]
final_data <- cbind(response_variable,c_data,pca_data)
head(final_data)


#Building Model Using RandomForest

#"""Since we have imbalanced kind of dataset, Ensemble methods will avoid overfitting problems

#We dont required 'reservation status date, company(94% missing values) & agent ID'
#Randomforest wont work for variable which have more than 52 Levels. since 'country'
#feature has 178 level, doing 'one hot encoding' for all values will result curse in dimensionality"""

str(final_data)
final_data<- final_data[ ,-c(11,12,15)]
head(final_data)

#One hot encoding for country variable
dmy <- dummyVars(" ~ country", data = final_data)
dummy_data <- data.frame(predict(dmy, newdata = final_data))
head(dummy_data)

#PCA for dummy variables (since it has many columns)
pc<- prcomp(dummy_data, scale. = T, center = T)
pca_dummy<- predict(pc, newdata = dummy_data)
pca_dummy_data <- as.data.frame(pca_dummy)
pca_dummy_final<- pca_dummy_data[1:2]
head(pca_dummy_final)

#Combining dataframes
new_data<- cbind(final_data, pca_dummy_final)
new_final <- new_data[-5]
str(new_final)
new_final$is_canceled<- as.factor(new_final$is_canceled)









#----------------------------------------------------------------------
#Partition the data
index <- sample(2,size = nrow(new_final), replace = T , prob=c(0.7,0.3))
train <- new_final[index == 1, ]
test <- new_final[index == 2, ]

###Random Forest on Train data
library(randomForest)
rf_model <- randomForest(is_canceled~.,
                         data = train,
                         ntree = 500)
rf_model

predict_train<- predict(rf_model,train)
confusionMatrix(predict_train, train$is_canceled)
#Dropping Country Variable
df_o_country<- new_final[1:14]
#RandomForest without Country Feature
sind<- sample(2,nrow(df_o_country),
              replace = T,
              prob = c(0.7,0.3))

train_data2<- new_final[sind == 1,]
test_data2<- new_final[sind == 2,]

rf_model2 <- randomForest(is_canceled~.,
                          data = train_data2,
                          ntree = 500)

predict_test<- predict(rf_model,test)
confusionMatrix(predict_test, test$is_canceled,positive = "1")

###Random Forest on test data
rf_model <- randomForest(is_canceled~.,
                         data = test,
                         ntree = 500)
rf_model
predict_test<- predict(rf_model,test)
confusionMatrix(predict_test, test$is_canceled)
#Dropping Country Variable
df_o_country<- new_final[1:14]
#RandomForest without Country Feature
sind<- sample(2,nrow(df_o_country),
              replace = T,
              prob = c(0.7,0.3))

train_data2<- new_final[sind == 1,]
test_data2<- new_final[sind == 2,]

rf_model2 <- randomForest(is_canceled~.,
                          data = test_data2,
                          ntree = 500)

predict_test<- predict(rf_model,test)
confusionMatrix(predict_test, test$is_canceled,positive = "1")


#################Precision, Recall and Cross Validation
library(caret)

# Define the model
model <- train(is_canceled ~ ., data = train, method = "glm", trControl = trainControl(method = "cv", number = 10))

# Make predictions on the test set
pred <- predict(model, newdata = test)

# Compute precision, recall, and F1-score
conf_matrix <- confusionMatrix(data = pred, reference = test$is_canceled);conf_matrix
precision <- conf_matrix$byClass[1];precision
recall <- conf_matrix$byClass[2];recall
f1_score <- 2 * precision * recall / (precision + recall);f1_score

# Perform cross-validation
cv_results <- train(is_canceled ~ ., data = new_final, method = "glm", trControl = trainControl(method = "cv", number = 10))
cv_results































##################Logistic regression
#Logistic Regression
#Partition the data
index <- sample(2,size = nrow(df), replace = T , prob=c(0.7,0.3))
train <- df[index == 1, ]
test <- df[index == 2, ]
set.seed(1234)
glm.fit <- glm(is_canceled ~ ., 
               data = train , family = "binomial")
summary(glm.fit)

#Training data
#Prediction on Training Data

# Make sure that the levels of market_segment in the train data frame match those in the original data set
train$market_segment <- factor(train$market_segment, levels = levels(df$market_segment))

# Predict using the updated train data frame
train_pred <- predict(glm.fit, train)
library(ROCR)
pred <- prediction(train_pred,train$is_canceled)
perform <- performance(pred,"acc")
max <- which.max(slot(perform,"y.values")[[1]])
prob <- slot(perform,"x.values")[[1]][max]
prob

#Accuracy of Training Data
train_pred1 <- ifelse(train_pred >  prob, 1,0)
mean(train$is_canceled == train_pred1)

#Confusion Matrix of Training data
tble <- table(Actual = train$is_canceled,Predicted = train_pred1 );tble
library(knitr)

#Classification Table of Training Data
TN <- tble[1,1]
FN <- tble[2,1]
FP <- tble[1,2]
TP <- tble[2,2]
N <- sum(tble[1,])
P <- sum(tble[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)

#Missclassification Error on training data
1 - sum(diag(tble))/sum(tble)

#AUC & ROC on training data
#ROC Train

#Predictions for logistic regression
#Predictions for Logistic Regression
train$logit_pred_prob<-predict(glm.fit,train,type="response")
train$logit_pred_class<-ifelse(train$logit_pred_prob>0.5,"1","0") 

logit_roc<-roc(train$is_canceled,train$logit_pred_prob,auc=TRUE)
plot(logit_roc,print.auc=TRUE,print.auc.y=.4, col="green")

#AUC for training dataset
auc <- performance(pred,"auc")
auc <- unlist(slot(auc,"y.values"))
auc

#Testing data
#Prediction on Testing Data

# Predict using the updated test data frame
test_pred <- predict(glm.fit, test, type = 'response')

pred1 <- prediction(test_pred,test$is_canceled)
perform1 <- performance(pred1,"acc")
max <- which.max(slot(perform1,"y.values")[[1]])
prob <- slot(perform,"x.values")[[1]][max]
prob

#Accuracy of testing Data
test_pred1 <- ifelse(test_pred >  prob, 1,0)
mean(test$is_canceled == test_pred1)

#Confusion Matrix of testing data
tble1 <- table(Actual = test$is_canceled,Predicted = test_pred1 );tble1
library(knitr)

#Classification Table of testing Data
TN <- tble1[1,1]
FN <- tble1[2,1]
FP <- tble1[1,2]
TP <- tble1[2,2]
N <- sum(tble[1,])
P <- sum(tble[2,])
Specificity <- FP/N
Sensitivity <- TP/N
df <- data.frame(Specificity,Sensitivity)
kable(df)

#Missclassification Error on testing data
1 - sum(diag(tble1))/sum(tble1)

#AUC & ROC on training data
#ROC Train

#Predictions for logistic regression
#Predictions for Logistic Regression
test$logit_pred_prob<-predict(glm.fit,test,type="response")
test$logit_pred_class<-ifelse(test$logit_pred_prob>0.5,"1","0") 

logit_roc<-roc(test$is_canceled,test$logit_pred_prob,auc=TRUE)
plot(logit_roc,print.auc=TRUE,print.auc.y=.4, col="red")

#AUC for training dataset
auc <- performance(pred1,"auc")
auc <- unlist(slot(auc,"y.values"))
auc