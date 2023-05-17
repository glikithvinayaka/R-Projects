###################################################################
######################### Code.exe ################################
## Data: Bank Marketing Data (classification)                    ##
## Read data and analyze the features                            ##
## EDA - plots, summaries, model building, summary of model      ##
## @Author:  ( Code Exe )                                        ##
###################################################################
rm(list=ls(all=TRUE))

##################### INSTALLING & LOADING PACKAGES ########################### 
install.packages('tidyverse')
install.packages('GGally')
install.packages('ggplot2')
install.packages('vtreat')
install.packages('MASS')
install.packages('gridExtra')
install.packages('plotly')
install.packages('dplyr')
install.packages('dplyr')
install.packages('corrgram')
install.packages('corrplot')
install.packages('knitr')
install.packages('kableExtra')
install.packages('ROSE')
install.packages('reshape2')
install.packages('caret')
install.packages('caTools')
install.packages('ROCR')
install.packages('pROC')
install.packages('rpart')
install.packages('xgboost')

### Loading required packages
library(tidyverse)
library(GGally)
library(ggplot2)
library(vtreat)
library(MASS)
library(gridExtra)
library(plotly)
library(dplyr)
library(corrgram)
library(corrplot)
library(knitr)
library(kableExtra)
library(ROSE)
library(reshape2)
require(caret)
library(caTools)
library(ROCR)
library(pROC)
library(rpart)
library(xgboost)


########################### DATA LOADING  ###########################

getwd()
# set('Path_goes_here')

## Reading the dataset
bank_data<-read.csv("Bank Marketing dataset.csv")

########################### DATA EXPLORATION ###########################
## head of dataset
head(bank_data)

# refer to the meta data description
bank_data <- subset(bank_data, select = -duration) 
#We delete duration because duration is highly correlated to target variable
#basically if we know duration, we know the target variable, so to have a realistic mdoel

## string type of data
str(bank_data)

## missing data
colSums(is.na(bank_data)) %>% show()


############################ DATA PRE-PROCESSING ###########################
names(bank_data)

sum(is.na(bank_data$euribor3m))

# treating missing values in variable - euribor3m
bank_data$euribor3m[is.na(bank_data$euribor3m)]<-mean(bank_data$euribor3m,na.rm=TRUE)
sum(is.na(bank_data$euribor3m))

# treating missing values in variable - day_of_week
sum(is.na(bank_data$day_of_week))
bank_data$day_of_week[is.na(bank_data$day_of_week)]<-mode(bank_data$day_of_week)

#Checking missing values
sum(is.na(bank_data$day_of_week))


############################ EXPLORATORY DATA ANALYSIS #########################
#This data has lot of class imbalance, lot of No compared to Yes
## Dimension of dataset
dim(bank_data)

# checking % of target variable
table(bank_data$y)/nrow(bank_data)*100
table
## summary of all columns
summary(bank_data)

bp <- barplot(table(bank_data$y), 
              beside=TRUE, 
              ylim=c(0, max(table(bank_data$y)) + 3452), 
              main="Term Deposit(yes/no) Distribution", 
              col = c("#eb8060", "#b9e38d"), border=0)
text(bp, table(bank_data$y) + 1200, table(bank_data$y), font=2, col="black")

head(bank_data)

## Density plot for age column
# Create a histogram
hist(bank_data$age, 
     freq = TRUE, 
     xlab = "Age", 
     main = "Distribution of Age", 
     col = 'royal blue')

#age is continuous and marital is categorical, hence compare how the EDA is done for the two variables
#also since age is continuous, you can directly use Term_deposit = y
#in case of marital being categorical we need to use Term_deposit = factor(y) as marital is also categorical
#so Marital = factor(marital)

## Distribution of Term deposit across the age
ggplot(bank_data, aes(x = age, fill = y)) + #fill=y will basically split the plotted age as per the split of categorical variable y
  geom_histogram(position = "identity", alpha = 0.4) +
  labs(title = "Age and Term Deposit") +
  theme(plot.title = element_text(hjust = 0.5))+guides(fill=guide_legend(title="Term Deposit"))

factor(bank_data$marital)
## Distribution of customer marital status by Term Deposit
mar_counts <- bank_data %>% 
  count(Marital = factor(marital), Term_Deposit = factor(y)) %>% 
  mutate(pct = prop.table(n))
mar_counts$pct<-round(mar_counts$pct,digits = 3)
ggplot(mar_counts,aes(x = reorder(Marital,-pct), y = pct, fill = Term_Deposit, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 0)) + ggtitle("Marital Status v/s Term Deposit") + ylab("% of Records") + theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title="Term Deposit"))



# Statistical test between marital status variable and Term Deposit target variable
chisq.test(bank_data$marital, bank_data$y, correct=FALSE)
#If the p-value is less or equal to 0.05 then we may reject the null hypothesis 
#that means the categorical variables are independent

## checking any relation in job and the term deposit
job_counts<-as.data.frame(table(bank_data$job, bank_data$y))
job_counts<-job_counts %>% 
  pivot_wider(names_from=Var2, values_from=Freq)
job_counts<-as.data.frame(job_counts)
names(job_counts)<-c("Job Title","Term Deposit No","Term Deposit Yes") 
job_counts$TD_No_Per<-round((job_counts$`Term Deposit No`/sum(job_counts$`Term Deposit No`))*100,2)
job_counts$TD_Yes_Per<-round((job_counts$`Term Deposit Yes`/sum(job_counts$`Term Deposit Yes`))*100,2)
job_counts


## Distribution of Job variable
library(dplyr)
JB_counts <- bank_data %>% 
  count(Job = factor(job)) %>% 
  mutate(pct = prop.table(n))
JB_counts$pct<-round(JB_counts$pct,digits = 3)
ggplot(JB_counts,aes(x = reorder(Job,-pct), y = pct, fill = Job, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90),legend.position="none") + ggtitle("Distribution of Job variable") + ylab("% of Records") + theme(plot.title = element_text(hjust = 0.5))


# Statistical test between Job variable and Term Deposit target variable
chisq.test(bank_data$job, bank_data$y, correct=FALSE)
#If the p-value is less or equal to 0.05 then we may reject the null hypothesis 
#that means the categorical variables are independent
#For two categorical variables chi square is best test for hypothesis testing
#for 2 continuous we use 2 tail t test
#Since it is more than 30 records we assume the dataset is normal

## Distribution of education variable
ed_counts <- bank_data %>% 
  count(Education = factor(education)) %>% 
  mutate(pct = prop.table(n))
ed_counts$pct<-round(ed_counts$pct,digits = 3)
ggplot(ed_counts,aes(x = reorder(Education,-pct), y = pct, fill = Education, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90),legend.position="none") + ggtitle("Distribution of Education variable") + ylab("% of Records") + theme(plot.title = element_text(hjust = 0.5))



# Statistical test between Education variable and Term Deposit target variable
chisq.test(bank_data$education, bank_data$y, correct=FALSE)
#If the p-value is less or equal to 0.05 then we may reject the null hypothesis 
#that means the categorical variables are independent

# Distribution of education variable by term deposit
edu_counts <- bank_data %>% 
  count(Education = factor(education), Term_Deposit = factor(y)) %>% 
  mutate(pct = prop.table(n))
edu_counts$pct<-round(edu_counts$pct,digits = 3)
ggplot(edu_counts,aes(x = reorder(Education,-pct), y = pct, fill = Term_Deposit, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 90),legend.position="none") + ggtitle("Education v/s Term Deposit") + ylab("% of Records") + theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title="Term Deposit"))



## Distribution of housing variable
hou_counts <- bank_data %>% 
  count(Housing = factor(housing)) %>% 
  mutate(pct = prop.table(n))
hou_counts$pct<-round(hou_counts$pct,digits = 3)
ggplot(hou_counts,aes(x = reorder(Housing,-pct), y = pct, fill = Housing, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 0),legend.position="none") + ggtitle("Distribution of Housing variable") + ylab("% of Records") + theme(plot.title = element_text(hjust = 0.5))


## checking any relation in housing and the term deposit
hou_counts1 <- bank_data %>% 
  count(Housing = factor(housing), Term_Deposit = factor(y)) %>% 
  mutate(pct = prop.table(n))
hou_counts1$pct<-round(hou_counts1$pct,digits = 3)
ggplot(hou_counts1,aes(x = reorder(Housing,-pct), y = pct, fill = Term_Deposit, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 0),legend.position="none") + ggtitle("Housing v/s Term Deposit") + ylab("% of Records") + theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title="Term Deposit"))



## Distribution of Loan variable
ln_counts <- bank_data %>% 
  count(Loan = factor(loan)) %>% 
  mutate(pct = prop.table(n))
ln_counts$pct<-round(ln_counts$pct,digits = 3)
ggplot(ln_counts,aes(x = reorder(Loan,-pct), y = pct, fill = Loan, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 0),legend.position="none") + ggtitle("Distribution of Loan variable") + ylab("% of Records") + theme(plot.title = element_text(hjust = 0.5))


## checking any relation in loan and the term deposit
loan_counts <- bank_data %>% 
  count(Loan = factor(loan), Term_Deposit = factor(y)) %>% 
  mutate(pct = prop.table(n))
loan_counts$pct<-round(loan_counts$pct,digits = 3)
ggplot(loan_counts,aes(x = reorder(Loan,-pct), y = pct, fill = Term_Deposit, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 0),legend.position="none") + ggtitle("Loan v/s Term Deposit") + ylab("% of Records") + theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title="Term Deposit"))



## checking any relation in month and the term deposit
mon_counts <- bank_data %>% 
  count(Month = factor(month), Term_Deposit = factor(y)) %>% 
  mutate(pct = prop.table(n))
mon_counts$pct<-round(mon_counts$pct,digits = 3)
ggplot(mon_counts,aes(x = reorder(Month,-pct), y = pct, fill = Term_Deposit, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) + theme(axis.title.x=element_blank(),axis.text.x = element_text(angle = 0),legend.position="none") + ggtitle("Month v/s Term Deposit") + ylab("% of Records") + theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title="Term Deposit"))



mon_cont_y_counts<-as.data.frame(table(bank_data$month, bank_data$contact, bank_data$y))
names(mon_cont_y_counts)<-c("Month","Contact","TermDepositYesNo","Freq")
ggplot(mon_cont_y_counts, aes(x = Month, y = Freq))+
  geom_bar(
    aes(fill = TermDepositYesNo), stat = "identity", color = "white",
    position = position_dodge(0.9)
  )+facet_wrap(~Contact)+guides(fill=guide_legend(title="Contact"))



bank_data$y[bank_data$y=='yes']<-1
bank_data$y[bank_data$y=='no']<-0

bank_data$y<- as.numeric(bank_data$y)
table(bank_data$y)


############################  FACTOR DATA ##################################
factor_cols <- c("job", "marital", "education", "default","housing","loan","contact","month","day_of_week","poutcome","y")
bank_data[,factor_cols] <- lapply(bank_data[,factor_cols], factor)
#bank_data[,factor_cols] <- lapply(bank_data[,factor_cols], as.numeric) #if you want data in numeric
str(bank_data)
head(bank_data)


############################  CLASS IMBALANCE TREATMENT ########################
#If imbalanced, the model will bias towards NO -> so we need to treat
#Oversampling, Undersampling, synthetic sampling


# Count the number of samples in each class
table(bank_data$y)

# Use ROSE to oversample the minority class -> Duplicate the rows to increase the number of Yes rows
bank_data<- ROSE(y ~ ., data = bank_data)$data

# Count the number of samples in each class after oversampling
table(bank_data$y)

# Plotting dependent variable distribution in data after class balance treatment
bp <- barplot(table(bank_data$y), 
              beside=TRUE, 
              ylim=c(0, max(table(bank_data$y)) + 3452), 
              main="Term Deposit(yes/no) Distribution", 
              col = c("#eb8060", "#b9e38d"), 
              border=0)
text(bp, table(bank_data$y) + 1200, table(bank_data$y), font=2, col="black")


# Correlation matrix
corr_data<-round(cor(bank_data[sapply(bank_data, is.numeric)]),2)
corr_data


# plotting corr matrix
melted_corr_data <- melt(corr_data)
ggplot(data = melted_corr_data, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank(),
        panel.background = element_blank())
#Keep negative correlated variables
#Positive correlated variables are not useful, so throw out one of the variables between the correlated variables
#Classes will be balanced, positive correlation data is useless and negative correlation
#is extremely important as it gives us the independent variables

############################ DATA MODELING (CLASSIFICATION) ####################

# Splitting the data into train and test
index <- createDataPartition(bank_data$y, p = .70, list = FALSE)
train <- bank_data[index, ]
test <- bank_data[-index, ]
dim(train)


#Checking dimentions
dim(train)
dim(test)

# Check distrn of target var 
table(train$y)
table(test$y)


#############################  LOGISTIC REGRESSION ############################

# Training the model
logistic_model <- glm(y ~ ., family = binomial(), train)
#Since our data has only 2 target variables we use binomial log reg
#If there are more than 2 target variables we use multinomial log reg

# Checking the model
summary(logistic_model)

# Predicting in the test dataset
pred_prob <- predict(logistic_model, test, type = "response")

# Converting from probability to actual output
test$pred_class <- ifelse(pred_prob >= 0.5, "yes", "no")
test$pred_class <- as.factor(test$pred_class)

# Generating the classification table
ctab_test <- table(test$y, test$pred_class)
ctab_test

#ROC
roc <- roc(train$y, logistic_model$fitted.values)
auc(roc)

## Accuracy in Test dataset
# Accuracy = (TP + TN)/(TN + FP + FN + TP)
accuracy_test <- sum(diag(ctab_test))/sum(ctab_test)
accuracy_test

#Precision = TP/FP + TP (Precision indicates how often does your predicted TRUE values are actually TRUE.)
# Precision in Test dataset
Precision <- (ctab_test[2, 2]/sum(ctab_test[, 2]))
Precision

# Recall Or TPR = TP/(FN + TP) (Recall or TPR indicates how often does our model predicts actual TRUE from the overall TRUE events.)
# Recall in Train dataset
Recall <- (ctab_test[2, 2]/sum(ctab_test[2, ]))
Recall

# F1 score (F-Score is a harmonic mean of recall and precision. The score value lies between 0 and 1. The value of 1 represents perfect precision & recall. The value 0 represents the worst case.)
F_Score <- (2 * Precision * Recall / (Precision + Recall))
F_Score

# Formatting results 
metric_eval <- data.frame(matrix(ncol = 6, nrow = 0))
x <- c("Model_Name", "Accuracy", "Precision","Recall", "F1_score", "AUC")
colnames(metric_eval) <- x
library(caret)
lgr_val <- c("Logistic Regression",accuracy_test,Precision,Recall,F_Score,auc(roc))
metric_eval <- rbind(metric_eval,lgr_val)
names(metric_eval)<-x
## making null for predicted column created in test data
test$pred_class<-NULL


##############################  DECISION TREE #############################

library(caTools)
library(knitr)
set.seed(123)
library(rpart)
classifier <- rpart(formula = y ~ .,
                   data = train)

#rpart.plot(classifier)
# Predicting the Test set results
names(test)
str(test)
y_pred <- predict(classifier,
                 newdata = test,
                 type = 'prob')[,2]

library(pROC)
tree.roc <- roc(test$y, y_pred)
dt_auc<-tree.roc$auc[1]
## for confusion matrix evaluation
y_pred = predict(classifier,
                 newdata = test,
                 type = 'class')

# Making the Confusion Matrix
library(caret)
cm<-confusionMatrix(as.factor(y_pred), test$y,mode = "everything",positive = "1")
#I removed "yes" to "1" under positive=, becaue y_pred has numeric values
cm

# Adding results in formatted matrix
dt_val <- c("Decision Tree",
            cm$overall[1],
            cm$byClass[5],
            cm$byClass[6],
            cm$byClass[7],dt_auc)
metric_eval <- rbind(metric_eval,dt_val)
names(metric_eval)<-x


###############################    RANDOM FOREST  ##############################

library(randomForest)
library(knitr)
library(randomForest)

# Random Forest for classification
classifier_RF = randomForest(x = train[-21],
                             y = train$y,
                             ntree = 500)
classifier_RF

# Predicting the Test set results
y_pred_rf = predict(classifier_RF, newdata = test[-21])
# Plot the error vs The number of trees graph
plot(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)

# confusion matrix
cm<-confusionMatrix(y_pred_rf, test$y, mode = "everything", positive="1")
cm

# ROC
require(pROC)
rf.roc<-roc(train$y,classifier_RF$votes[,2])
plot(rf.roc)

# AUC
rf_auc<-auc(rf.roc)[1]
rf_val <- c("Random Forest",cm$overall[1],cm$byClass[5],cm$byClass[6],cm$byClass[7],rf_auc)
metric_eval <- rbind(metric_eval,rf_val)
colnames(metric_eval) <- x

# Adding results in formatted matrix
metric_eval$Accuracy<-round(as.numeric(metric_eval$Accuracy),digits = 4)
metric_eval$Precision<-round(as.numeric(metric_eval$Precision),digits = 4)
metric_eval$Recall<-round(as.numeric(metric_eval$Recall),digits = 4)
metric_eval$F1_score<-round(as.numeric(metric_eval$F1_score),digits = 4)
metric_eval$AUC<-round(as.numeric(metric_eval$AUC),digits = 4)
metric_eval

###############################    XG BOOST  ##############################

head(train[,21])

X_train = data.matrix(train[,-21])                  # independent variables for train
y_train = train[,21]                                # dependent variables for train

X_test = data.matrix(test[,-21])                    # independent variables for test
y_test = test[,21]                                   # dependent variables for test

# convert the train and test data into xgboost matrix type.
xgboost_train = xgb.DMatrix(data=X_train, label=y_train)
xgboost_test = xgb.DMatrix(data=X_test, label=y_test)

# train a model using our training data
model <- xgboost(data = xgboost_train,                    # the data   
                 max.depth=3,                          # max depth 
                 nrounds=50)                              # max number of boosting iterations

summary(model)

# Predicting 
pred_test = predict(model, xgboost_test)


pred_y = as.factor((levels(y_test))[round(pred_test)])
print(pred_y)

#Confusion matrix
conf_mat = confusionMatrix(y_test, pred_y)
print(conf_mat)

#ROC
roc_test <- roc(test$y,round(pred_test),  algorithm = 2)
plot(roc_test ) 

#AUC
Xgb_auc = auc(roc_test )

# Adding results in formatted matrix
xgb_val <- c("Xgboost",conf_mat$overall[1],conf_mat$byClass[5],conf_mat$byClass[6],conf_mat$byClass[7],Xgb_auc)
metric_eval <- rbind(metric_eval,xgb_val)
colnames(metric_eval) <- x
metric_eval$Accuracy<-round(as.numeric(metric_eval$Accuracy),digits = 4)
metric_eval$Precision<-round(as.numeric(metric_eval$Precision),digits = 4)
metric_eval$Recall<-round(as.numeric(metric_eval$Recall),digits = 4)
metric_eval$F1_score<-round(as.numeric(metric_eval$F1_score),digits = 4)
metric_eval$AUC<-round(as.numeric(metric_eval$AUC),digits = 4)
metric_eval


############################## INFERENCE TIME!! ############################## 

# Question: What model performed the best as per you?
#Random Forest was the best model with it perfomring the best to provide an accuracy of 89.26%
############################### END ########################################## 










