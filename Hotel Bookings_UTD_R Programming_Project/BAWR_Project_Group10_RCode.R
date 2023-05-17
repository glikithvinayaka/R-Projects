#----------------------------------Project--------------------------------------
#################### Hotel Bookings Cancellation Prediction ####################
#---------------------BA With R - BUAN 6356 - SEC003 - 26942--------------------
#--------------------1) LIKITH VINAYAKA GIRIDHAR (LXG210034)--------------------
#--------------------2) PRAFUL VENKATESH PATIL (PVP22001)-----------------------
#-------------------------3) RAHUL KINTALI (RXK210144)--------------------------
#--------------------4)SAI ROHIT BOGGARAPU (SXB220120)--------------------------

# Setting up libraries
library(dplyr) #For data manipulation and transformation
library(ggplot2) #Data Viz
library(tidyr) #Cleaning the data
library(plotly) #Data Viz
library(rpart) 
library(rpart.plot)
library(FactoMineR) #For multivariate data analysis
library(countrycode) #used for converting country names and codes from one format to another
library(lubridate) #Date Manipulation
library(pROC)
library(ROCR)
library(mice) #Imputing missing data
library(caret)
library(randomForest) #Using RandomForest Model

# Read the data
df <- read.csv("hotel_bookings.csv",stringsAsFactors = TRUE)
head(df)
#Check the balance of the target variable
prop.table(table(df$is_canceled)) #Slightly imbalanced data
 
#Understanding the data #Converts character columns to factors
df[sapply(df, is.character)] <-
  lapply(df[sapply(df, is.character)], as.factor)
str(df)
summary(df)

# Let us eliminate NA and other undefined value
sum(is.na(df)) #There are totally 4 missing value in the dataset
#create a function to list each column with the total number of missing values in each
missfun<- function(x){ 
  sum(is.na(x))
}
apply(df,2, missfun) #Call the function
summary(df)
#We can observe that children feature consists of 4 missing value, so let us drop the four records
df<-na.omit(df)
# The 4 records which had NA values for children were all part of the city hotel and have been dropped

# "meal" contains values "Undefined", which is equal to SC.
df$meal <- ifelse(df$meal == "Undefined", "SC", df$meal)

# Some rows contain entries with 0 adults, 0 children and 0 babies. 
# I'm dropping these entries with no guests.
zero_guests <- which(df$adults + df$children + df$babies == 0)
zero_guests
df <- df[-zero_guests, ]

#Undefined values in market_segment and distribution_channel
df <- subset(df, market_segment!='Undefined')
df<- subset(df, distribution_channel!='Undefined')

#Remaining Data
dim(df)


######################### EDA to answer our questions ##########################
#We have two levels for Hotel, before moving further for Exploratory Data analysis, 
#it is a good idea to have some information about the ratio of preference for City and Resort Hotels
# Check the number of booking at respective hotels
table(df$hotel)

# Visualize the distribution
ggplot(data = df, aes(x = hotel)) +
  geom_bar(stat = "count") +
  labs(title = "Booking Request by Hotel type",
       x = "Hotel type",
       y = "No. of bookings") +
  theme_classic() + scale_color_brewer(palette = "Set2")
# Check the distribution of hotel type for cancellation
table(df$is_canceled, df$hotel)


# After cleaning, separate Resort and City hotel
# To know the actual visitor numbers, only bookings that were not canceled are included. 
rh <- df[df$hotel == "Resort Hotel" & df$is_canceled == 0, ]
ch <- df[df$hotel == "City Hotel" & df$is_canceled == 0, ]

################# Where do guests come from #################
# get number of acutal guests by country
origin <- df[df$reservation_status == "Check-Out",]
# Subset the data to include the countries which has more than 1500 reservation request
# otherwise including all the country with few or occasional request to avoid the graph
# from being clumsy
type_hotel <- origin %>% 
  group_by(country) %>% 
  filter(n() > 1500)

# Visualize the Traveler by Country.
type_hotel$county_name <- countrycode(type_hotel$country, 
                                      origin = "iso3c",
                                      destination = "country.name")

# Traveler by Country per hotel wise
ggplot(type_hotel, aes(county_name, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Booking Status by Country",
       x = "Country",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())

#People from all over the world are staying in these two hotels. Most guests are 
#from Portugal and other countries in Europe. Hence, we can say that this data set 
#is for hotels located in Portugal

################# How much do guests pay for a room per night? #################

#Since no currency information is given, but Portugal is part of the European Monetary Union, 
#I assume that all prices are in EUR.
# Counting adults and children as paying guests only, not babies.
# Average daily rate by Hotel Type
ggplot(type_hotel, aes(x = adr, fill = hotel, color = hotel)) + 
  geom_histogram(position = position_dodge(), binwidth = 20 ) +
  labs(title = "Average Daily rate by Hotel",
       x = "Hotel Price (in Euro)",
       y = "Count") + 
  scale_color_brewer(palette = "Paired") + 
  theme_classic() + 
  theme(legend.position = "top")

#For group wise hotel booking:
ggplot(type_hotel, aes(customer_type, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Hotel Preference by Customer Type",
       x = "Customer Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())
#Does the hotel charged differently for different customer type: Boxplot
ggplot(type_hotel, aes(x = customer_type, y = adr, fill = hotel)) + 
  geom_boxplot(position = position_dodge()) + 
  labs(title = "Price Charged by Hotel Type",
       subtitle = "for Customer Type",
       x = "Customer Type",
       y = "Price per night(in Euro)") + theme_classic()


################# How does the price per night vary over the year? ################# 
# Organize the Month in proper order

# subset data and calculate mean room prices by hotel and month
subdf <- df %>% 
  filter(is_canceled == 0) %>% 
  group_by(hotel, arrival_date_month) %>% 
  summarise(avg_price = mean(adr)) %>% 
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

# create line plot with standard deviation error bars
ggplot(subdf, aes(x = arrival_date_month, y = avg_price, color = hotel, group = hotel)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar", 
               width = 0.2, size = 1, color = "black") +
  labs(title = "Room price per night and person over the year",
       x = "Month",
       y = "Price [EUR]") +
  scale_color_manual(values = c("Resort Hotel" = "red", "City Hotel" = "blue")) +
  theme_classic() +
  theme(legend.position = "top")


################# Which are the most busy months? ################# 

# Organize the Month in proper order
df$arrival_date_month <-
  factor(df$arrival_date_month, levels = month.name)
# Visualize Hotel traffic on Monthly basis
ggplot(data = df, aes(x = arrival_date_month)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + labs(title = "Month Wise Booking Request",
                      x = "Month",
                      y = "Count") +
  theme_classic()

# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(df, aes(arrival_date_month, fill = hotel)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()

#Both hotels have the fewest guests during the winter.

################# How long do people stay at the hotels? ################# 

# Total Stay Duration
# Filter data where is_canceled is equal to 0
filter <- df$is_canceled == 0
data <- df[filter,]
head(data)

data$total_nights <- data$stays_in_weekend_nights + data$stays_in_week_nights
head(data)

stay <- aggregate(is_canceled ~ total_nights + hotel, data = data, FUN = length)
stay <- stay[, c("total_nights", "hotel", "is_canceled")]
stay <- rename(stay, `Number of stays` = is_canceled)

fig <- plot_ly(data = stay, x = ~total_nights, y = ~`Number of stays`, color = ~hotel, type = 'bar') %>%
  layout(barmode = 'group', title = 'Number of Stays by Total Nights and Hotel',
         xaxis = list(title = 'Total Nights',range = c(0, 20)), yaxis = list(title = 'Number of Stays'))

fig

################# How many bookings were canceled? ################# 
total_cancelations <- sum(df$is_canceled)
rh_cancelations <- sum(df$hotel == "Resort Hotel" & df$is_canceled == 1)
ch_cancelations <- sum(df$hotel == "City Hotel" & df$is_canceled == 1)

rel_cancel <- total_cancelations / nrow(df) * 100
rh_rel_cancel <- rh_cancelations / sum(df$hotel == "Resort Hotel") * 100
ch_rel_cancel <- ch_cancelations / sum(df$hotel == "City Hotel") * 100

cat(paste("Total bookings canceled: ", format(total_cancelations, big.mark=","), " (", round(rel_cancel), "%)\n", sep=""))
cat(paste("Resort hotel bookings canceled: ", format(rh_cancelations, big.mark=","), " (", round(rh_rel_cancel), "%)\n", sep=""))
cat(paste("City hotel bookings canceled: ", format(ch_cancelations, big.mark=","), " (", round(ch_rel_cancel), "%)\n", sep=""))

#Total bookings canceled: 44,199 (37 %)
#Resort hotel bookings canceled: 11,120 (28 %)
#City hotel bookings canceled: 33,079 (42 %)

############## Which month have the highest number of cancellations? #############

# Create a DateFrame with the relevant data:
resortbooking_monthly <- df %>%
  filter(hotel == "Resort Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Bookings = n())

resortcancellation_monthly <- df %>%
  filter(hotel == "Resort Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Cancelations = sum(is_canceled))

citybooking_monthly <- df %>%
  filter(hotel == "City Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Bookings = n())

citycancellation_monthly <- df %>%
  filter(hotel == "City Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Cancelations = sum(is_canceled))

resortcancellation_subdf <- data.frame(Hotel = "Resort Hotel",
                              Month = resortbooking_monthly$arrival_date_month,
                              Bookings = resortbooking_monthly$Bookings,
                              Cancelations = resortcancellation_monthly$Cancelations)

citycancellation_subdf <- data.frame(Hotel = "City Hotel",
                              Month = citybooking_monthly$arrival_date_month,
                              Bookings = citybooking_monthly$Bookings,
                              Cancelations = citycancellation_monthly$Cancelations)

cancellation_subdf <- rbind(resortcancellation_subdf, citycancellation_subdf)
cancellation_subdf$cancel_percent <- cancellation_subdf$Cancelations / cancellation_subdf$Bookings * 100

# order by month:
ordered_months <- c("January", "February", "March", "April", "May", "June", 
                    "July", "August", "September", "October", "November", "December")
cancellation_subdf$Month <- factor(cancellation_subdf$Month, levels=ordered_months)

# show figure:
ggplot(cancellation_subdf, aes(x = Month, y = cancel_percent, fill = Hotel)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Cancelations per month", x="Month", y="Cancelations [%]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#For the City hotel the relative number of cancelations is around 40 % throughout the year.
#For the Resort hotel it is highest in the summer and lowest during the winter.

################# Data Pre Processing and cleaning #################
#Correlation Heat Map
# select numeric columns only
df_numeric <- df[, sapply(df, is.numeric)]

# compute correlation matrix
corr <- cor(df_numeric)

# plot heatmap
# create heatmap with darker color scheme
heatmap(
  corr,
  Rowv = NA,
  Colv = NA,
  col = colorRampPalette(c("#e74c3c", "#2c3e50"))(256),
  scale = "none",
  margins = c(5, 10),
  xlab = "Variables",
  ylab = "Variables"
)


#Let us convert the dataframe to factors
df$is_canceled <- as.factor(as.character(df$is_canceled))
class(df$is_canceled)
# Remove irrelevant columns
df <- select(df, -c(agent, company, reservation_status_date,reservation_status))

# Replace missing values with the mode for categorical variables
df <- df %>% 
  replace_na(list(children = 0, country = "Unknown", meal = "SC"))

# Fill missing values with the median for numerical variables
df <- df %>% 
  fill(lead_time, .direction = "down") %>% 
  fill(required_car_parking_spaces, .direction = "down")


################# Feature Engineering #################
# Combine arrival year, month, and day to create a new variable called arrival_date
df$is_canceled <- as.factor(as.character(df$is_canceled))
class(df$is_canceled)
df <- df %>% 
  unite(arrival_date, arrival_date_year, arrival_date_month, arrival_date_day_of_month, sep = "-")

# Create a new variable called total_guests by adding the number of adults, children, and babies
df <- df %>% 
  mutate(total_guests = adults + children + babies)

# Convert categorical variables to factors
df[, c("hotel", "meal", "country", "market_segment", "distribution_channel",
               "reserved_room_type", "assigned_room_type", "deposit_type", "customer_type",
               "reservation_status")] <- lapply(df[, c("hotel", "meal", "country",
                                                               "market_segment", "distribution_channel",
                                                               "reserved_room_type", "assigned_room_type",
                                                               "deposit_type", "customer_type")],
                                                as.factor)

################# PCA #################
# Perform PCA on numeric variables
pca_data <- df[, c("lead_time", "stays_in_weekend_nights", "stays_in_week_nights",
                           "adults", "children", "babies", "previous_cancellations",
                           "previous_bookings_not_canceled", "booking_changes", "days_in_waiting_list",
                           "adr", "required_car_parking_spaces", "total_of_special_requests")]

pca_result <- PCA(pca_data, graph = FALSE)

# Merge PCA results with categorical variables
df <- bind_cols(df[, -c(3:15)], pca_result$ind$coord[, 1:3])

################# Building the Model #################
#######################Logistic Regression Model #######################
# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(df$is_canceled, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Fit the model
ctrl <- trainControl(method = "cv", number = 5)
model <- train(is_canceled ~ ., data = train_data, method = "glm", family = "binomial", trControl = ctrl)

# Print the model results
print(model)

# Make predictions on test data
predictions <- predict(model, newdata = test_data)

# Create confusion matrix
confusion_matrix <- confusionMatrix(predictions, as.factor(test_data$is_canceled),positive = "1"); confusion_matrix

# Print confusion matrix
print(confusion_matrix$table)

# Let us look at the byclass statistics for the prediction on test data
View(confusion_matrix)

#ROC & AUC for Logistic Regression Model
# Convert predictions to binary
predictions<-as.numeric(as.character(predictions))
class(predictions)
predictions_binary <- ifelse(predictions < 0.5, 0, 1)

# Calculate ROC and AUC
roc_lr <- roc(test_data$is_canceled, predictions)
auc_lr <- auc(roc_lr)
# Print AUC
print(paste("AUC:", auc_lr))


####################### Random Forest Model #######################
rf_model <- randomForest(is_canceled~.,
                         data = train_data,
                         ntree = 500)
rf_model
#Prediction for Training Data
predict_train<- predict(rf_model,train_data)
confusionMatrix(predict_train, train_data$is_canceled,positive = '1')
#On test data
predict_test<- predict(rf_model,test_data)
conf_matr2<-confusionMatrix(predict_test, test_data$is_canceled,positive = '1');conf_matr2

# Let us look at the ByClass Statistics for thes test data
View(conf_matr2)

#ROC & AUC for Random Forest Model
# Calculate ROC curve and AUC
class(predict_test)
predict_test<-as.numeric(as.character(predict_test))
roc_rf <- roc(test_data$is_canceled, predict_test,auc=TRUE)
auc_rf <- auc(roc_rf)
# Print AUC
print(paste("AUC:", auc_rf))

#We can see that random forest is giving us better performance with better byclass
#statistics and a higher area under the curve in ROC!!