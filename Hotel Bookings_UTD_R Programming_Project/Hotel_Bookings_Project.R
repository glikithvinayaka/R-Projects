# Setup

# common:
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(rpart)
library(rpart.plot)
library(FactoMineR)
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

#1) EDA
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
# To know the acutal visitor numbers, only bookings that were not canceled are included. 
rh <- df[df$hotel == "Resort Hotel" & df$is_canceled == 0, ]
ch <- df[df$hotel == "City Hotel" & df$is_canceled == 0, ]

######################.   Where do guests come from
# get number of acutal guests by country
origin <- df[df$reservation_status == "Check-Out",]
# Subset the data to include the countries which has more than 1500 reservation request
# otherwise including all the country with few or occassional request to avoid the graph
# from being clumsy
type_hotel <- origin %>% 
  group_by(country) %>% 
  filter(n() > 1500)

# Visualize the Traveller by Country.
type_hotel$county_name <- countrycode(type_hotel$country, 
                                     origin = "iso3c",
                                     destination = "country.name")

# Traveller by Country per hotel wise
ggplot(type_hotel, aes(county_name, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Booking Status by Country",
       x = "Country",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())

#People from all over the world are staying in these two hotels. Most guests are 
#from Portugal and other countries in Europe. Hence, we can say that this dataset is from a hotels
#lcated in Portugal

#How much do guests pay for a room per night?
  
#Since no currency information is given, but Portugal is part of the European Monetary Union, 
#I assume that all prices are in EUR.
# Counting adults and children as paying guests only, not babies.
# Average daily rate by Hotel Type
ggplot(type_hotel, aes(x = adr, fill = hotel, color = hotel)) + 
  geom_histogram(aes(y = ..density..), position = position_dodge(), binwidth = 20 ) +
  geom_density(alpha = 0.2) + 
  labs(title = "Average Daily rate by Hotel",
       x = "Hotel Price(in Euro)",
       y = "Count") + scale_color_brewer(palette = "Paired") + 
  theme_classic() + theme(legend.position = "top")
# boxplot:
ggplot(type_hotel, aes(customer_type, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Hotel Preference by Customer Type",
       x = "Customer Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())
#Does the hotel charged differently for different customer type
ggplot(type_hotel, aes(x = customer_type, y = adr, fill = hotel)) + 
  geom_boxplot(position = position_dodge()) + 
  labs(title = "Price Charged by Hotel Type",
       subtitle = "for Customer Type",
       x = "Customer Type",
       y = "Price per night(in Euro)") + theme_classic()


#How does the price per night vary over the year?
# Organize the Month in proper order
# load necessary packages
library(dplyr)
library(ggplot2)
library(lubridate)

# subset data and calculate mean room prices by hotel and month
hotel_data <- df %>% 
  filter(is_canceled == 0) %>% 
  group_by(hotel, arrival_date_month) %>% 
  summarise(avg_price = mean(adr)) %>% 
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name))

# create line plot with standard deviation error bars
ggplot(hotel_data, aes(x = arrival_date_month, y = avg_price, color = hotel, group = hotel)) +
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

ggplot(df, aes(arrival_date_month, fill = factor(is_canceled))) +
  geom_bar() + geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    label = c("Cancelled", "Not Cancelled")
  ) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()

# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(df, aes(arrival_date_month, fill = hotel)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()


#############Which are the most busy month?

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

ggplot(df, aes(arrival_date_month, fill = factor(is_canceled))) +
  geom_bar() + geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    label = c("Cancelled", "Not Cancelled")
  ) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()

# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(df, aes(arrival_date_month, fill = hotel)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()

#The City hotel has more guests during spring and autumn, when the prices are also highest.
#In July and August there are less visitors, although prices are lower.

#Guest numbers for the Resort hotel go down slighty from June to September, which is also when the prices are highest.
#Both hotels have the fewest guests during the winter.

####################How long do people stay at the hotels?

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


#########################Bookings by Market Segment
library(plotly)

# total bookings per market segment (incl. canceled)
segments <- table(df$market_segment)

# pie plot
fig <- plot_ly(labels = names(segments), values = segments, type = "pie", 
               title = "Bookings per market segment", textinfo = "percent+label",
               rotation = -90)
fig <- fig %>% layout(template = "seaborn")
fig

#################How many bookings were canceled?
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

#Which month have the highest number of cancelations?
# Create a DateFrame with the relevant data:
res_book_per_month <- df %>%
  filter(hotel == "Resort Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Bookings = n())

res_cancel_per_month <- df %>%
  filter(hotel == "Resort Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Cancelations = sum(is_canceled))

cty_book_per_month <- df %>%
  filter(hotel == "City Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Bookings = n())

cty_cancel_per_month <- df %>%
  filter(hotel == "City Hotel") %>%
  group_by(arrival_date_month) %>%
  summarize(Cancelations = sum(is_canceled))

res_cancel_data <- data.frame(Hotel = "Resort Hotel",
                              Month = res_book_per_month$arrival_date_month,
                              Bookings = res_book_per_month$Bookings,
                              Cancelations = res_cancel_per_month$Cancelations)

cty_cancel_data <- data.frame(Hotel = "City Hotel",
                              Month = cty_book_per_month$arrival_date_month,
                              Bookings = cty_book_per_month$Bookings,
                              Cancelations = cty_cancel_per_month$Cancelations)

full_cancel_data <- rbind(res_cancel_data, cty_cancel_data)
full_cancel_data$cancel_percent <- full_cancel_data$Cancelations / full_cancel_data$Bookings * 100

# order by month:
ordered_months <- c("January", "February", "March", "April", "May", "June", 
                    "July", "August", "September", "October", "November", "December")
full_cancel_data$Month <- factor(full_cancel_data$Month, levels=ordered_months)

# show figure:
ggplot(full_cancel_data, aes(x = Month, y = cancel_percent, fill = Hotel)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Cancelations per month", x="Month", y="Cancelations [%]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#For the City hotel the relative number of cancelations is around 40 % throughout the year.
#For the Resort hotel it is highest in the summer and lowest during the winter.

# select numeric columns only
df_numeric <- df[, sapply(df, is.numeric)]

# compute correlation matrix
corr <- cor(df_numeric)

# plot heatmap
# create heatmap
heatmap(
  corr,
  Rowv = NA,
  Colv = NA,
  col = cm.colors(256),
  scale = "none",
  margins = c(5, 10),
  xlab = "Variables",
  ylab = "Variables"
)

df <- df %>% 
  mutate(stay_nights_total = stays_in_weekend_nights + stays_in_week_nights,
         stay_cost_total = adr * stay_nights_total)


