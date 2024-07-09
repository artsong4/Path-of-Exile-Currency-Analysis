library(pheatmap)
library(fastDummies)
library(reshape2)
library(readxl)
library(gridExtra)
library(patchwork)
library(tidyr)
library(dplyr)
library(ggplot2)
library(arules)
library(lubridate)
library(caret)
library(outliers)
library(rpart)
library(rpart.plot)
library(naivebayes)
library(e1071)
library(randomForest)
library(heatmaply)
library(gplots)

Currency <- read_excel("C:/DS/707/Currency.xlsx")
summary(Currency)
str(Currency)
head(Currency)

# Check for missing values
sum(is.na(Currency))

Currency <- Currency %>%
  select(-Confidence, -League)

# Calculating average value for each 'Get' type
average_values <- Currency %>%
  group_by(Get) %>%
  summarise(AverageValue = mean(Value, na.rm = TRUE))

# Viewing the results
print(average_values)

# Assuming you are calculating this for the 'Value' column in your dataset
Q1 <- quantile(Currency$Value, 0.25) # 1st Quartile
Q3 <- quantile(Currency$Value, 0.75) # 3rd Quartile

# Interquartile Range
IQR <- IQR(Currency$Value)

# Larger multiplier, e.g., 2.5 or 3
multiplier <- 10
multiplier <- 40
multiplier <- 100

# New outlier bounds
lower_bound <- Q1 - multiplier * IQR
upper_bound <- Q3 + multiplier * IQR

# Flagging outliers with new bounds
average_values$IsOutlier <- with(average_values, AverageValue < lower_bound | AverageValue > upper_bound)

# Extracting outlier data
outlier_averages <- average_values[average_values$IsOutlier, ]

# Viewing the outlier average values
print(outlier_averages)

# Filter out "Mirror Shard" and "Mirror of Kalandra"
Currency <- Currency %>%
  filter(Get != "Mirror Shard" & Get != "Mirror of Kalandra" & Pay != "Mirror Shard" & Pay != "Mirror of Kalandra")



# Assuming your dataset is named 'Currency'
# Select the 'Value' column for clustering
data_for_clustering <- Currency %>%
  select(Value)

# Normalize the 'Value' column (optional)
data_for_clustering$Value <- scale(data_for_clustering$Value)

# Specify a range of possible cluster numbers (K)
possible_k_values <- 1:10

# Initialize an empty vector to store the within-cluster sum of squares (WCSS)
wcss_values <- vector()

# Calculate WCSS for each K
for (k in possible_k_values) {
  kmeans_model <- kmeans(data_for_clustering, centers = k, nstart = 25)
  wcss_values[k] <- kmeans_model$tot.withinss
}

# Plot the elbow curve
ggplot(data.frame(K = possible_k_values, WCSS = wcss_values), aes(x = K, y = WCSS)) +
  geom_line() +
  labs(title = "Elbow Method for Optimal K in K-Means Clustering",
       x = "Number of Clusters (K)",
       y = "Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal()



# Assuming your dataset is named 'Currency'
# Select the 'Value' column for clustering
data_for_clustering <- Currency %>%
  select(Value)

# Normalize the 'Value' column (optional)
data_for_clustering$Value <- scale(data_for_clustering$Value)

# Specify the number of clusters (K)
num_clusters <- 3

# Perform K-Means clustering
kmeans_model <- kmeans(data_for_clustering, centers = num_clusters, nstart = 25)

# Add cluster assignments back to the original dataset
Currency$Cluster <- as.factor(kmeans_model$cluster)

# Visualize the clusters (for example)
ggplot(Currency, aes(x = Date, y = Value, color = Cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering of Currency Values",
       x = "Date",
       y = "Value") +
  theme_minimal()

summary(Currency)

# Naive Bayes
# Assuming you already have a kmeans_model and Currency dataset
# Splitting the dataset
set.seed(123)
split <- createDataPartition(Currency$Cluster, p = 0.8, list = FALSE)
trainingSet <- Currency[split, ]
testingSet <- Currency[-split, ]

# Convert 'Date' to a factor (if not already)
trainingSet$Date <- as.factor(trainingSet$Date)
testingSet$Date <- as.factor(testingSet$Date)

# Train the Naive Bayes model
model <- naive_bayes(Cluster ~ ., data = trainingSet, laplace = 1)

# Prepare testing set for prediction (exclude the actual ValueClustered column)
testingSet_for_prediction <- testingSet
testingSet_for_prediction$Cluster <- NULL

# Predictions
predictions <- predict(model, testingSet_for_prediction, type = "class")

# Evaluating the model
confusionMatrix(predictions, testingSet$Cluster)


# Confusion Matrix
conf_matrix <- as.table(matrix(c(49, 0, 2, 9, 7626, 0, 0, 181, 614), nrow = 3, byrow = TRUE))
rownames(conf_matrix) <- c("Class 1", "Class 2", "Class 3")
colnames(conf_matrix) <- c("Class 1", "Class 2", "Class 3")


# Confusion Matrix
conf_matrix <- matrix(c(49, 0, 2, 9, 7626, 0, 0, 181, 614), nrow = 3, byrow = TRUE)

class_labels <- c("Class 1", "Class 2", "Class 3")



# Create a heatmap of the confusion matrix
heatmap.2(conf_matrix, 
          Colv = NA, Rowv = NA,
          col = colorRampPalette(c("white", "steelblue"))(100),
          main = "Confusion Matrix",
          xlab = "Predicted", ylab = "Actual",
          cellnote = conf_matrix, notecol="black", density.info="none",
          trace = "none", cexCol = 1, cexRow = 1,
          margins = c(6, 6),
          labRow = class_labels, labCol = class_labels,
          srtRow = 0, srtCol = 0)


# Random Forest
str(Currency)

# Convert 'Get' column to a factor
Currency$Get <- as.factor(Currency$Get)

# Get levels (unique values) from 'Get' column
get_levels <- levels(Currency$Get)

# Initialize an empty data frame for dummy variables
dummy_data <- data.frame(matrix(ncol = length(get_levels), nrow = nrow(Currency)))

# Name the columns of dummy_data exactly as the levels
names(dummy_data) <- get_levels

# Generate dummy variables
for (level in get_levels) {
  dummy_data[[level]] <- as.numeric(Currency$Get == level)
}

# Bind dummy variables to the original dataset
Currency_with_dummies <- cbind(Currency, dummy_data)

# Convert 'Pay' column to a factor
Currency$Pay <- as.factor(Currency$Pay)

# Get levels (unique values) from 'Pay' column
pay_levels <- levels(Currency$Pay)

# Initialize an empty data frame for dummy variables
dummy_data_pay <- data.frame(matrix(ncol = length(pay_levels), nrow = nrow(Currency)))

# Name the columns of dummy_data_pay exactly as the levels
names(dummy_data_pay) <- pay_levels

# Generate dummy variables for 'Pay'
for (level in pay_levels) {
  dummy_data_pay[[level]] <- as.numeric(Currency$Pay == level)
}

# Remove columns from dummy_data_pay that already exist in Currency_with_dummies
existing_columns <- names(Currency_with_dummies)
dummy_data_pay <- dummy_data_pay[, !(names(dummy_data_pay) %in% existing_columns)]


# Bind the new dummy variables to Currency_with_dummies
Currency_with_dummies <- cbind(Currency_with_dummies, dummy_data_pay)

# Iterating over each row of the Currency data frame
for (i in 1:nrow(Currency)) {
  # Get the current 'Get' value
  current_get <- Currency$Get[i]
  
  # If a dummy column name matches the 'Get' value, assign 1
  if (current_get %in% names(Currency)) {
    Currency[i, current_get] <- 1
  }
}

# Convert 'Pay' column to character
Currency_with_dummies$Pay <- as.character(Currency_with_dummies$Pay)

# Initialize a vector to store unmatched 'Pay' values
unmatched_pay_values <- character(0)

# Iterating over each row of the Currency data frame
for (i in 1:nrow(Currency_with_dummies)) {
  # Get the current 'Pay' value
  current_pay <- Currency_with_dummies$Pay[i]
  
  # If a dummy column name matches the 'Pay' value, assign 2
  if (current_pay %in% names(Currency_with_dummies)) {
    Currency_with_dummies[i, current_pay] <- 2
  } else {
    # Add unmatched 'Pay' value to the vector
    unmatched_pay_values <- c(unmatched_pay_values, current_pay)
  }
}

# Remove the original 'Get' and 'Pay' columns
Currency_with_dummies <- Currency_with_dummies[, !(names(Currency_with_dummies) %in% c("Get", "Pay"))]


# Remove spaces and special characters from column names
colnames(Currency_with_dummies) <- make.names(colnames(Currency_with_dummies))





# Specify the target variable
target_variable <- "Cluster"  # Assuming "Cluster" is the target variable

# Split the data into training and test sets
set.seed(123)  # For reproducibility
split <- createDataPartition(Currency_with_dummies$Cluster, p = 0.8, list = FALSE)
trainingSet <- Currency_with_dummies[split, ]
testSet <- Currency_with_dummies[-split, ]

# Train the Random Forest model
rf_model <- randomForest(formula = as.formula(paste(target_variable, "~ .")), data = trainingSet, ntree = 50)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = testSet)

# Evaluate the model's performance
confusion_matrix <- table(predictions, testSet$Cluster)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(predictions == testSet$Cluster) / length(testSet$Cluster)
cat("Model Accuracy:", accuracy, "\n")



# Convert the confusion matrix to a dataframe for ggplot
conf_matrix_df <- as.data.frame(as.table(confusion_matrix))

conf_matrix_df$predictions <- factor(conf_matrix_df$predictions, levels = c("1", "2", "3"), labels = c("Class 1", "Class 2", "Class 3"))
conf_matrix_df$Var2 <- factor(conf_matrix_df$Var2, levels = c("1", "2", "3"), labels = c("Class 1", "Class 2", "Class 3"))

ggplot(conf_matrix_df, aes(x = predictions, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_discrete(limits = c("Class 1", "Class 2", "Class 3")) +
  scale_y_discrete(limits = rev(c("Class 1", "Class 2", "Class 3"))) +
  labs(title = "Confusion Matrix", x = "Predicted Class", y = "Actual Class") +
  theme_minimal()


# Create 5-fold cross-validation
folds <- createFolds(trainingSet$Cluster, k = 5)
cv_control <- trainControl(method = "cv", index = folds)

# Define a tuning grid for mtry (adjust the values as needed)
tuneGrid <- expand.grid(mtry = seq(2, sqrt(ncol(trainingSet)), by = 2))

rf_cv_model <- train(Cluster ~ ., data = trainingSet, method = "rf", trControl = cv_control, tuneGrid = tuneGrid, ntree = 50)

# Print the results
print(rf_cv_model)

# Make predictions using the best tuned model
predictions <- predict(rf_cv_model, newdata = testSet)

# Evaluate the model's performance
confusion_matrix <- table(predictions, testSet$Cluster)
print(confusion_matrix)

# Calculate and print accuracy
accuracy <- sum(predictions == testSet$Cluster) / length(testSet$Cluster)
cat("Model Accuracy:", accuracy, "\n")

# Convert the confusion matrix to a dataframe for ggplot
conf_matrix_df <- as.data.frame(as.table(confusion_matrix))

conf_matrix_df$predictions <- factor(conf_matrix_df$predictions, levels = c("1", "2", "3"), labels = c("Class 1", "Class 2", "Class 3"))
conf_matrix_df$Var2 <- factor(conf_matrix_df$Var2, levels = c("1", "2", "3"), labels = c("Class 1", "Class 2", "Class 3"))

ggplot(conf_matrix_df, aes(x = predictions, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), vjust = 1.5) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_discrete(limits = c("Class 1", "Class 2", "Class 3")) +
  scale_y_discrete(limits = rev(c("Class 1", "Class 2", "Class 3"))) +
  labs(title = "Confusion Matrix", x = "Predicted Class", y = "Actual Class") +
  theme_minimal()








###########################################################
Currency <- read_excel("C:/DS/707/Currency.xlsx")

data <- Currency
data <- data[,-1]

colnames(data) <- tolower(colnames(data))

data <- data[, -ncol(data)]

data <- data[data$pay == "Chaos Orb", ]

data <- data[, -which(colnames(data) == "pay")]

# Filter the dataframe to exclude rows where "get" is "Mirror of Kalandra"
data <- data[data$get != "Mirror of Kalandra", ]
data <- data[data$get != "Mirror Shard", ]

unique_pay_values <- unique(data$get)
unique_pay_values

data_frames <- split(data, data$get)

earliest_date <- min(data$date)

latest_date <- max(data$date)

# Load the ggplot2 library if not already loaded
library(ggplot2)

# Create a line plot overlaying all lines for each Currency
ggplot(data, aes(x = date, y = value)) +
  geom_line(aes(color = get)) +
  labs(title = "Value Change for Each Currency",
       x = "date",
       y = "value") +
  theme_minimal() +
  theme(legend.position = "none")


library(dplyr)

# Define the timeframes for "end of league" and "middle of league"
end_of_league_date <- latest_date  # Use the latest date as the end of league
middle_of_league_date <- earliest_date + (latest_date - earliest_date) / 2  # Middle of the league
quarter_length <- (latest_date - earliest_date) / 4
week2_length <- (latest_date - earliest_date) / 8

# Define the start and end dates for each quarter
week2_end <- earliest_date + week2_length
quarter_1_start <- earliest_date
quarter_1_end <- earliest_date + quarter_length
quarter_middle_start <- quarter_1_end + 1
quarter_middle_end <- quarter_1_end + quarter_length
quarter_3_start <- quarter_middle_end + 1
quarter_3_end <- quarter_middle_end + quarter_length
quarter_end_start <- quarter_3_end + 1
quarter_end_end <- latest_date

# Filter data for each quarter
week2_data <- data %>%
  filter(date >= quarter_1_start, date <= week2_end)
quarter_1_data <- data %>%
  filter(date >= quarter_1_start, date <= quarter_1_end)
quarter_middle_data <- data %>%
  filter(date >= quarter_middle_start, date <= quarter_middle_end)
quarter_3_data <- data %>%
  filter(date >= quarter_3_start, date <= quarter_3_end)
quarter_end_data <- data %>%
  filter(date >= quarter_end_start, date <= quarter_end_end)

# Calculate the rate of change from the beginning to the end of each quarter
week2_data <- week2_data %>%
  group_by(get) %>%
  mutate(begin_value = first(value), end_value = last(value), rate_of_change = (end_value - begin_value) / begin_value)

quarter_1_data <- quarter_1_data %>%
  group_by(get) %>%
  mutate(begin_value = first(value), end_value = last(value), rate_of_change = (end_value - begin_value) / begin_value)

quarter_middle_data <- quarter_middle_data %>%
  group_by(get) %>%
  mutate(begin_value = first(value), end_value = last(value), rate_of_change = (end_value - begin_value) / begin_value)

quarter_3_data <- quarter_3_data %>%
  group_by(get) %>%
  mutate(begin_value = first(value), end_value = last(value), rate_of_change = (end_value - begin_value) / begin_value)

quarter_end_data <- quarter_end_data %>%
  group_by(get) %>%
  mutate(begin_value = first(value), end_value = last(value), rate_of_change = (end_value - begin_value) / begin_value)

# Retrieve the distinct values of "get" with the beginning, end values, begin_date, and end_date, and rate of change of each quarter
top_10_week_2 <- week2_data %>%
  group_by(get) %>%
  summarise(begin_value = first(begin_value), end_value = last(end_value), begin_date = first(date), end_date = last(date), rate_of_change = last(rate_of_change)) %>%
  arrange(desc(rate_of_change)) %>%
  head(10)

top_10_quarter_1 <- quarter_1_data %>%
  group_by(get) %>%
  summarise(begin_value = first(begin_value), end_value = last(end_value), begin_date = first(date), end_date = last(date), rate_of_change = last(rate_of_change)) %>%
  arrange(desc(rate_of_change)) %>%
  head(10)

top_10_middle_of_league <- quarter_middle_data %>%
  group_by(get) %>%
  summarise(begin_value = first(begin_value), end_value = last(end_value), begin_date = first(date), end_date = last(date), rate_of_change = last(rate_of_change)) %>%
  arrange(desc(rate_of_change)) %>%
  head(10)

top_10_quarter_3 <- quarter_3_data %>%
  group_by(get) %>%
  summarise(begin_value = first(begin_value), end_value = last(end_value), begin_date = first(date), end_date = last(date), rate_of_change = last(rate_of_change)) %>%
  arrange(desc(rate_of_change)) %>%
  head(10)

top_10_end_of_league <- quarter_end_data %>%
  group_by(get) %>%
  summarise(begin_value = first(begin_value), end_value = last(end_value), begin_date = first(date), end_date = last(date), rate_of_change = last(rate_of_change)) %>%
  arrange(desc(rate_of_change)) %>%
  head(10)





# Create a function to generate bar plots with labels
generate_labeled_bar_plot <- function(data, title, x_label, y_label, fill_color, label) {
  ggplot(data, aes(x = reorder(get, -rate_of_change), y = rate_of_change)) +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    annotate("text", x = Inf, y = Inf, label = label, vjust = 1, hjust = 1, size = 8)
}


# Create labeled bar plots for each quarter and timeframe
plot_quarter_1 <- generate_labeled_bar_plot(top_10_quarter_1, "1/4 of the League", "Currency", "Rate of Change", "red", "1/4")
plot_middle_of_league <- generate_labeled_bar_plot(top_10_middle_of_league, "Middle of League", "Currency", "Rate of Change", "green", "2/4")
plot_quarter_3 <- generate_labeled_bar_plot(top_10_quarter_3, "3/4 of the League", "Currency", "Rate of Change", "orange", "3/4")
plot_end_of_league <- generate_labeled_bar_plot(top_10_end_of_league, "End of League", "Currency", "Rate of Change", "blue", "4/4")

# Arrange the plots in a grid
grid.arrange(plot_quarter_1, plot_middle_of_league, plot_quarter_3, plot_end_of_league, ncol = 2)




library(dplyr)

currency_data <- Currency
currency_data <- currency_data[,-1]

colnames(currency_data) <- tolower(colnames(currency_data))

currency_data <- currency_data[, -ncol(currency_data)]

# Filter out rows where either 'get' or 'pay' is "Mirror of Kalandra" or "Mirror Shard"
currency_data <- currency_data %>%
  filter(!(get %in% c("Mirror of Kalandra", "Mirror Shard", "Esh's Flawless Breachstone") | pay %in% c("Mirror of Kalandra", "Mirror Shard", "Esh's Flawless Breachstone")))








earliest_date <- min(currency_data$date)

latest_date <- max(currency_data$date)



# Filter the dataset based on the start and end dates
currency_data <- currency_data %>%
  filter(date >= earliest_date & date <= latest_date) %>%
  arrange(date)

currency_data <- currency_data %>%
  group_by(get, pay) %>%
  mutate(daily_percentage_change = (value - lag(value, default = first(value))) / lag(value, default = first(value)) * 100)

# Filter the dataset to include only "Divine Orb" transactions where you paid with "Chaos Orb"
divine_orb_chaos_data <- currency_data %>%
  filter(get == "Divine Orb" & pay == "Chaos Orb") %>%
  arrange(date)

# Calculate the daily percentage change for "Divine Orb" with "pay" as "Chaos Orb"
divine_orb_chaos_data <- divine_orb_chaos_data %>%
  mutate(
    lagged_value = lag(value),
    daily_percentage_change = (value - lagged_value) / lagged_value * 100
  )

# Create a time series plot of the daily percentage change for "Divine Orb"
ggplot(divine_orb_chaos_data, aes(x = date, y = daily_percentage_change)) +
  geom_line() +
  labs(x = "Date", y = "Daily Percentage Change") +
  ggtitle("Daily Percentage Price Change of Divine Orb (pay: Chaos Orb)")

# Filter the dataset to include only "Divine Orb" transactions where you paid with "Chaos Orb"
divine_orb_chaos_data <- currency_data %>%
  filter(get == "Divine Orb" & pay == "Chaos Orb") %>%
  arrange(date)

# Calculate the price change of "Divine Orb" in terms of "Chaos Orb"
divine_orb_chaos_data <- divine_orb_chaos_data %>%
  mutate(
    price_in_chaos_orb = value
  )

# Create a time series plot of the price change of "Divine Orb" in terms of "Chaos Orb"
ggplot(divine_orb_chaos_data, aes(x = date, y = price_in_chaos_orb)) +
  geom_line() +
  labs(x = "Date", y = "Price of Divine Orb in Chaos Orb") +
  ggtitle("Price Change of Divine Orb in Terms of Chaos Orb")


# Replace 'divine_orb_price' with the correct column name in both plots

# Create a time series plot of the daily percentage change for "Divine Orb"
plot_percentage_change <- ggplot(divine_orb_chaos_data, aes(x = date, y = daily_percentage_change)) +
  geom_line() +
  labs(x = "Date", y = "Daily Percentage Change") +
  ggtitle("Daily Percentage Price Change of Divine Orb (pay: Chaos Orb)") +
  theme_minimal()  # Optional: Use a minimal theme

# Create a time series plot of the price change of "Divine Orb" in terms of "Chaos Orb"
plot_price_change <- ggplot(divine_orb_chaos_data, aes(x = date, y = value)) +
  geom_line() +
  labs(x = "Date", y = "Price of Divine Orb in Chaos Orb") +
  ggtitle("Price Change of Divine Orb in Terms of Chaos Orb") +
  theme_minimal()  # Optional: Use a minimal theme

# Arrange both plots side by side
combined_plots <- plot_percentage_change / plot_price_change

# Print the combined plots
print(combined_plots)







# Replace 'currency_data' with your actual dataframe name if it's different
# Let's calculate the correlations with "Chaos Orb" and "Divine Orb" for each unique currency in 'get' column
unique_currencies <- unique(currency_data$get)

# Create empty dataframes to store correlation results
correlation_results_chaos <- data.frame(Currency = character(0), Correlation = numeric(0))
correlation_results_divine <- data.frame(Currency = character(0), Correlation = numeric(0))

# Extract the values for "Chaos Orb" and "Divine Orb"
chaos_orb_values <- currency_data %>%
  filter(get == "Chaos Orb") %>%
  select(value)

divine_orb_values <- currency_data %>%
  filter(get == "Divine Orb") %>%
  select(value)

# Convert "Chaos Orb" and "Divine Orb" values to numeric
chaos_orb_values <- as.numeric(chaos_orb_values$value)
divine_orb_values <- as.numeric(divine_orb_values$value)

for (currency in unique_currencies) {
  # Filter data for the specific currency
  currency_subset <- currency_data %>%
    filter(get == currency)
  
  # Extract the values for the current currency
  currency_values <- currency_subset %>%
    select(value)
  
  # Convert currency values to numeric
  currency_values <- as.numeric(currency_values$value)
  
  # Ensure that all vectors have the same length
  min_length <- min(length(currency_values), length(chaos_orb_values), length(divine_orb_values))
  currency_values <- currency_values[1:min_length]
  chaos_orb_values <- chaos_orb_values[1:min_length]
  divine_orb_values <- divine_orb_values[1:min_length]
  
  # Calculate the correlation with "Chaos Orb" using Spearman method
  correlation_chaos <- cor(currency_values, chaos_orb_values, method = "spearman")
  
  # Calculate the correlation with "Divine Orb" using Spearman method
  correlation_divine <- cor(currency_values, divine_orb_values, method = "spearman")
  
  # Add the results to the dataframes
  correlation_results_chaos <- rbind(correlation_results_chaos, data.frame(Currency = currency, Correlation = correlation_chaos))
  correlation_results_divine <- rbind(correlation_results_divine, data.frame(Currency = currency, Correlation = correlation_divine))
}

# Order the results by the absolute value of correlation in descending order for "Chaos Orb"
correlation_results_chaos <- correlation_results_chaos %>%
  arrange(desc(abs(Correlation)))

# Order the results by the absolute value of correlation in descending order for "Divine Orb"
correlation_results_divine <- correlation_results_divine %>%
  arrange(desc(abs(Correlation)))

# Print the results for "Chaos Orb"
cat("Correlations with Chaos Orb:\n")
print(correlation_results_chaos)

# Print the results for "Divine Orb"
cat("\nCorrelations with Divine Orb:\n")
print(correlation_results_divine)
str(correlation_results_divine)






# Let's calculate the correlations and p-values with "Chaos Orb" and "Divine Orb" for each unique currency in 'get' column
unique_currencies <- unique(currency_data$get)

# Create empty dataframes to store correlation results
correlation_results_chaos <- data.frame(Currency = character(0), Correlation = numeric(0), P_Value = numeric(0))
correlation_results_divine <- data.frame(Currency = character(0), Correlation = numeric(0), P_Value = numeric(0))

# Extract the values for "Chaos Orb" and "Divine Orb"
chaos_orb_values <- currency_data %>%
  filter(get == "Chaos Orb") %>%
  select(value)

divine_orb_values <- currency_data %>%
  filter(get == "Divine Orb") %>%
  select(value)

# Convert "Chaos Orb" and "Divine Orb" values to numeric
chaos_orb_values <- as.numeric(chaos_orb_values$value)
divine_orb_values <- as.numeric(divine_orb_values$value)

for (currency in unique_currencies) {
  # Filter data for the specific currency
  currency_subset <- currency_data %>%
    filter(get == currency)
  
  # Extract the values for the current currency
  currency_values <- currency_subset %>%
    select(value)
  
  # Convert currency values to numeric
  currency_values <- as.numeric(currency_values$value)
  
  # Ensure that all vectors have the same length
  min_length <- min(length(currency_values), length(chaos_orb_values), length(divine_orb_values))
  currency_values <- currency_values[1:min_length]
  chaos_orb_values <- chaos_orb_values[1:min_length]
  divine_orb_values <- divine_orb_values[1:min_length]
  
  # Calculate the correlation and p-value with "Chaos Orb" using Spearman method
  test_chaos <- cor.test(currency_values, chaos_orb_values, method = "spearman")
  
  # Calculate the correlation and p-value with "Divine Orb" using Spearman method
  test_divine <- cor.test(currency_values, divine_orb_values, method = "spearman")
  
  # Add the results to the dataframes
  correlation_results_chaos <- rbind(correlation_results_chaos, data.frame(Currency = currency, Correlation = test_chaos$estimate, P_Value = test_chaos$p.value))
  correlation_results_divine <- rbind(correlation_results_divine, data.frame(Currency = currency, Correlation = test_divine$estimate, P_Value = test_divine$p.value))
}

test_chaos <- cor.test(currency_values, chaos_orb_values, method = "spearman")
test_divine <- cor.test(currency_values, divine_orb_values, method = "spearman")

# Order the results by the absolute value of correlation in descending order for "Chaos Orb"
correlation_results_chaos <- correlation_results_chaos %>%
  arrange(desc(abs(Correlation)))

# Order the results by the absolute value of correlation in descending order for "Divine Orb"
correlation_results_divine <- correlation_results_divine %>%
  arrange(desc(abs(Correlation)))

# Print the results for "Chaos Orb"
cat("Correlations with Chaos Orb:\n")
print(correlation_results_chaos)

# Print the results for "Divine Orb"
cat("\nCorrelations with Divine Orb:\n")
print(correlation_results_divine)


###############################################################
# Load the required libraries
library(dplyr)

# Replace 'currency_data' with your actual dataframe name if it's different
# Get all unique currencies in the 'get' and 'pay' columns
all_currencies <- unique(c(currency_data$get, currency_data$pay))

# Create an empty correlation matrix
correlation_matrix <- matrix(NA, nrow = length(all_currencies), ncol = length(all_currencies))

# Set row and column names for the correlation matrix
colnames(correlation_matrix) <- all_currencies
rownames(correlation_matrix) <- all_currencies

# Iterate through pairs of currencies and calculate correlations
for (i in 1:length(all_currencies)) {
  for (j in 1:length(all_currencies)) {
    # Filter data for the two currencies
    currency_i_subset <- currency_data %>%
      filter(get == all_currencies[i])
    
    currency_j_subset <- currency_data %>%
      filter(get == all_currencies[j])
    
    # Extract the values for the current currencies and convert to numeric
    currency_i_values <- as.numeric(currency_i_subset$value)
    currency_j_values <- as.numeric(currency_j_subset$value)
    
    # Check if both currency data frames have non-empty values
    if (length(currency_i_values) > 0 && length(currency_j_values) > 0 &&
        length(currency_i_values) == length(currency_j_values)) {
      # Calculate the correlation using Spearman method
      correlation <- cor(currency_i_values, currency_j_values, method = "spearman", use = "pairwise.complete.obs")
      
      # Store the correlation in the matrix
      correlation_matrix[i, j] <- correlation
    } else {
      # If dimensions don't match or data is empty, store NA in the matrix
      correlation_matrix[i, j] <- NA
    }
  }
}

# Print the correlation matrix
print(correlation_matrix)




melted_correlation_matrix <- melt(correlation_matrix, varnames = c("Currency1", "Currency2"))

ggplot(melted_correlation_matrix, aes(x = Currency1, y = Currency2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Removes the x-axis item names
    axis.text.y = element_blank(),  # Removes the y-axis item names
    axis.ticks = element_blank()  # Keeping the axis ticks
  ) +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "")





# Filter out rows where either 'get' or 'pay' is "Mirror of Kalandra" or "Mirror Shard"
currency_data <- currency_data %>%
  filter(!(get %in% c("Operative's Scouting Report") | pay %in% c("Operative's Scouting Report")))



# Convert Date to numeric format (e.g., number of days since the earliest date)
currency_data$date_numeric <- as.numeric(currency_data$date - min(currency_data$date))

correlation_results <- currency_data %>%
  group_by(get) %>%
  summarize(correlation = cor(date_numeric, value, use = "complete.obs"))

print(correlation_results)

# Create a scatter plot of date vs. value for each currency with color but no item names on axes
ggplot(currency_data, aes(x = date, y = value, color = get)) +
  geom_point() +
  labs(title = "Correlation between Date and Currency Value", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())





# Find the last date in your dataset
last_date <- max(aggregated_data$date)

# Get the last value for each currency
last_values <- aggregated_data %>%
  filter(date == last_date) %>%
  arrange(desc(value))

# Order the wide_data columns based on the last values
ordered_columns <- c("date", last_values$get)
wide_data_ordered <- wide_data[ordered_columns]


currency_data <- currency_data %>%
  group_by(get) %>%
  arrange(date) %>%
  mutate(daily_change = (value - lag(value)) / lag(value) * 100) %>%
  ungroup()

fluctuation_measure <- currency_data %>%
  filter(!is.na(daily_change)) %>%
  group_by(get) %>%
  summarize(fluctuation = sd(daily_change, na.rm = TRUE)) %>%
  ungroup()

top_items <- fluctuation_measure %>%
  arrange(desc(fluctuation)) %>%
  head(30) %>%
  pull(get)

filtered_data <- currency_data %>%
  filter(get %in% top_items)

aggregated_data <- filtered_data %>%
  group_by(date, get) %>%
  summarize(value = mean(value))

wide_data <- aggregated_data %>%
  spread(key = get, value = value)

wide_data$date <- as.factor(wide_data$date)

# Melting the ordered data into a long format for ggplot
melted_ordered_data <- melt(wide_data_ordered, id.vars = 'date')

ggplot(melted_ordered_data, aes(x = date, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", name = "Value") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Removes the x-axis labels
    axis.ticks.x = element_blank()  # Removes the x-axis ticks
  ) +
  labs(x = "Date", y = "Currency Type", title = "Top 30 Fluctuating Currency Values Over Time")



# Filter data for the top 30 items and calculate the average daily change rate
aggregated_change_rate_data <- filtered_data %>%
  group_by(date, get) %>%
  summarize(average_daily_change = mean(daily_change, na.rm = TRUE))

# Spread the data to wide format for the heatmap
wide_change_data <- aggregated_change_rate_data %>%
  spread(key = get, value = average_daily_change)

# Convert dates to a factor
wide_change_data$date <- as.factor(wide_change_data$date)

# Melting the data into a long format for ggplot
melted_change_data <- melt(wide_change_data, id.vars = 'date')


# Ensure that 'last_values$get' contains the currency names in the desired order
ordered_currencies <- last_values$get

# Filter 'wide_change_data' to include only the top currencies and reorder columns
ordered_change_data <- wide_change_data %>%
  select(c("date", intersect(ordered_currencies, names(wide_change_data))))

# Convert dates to a factor (if not already done)
ordered_change_data$date <- as.factor(ordered_change_data$date)

# Melt the ordered data for plotting
melted_ordered_change_data <- melt(ordered_change_data, id.vars = 'date')

# Plotting the heatmap with ordered currencies
ggplot(melted_ordered_change_data, aes(x = date, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "%") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(x = "Date", y = "Currency Type", title = "Daily Change Rate for Top 30 Fluctuating Currencies")



Currency <- read_excel("C:/DS/707/Currency.xlsx")

data <- Currency
data <- data[,-1]

colnames(data) <- tolower(colnames(data))

data <- data[, -ncol(data)]

data <- data[data$pay == "Chaos Orb", ]

data <- data[, -which(colnames(data) == "pay")]

# Filter the dataframe to exclude rows where "get" is "Mirror of Kalandra"
data <- data[data$get != "Mirror of Kalandra", ]
data <- data[data$get != "Mirror Shard", ]

unique_pay_values <- unique(data$get)
unique_pay_values

data_frames <- split(data, data$get)

earliest_date <- min(data$date)

latest_date <- max(data$date)

# Create a line plot overlaying all lines for each Currency
ggplot(data, aes(x = date, y = value)) +
  geom_line(aes(color = get)) +
  labs(title = "Value Change for Each Currency",
       x = "date",
       y = "value") +
  theme_minimal() +
  theme(legend.position = "none")


# Define the timeframes for "end of league" and "middle of league"
end_of_league_date <- latest_date  # Use the latest date as the end of league
middle_of_league_date <- earliest_date + (latest_date - earliest_date) / 2  # Middle of the league
quarter_length <- (latest_date - earliest_date) / 4
week2_length <- (latest_date - earliest_date) / 8

# Define the start and end dates for each quarter
week2_end <- earliest_date + week2_length
quarter_1_start <- earliest_date
quarter_1_end <- earliest_date + quarter_length
quarter_middle_start <- quarter_1_end + 1
quarter_middle_end <- quarter_1_end + quarter_length
quarter_3_start <- quarter_middle_end + 1
quarter_3_end <- quarter_middle_end + quarter_length
quarter_end_start <- quarter_3_end + 1
quarter_end_end <- latest_date

# Filter data for each quarter
week2_data <- data %>%
  filter(date >= quarter_1_start, date <= week2_end)
quarter_1_data <- data %>%
  filter(date >= quarter_1_start, date <= quarter_1_end)
quarter_middle_data <- data %>%
  filter(date >= quarter_middle_start, date <= quarter_middle_end)
quarter_3_data <- data %>%
  filter(date >= quarter_3_start, date <= quarter_3_end)
quarter_end_data <- data %>%
  filter(date >= quarter_end_start, date <= quarter_end_end)

# Calculate the rate of change from the beginning to the end of each quarter
week2_data <- week2_data %>%
  group_by(get) %>%
  mutate(begin_value = first(value), end_value = last(value), rate_of_change = (end_value - begin_value) / begin_value)

quarter_1_data <- quarter_1_data %>%
  group_by(get) %>%
  mutate(begin_value = first(value), end_value = last(value), rate_of_change = (end_value - begin_value) / begin_value)

quarter_middle_data <- quarter_middle_data %>%
  group_by(get) %>%
  mutate(begin_value = first(value), end_value = last(value), rate_of_change = (end_value - begin_value) / begin_value)

quarter_3_data <- quarter_3_data %>%
  group_by(get) %>%
  mutate(begin_value = first(value), end_value = last(value), rate_of_change = (end_value - begin_value) / begin_value)

quarter_end_data <- quarter_end_data %>%
  group_by(get) %>%
  mutate(begin_value = first(value), end_value = last(value), rate_of_change = (end_value - begin_value) / begin_value)

# Retrieve the distinct values of "get" with the beginning, end values, begin_date, and end_date, and rate of change of each quarter
top_10_week_2 <- week2_data %>%
  group_by(get) %>%
  summarise(begin_value = first(begin_value), end_value = last(end_value), begin_date = first(date), end_date = last(date), rate_of_change = last(rate_of_change)) %>%
  arrange(desc(rate_of_change)) %>%
  head(10)

top_10_quarter_1 <- quarter_1_data %>%
  group_by(get) %>%
  summarise(begin_value = first(begin_value), end_value = last(end_value), begin_date = first(date), end_date = last(date), rate_of_change = last(rate_of_change)) %>%
  arrange(desc(rate_of_change)) %>%
  head(10)

top_10_middle_of_league <- quarter_middle_data %>%
  group_by(get) %>%
  summarise(begin_value = first(begin_value), end_value = last(end_value), begin_date = first(date), end_date = last(date), rate_of_change = last(rate_of_change)) %>%
  arrange(desc(rate_of_change)) %>%
  head(10)

top_10_quarter_3 <- quarter_3_data %>%
  group_by(get) %>%
  summarise(begin_value = first(begin_value), end_value = last(end_value), begin_date = first(date), end_date = last(date), rate_of_change = last(rate_of_change)) %>%
  arrange(desc(rate_of_change)) %>%
  head(10)

top_10_end_of_league <- quarter_end_data %>%
  group_by(get) %>%
  summarise(begin_value = first(begin_value), end_value = last(end_value), begin_date = first(date), end_date = last(date), rate_of_change = last(rate_of_change)) %>%
  arrange(desc(rate_of_change)) %>%
  head(10)





# Create a function to generate bar plots with labels
generate_labeled_bar_plot <- function(data, title, x_label, y_label, fill_color, label) {
  ggplot(data, aes(x = reorder(get, -rate_of_change), y = rate_of_change)) +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    annotate("text", x = Inf, y = Inf, label = label, vjust = 1, hjust = 1, size = 8)
}


# Create labeled bar plots for each quarter and timeframe
plot_quarter_1 <- generate_labeled_bar_plot(top_10_quarter_1, "1/4 of the League", "Currency", "Rate of Change", "red", "1/4")
plot_middle_of_league <- generate_labeled_bar_plot(top_10_middle_of_league, "Middle of League", "Currency", "Rate of Change", "green", "2/4")
plot_quarter_3 <- generate_labeled_bar_plot(top_10_quarter_3, "3/4 of the League", "Currency", "Rate of Change", "orange", "3/4")
plot_end_of_league <- generate_labeled_bar_plot(top_10_end_of_league, "End of League", "Currency", "Rate of Change", "blue", "4/4")

# Arrange the plots in a grid
grid.arrange(plot_quarter_1, plot_middle_of_league, plot_quarter_3, plot_end_of_league, ncol = 2)








currency_data <- Currency
currency_data <- currency_data[,-1]

colnames(currency_data) <- tolower(colnames(currency_data))

currency_data <- currency_data[, -ncol(currency_data)]

# Filter out rows where either 'get' or 'pay' is "Mirror of Kalandra" or "Mirror Shard"
currency_data <- currency_data %>%
  filter(!(get %in% c("Mirror of Kalandra", "Mirror Shard", "Esh's Flawless Breachstone") | pay %in% c("Mirror of Kalandra", "Mirror Shard", "Esh's Flawless Breachstone")))








earliest_date <- min(currency_data$date)

latest_date <- max(currency_data$date)



# Filter the dataset based on the start and end dates
currency_data <- currency_data %>%
  filter(date >= earliest_date & date <= latest_date) %>%
  arrange(date)

currency_data <- currency_data %>%
  group_by(get, pay) %>%
  mutate(daily_percentage_change = (value - lag(value, default = first(value))) / lag(value, default = first(value)) * 100)

# Filter the dataset to include only "Divine Orb" transactions where you paid with "Chaos Orb"
divine_orb_chaos_data <- currency_data %>%
  filter(get == "Divine Orb" & pay == "Chaos Orb") %>%
  arrange(date)

# Calculate the daily percentage change for "Divine Orb" with "pay" as "Chaos Orb"
divine_orb_chaos_data <- divine_orb_chaos_data %>%
  mutate(
    lagged_value = lag(value),
    daily_percentage_change = (value - lagged_value) / lagged_value * 100
  )

# Create a time series plot of the daily percentage change for "Divine Orb"
ggplot(divine_orb_chaos_data, aes(x = date, y = daily_percentage_change)) +
  geom_line() +
  labs(x = "Date", y = "Daily Percentage Change") +
  ggtitle("Daily Percentage Price Change of Divine Orb (pay: Chaos Orb)")

# Filter the dataset to include only "Divine Orb" transactions where you paid with "Chaos Orb"
divine_orb_chaos_data <- currency_data %>%
  filter(get == "Divine Orb" & pay == "Chaos Orb") %>%
  arrange(date)

# Calculate the price change of "Divine Orb" in terms of "Chaos Orb"
divine_orb_chaos_data <- divine_orb_chaos_data %>%
  mutate(
    price_in_chaos_orb = value
  )

# Create a time series plot of the price change of "Divine Orb" in terms of "Chaos Orb"
ggplot(divine_orb_chaos_data, aes(x = date, y = price_in_chaos_orb)) +
  geom_line() +
  labs(x = "Date", y = "Price of Divine Orb in Chaos Orb") +
  ggtitle("Price Change of Divine Orb in Terms of Chaos Orb")


# Create a time series plot of the daily percentage change for "Divine Orb"
plot_percentage_change <- ggplot(divine_orb_chaos_data, aes(x = date, y = daily_percentage_change)) +
  geom_line() +
  labs(x = "Date", y = "Daily Percentage Change") +
  ggtitle("Daily Percentage Price Change of Divine Orb (pay: Chaos Orb)") +
  theme_minimal()  # Optional: Use a minimal theme

# Create a time series plot of the price change of "Divine Orb" in terms of "Chaos Orb"
plot_price_change <- ggplot(divine_orb_chaos_data, aes(x = date, y = value)) +
  geom_line() +
  labs(x = "Date", y = "Price of Divine Orb in Chaos Orb") +
  ggtitle("Price Change of Divine Orb in Terms of Chaos Orb") +
  theme_minimal()  # Optional: Use a minimal theme

# Arrange both plots side by side
combined_plots <- plot_percentage_change / plot_price_change

# Print the combined plots
print(combined_plots)



# Replace 'currency_data' with your actual dataframe name if it's different
# Let's calculate the correlations with "Chaos Orb" and "Divine Orb" for each unique currency in 'get' column
unique_currencies <- unique(currency_data$get)

# Create empty dataframes to store correlation results
correlation_results_chaos <- data.frame(Currency = character(0), Correlation = numeric(0))
correlation_results_divine <- data.frame(Currency = character(0), Correlation = numeric(0))

# Extract the values for "Chaos Orb" and "Divine Orb"
chaos_orb_values <- currency_data %>%
  filter(get == "Chaos Orb") %>%
  select(value)

divine_orb_values <- currency_data %>%
  filter(get == "Divine Orb") %>%
  select(value)

# Convert "Chaos Orb" and "Divine Orb" values to numeric
chaos_orb_values <- as.numeric(chaos_orb_values$value)
divine_orb_values <- as.numeric(divine_orb_values$value)

for (currency in unique_currencies) {
  # Filter data for the specific currency
  currency_subset <- currency_data %>%
    filter(get == currency)
  
  # Extract the values for the current currency
  currency_values <- currency_subset %>%
    select(value)
  
  # Convert currency values to numeric
  currency_values <- as.numeric(currency_values$value)
  
  # Ensure that all vectors have the same length
  min_length <- min(length(currency_values), length(chaos_orb_values), length(divine_orb_values))
  currency_values <- currency_values[1:min_length]
  chaos_orb_values <- chaos_orb_values[1:min_length]
  divine_orb_values <- divine_orb_values[1:min_length]
  
  # Calculate the correlation with "Chaos Orb" using Spearman method
  correlation_chaos <- cor(currency_values, chaos_orb_values, method = "spearman")
  
  # Calculate the correlation with "Divine Orb" using Spearman method
  correlation_divine <- cor(currency_values, divine_orb_values, method = "spearman")
  
  # Add the results to the dataframes
  correlation_results_chaos <- rbind(correlation_results_chaos, data.frame(Currency = currency, Correlation = correlation_chaos))
  correlation_results_divine <- rbind(correlation_results_divine, data.frame(Currency = currency, Correlation = correlation_divine))
}

# Order the results by the absolute value of correlation in descending order for "Chaos Orb"
correlation_results_chaos <- correlation_results_chaos %>%
  arrange(desc(abs(Correlation)))

# Order the results by the absolute value of correlation in descending order for "Divine Orb"
correlation_results_divine <- correlation_results_divine %>%
  arrange(desc(abs(Correlation)))

# Print the results for "Chaos Orb"
cat("Correlations with Chaos Orb:\n")
print(correlation_results_chaos)

# Print the results for "Divine Orb"
cat("\nCorrelations with Divine Orb:\n")
print(correlation_results_divine)
str(correlation_results_divine)



###############################################################
# Get all unique currencies in the 'get' and 'pay' columns
all_currencies <- unique(c(currency_data$get, currency_data$pay))

# Create an empty correlation matrix
correlation_matrix <- matrix(NA, nrow = length(all_currencies), ncol = length(all_currencies))

# Set row and column names for the correlation matrix
colnames(correlation_matrix) <- all_currencies
rownames(correlation_matrix) <- all_currencies

# Iterate through pairs of currencies and calculate correlations
for (i in 1:length(all_currencies)) {
  for (j in 1:length(all_currencies)) {
    # Filter data for the two currencies
    currency_i_subset <- currency_data %>%
      filter(get == all_currencies[i])
    
    currency_j_subset <- currency_data %>%
      filter(get == all_currencies[j])
    
    # Extract the values for the current currencies and convert to numeric
    currency_i_values <- as.numeric(currency_i_subset$value)
    currency_j_values <- as.numeric(currency_j_subset$value)
    
    # Check if both currency data frames have non-empty values
    if (length(currency_i_values) > 0 && length(currency_j_values) > 0 &&
        length(currency_i_values) == length(currency_j_values)) {
      # Calculate the correlation using Spearman method
      correlation <- cor(currency_i_values, currency_j_values, method = "spearman", use = "pairwise.complete.obs")
      
      # Store the correlation in the matrix
      correlation_matrix[i, j] <- correlation
    } else {
      # If dimensions don't match or data is empty, store NA in the matrix
      correlation_matrix[i, j] <- NA
    }
  }
}

# Print the correlation matrix
print(correlation_matrix)




melted_correlation_matrix <- melt(correlation_matrix, varnames = c("Currency1", "Currency2"))

ggplot(melted_correlation_matrix, aes(x = Currency1, y = Currency2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Removes the x-axis item names
    axis.text.y = element_blank(),  # Removes the y-axis item names
    axis.ticks = element_blank()  # Keeping the axis ticks
  ) +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "")





# Filter out rows where either 'get' or 'pay' is "Mirror of Kalandra" or "Mirror Shard"
currency_data <- currency_data %>%
  filter(!(get %in% c("Operative's Scouting Report") | pay %in% c("Operative's Scouting Report")))



# Convert Date to numeric format (e.g., number of days since the earliest date)
currency_data$date_numeric <- as.numeric(currency_data$date - min(currency_data$date))

correlation_results <- currency_data %>%
  group_by(get) %>%
  summarize(correlation = cor(date_numeric, value, use = "complete.obs"))

print(correlation_results)

# Create a scatter plot of date vs. value for each currency with color but no item names on axes
ggplot(currency_data, aes(x = date, y = value, color = get)) +
  geom_point() +
  labs(title = "Correlation between Date and Currency Value", x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())




# Find the last date in your dataset
last_date <- max(aggregated_data$date)

# Get the last value for each currency
last_values <- aggregated_data %>%
  filter(date == last_date) %>%
  arrange(desc(value))

# Order the wide_data columns based on the last values
ordered_columns <- c("date", last_values$get)
wide_data_ordered <- wide_data[ordered_columns]


currency_data <- currency_data %>%
  group_by(get) %>%
  arrange(date) %>%
  mutate(daily_change = (value - lag(value)) / lag(value) * 100) %>%
  ungroup()

fluctuation_measure <- currency_data %>%
  filter(!is.na(daily_change)) %>%
  group_by(get) %>%
  summarize(fluctuation = sd(daily_change, na.rm = TRUE)) %>%
  ungroup()

top_items <- fluctuation_measure %>%
  arrange(desc(fluctuation)) %>%
  head(30) %>%
  pull(get)

filtered_data <- currency_data %>%
  filter(get %in% top_items)

aggregated_data <- filtered_data %>%
  group_by(date, get) %>%
  summarize(value = mean(value))

wide_data <- aggregated_data %>%
  spread(key = get, value = value)

wide_data$date <- as.factor(wide_data$date)

# Melting the ordered data into a long format for ggplot
melted_ordered_data <- melt(wide_data_ordered, id.vars = 'date')

ggplot(melted_ordered_data, aes(x = date, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", name = "Value") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Removes the x-axis labels
    axis.ticks.x = element_blank()  # Removes the x-axis ticks
  ) +
  labs(x = "Date", y = "Currency Type", title = "Top 30 Fluctuating Currency Values Over Time")



# Filter data for the top 30 items and calculate the average daily change rate
aggregated_change_rate_data <- filtered_data %>%
  group_by(date, get) %>%
  summarize(average_daily_change = mean(daily_change, na.rm = TRUE))

# Spread the data to wide format for the heatmap
wide_change_data <- aggregated_change_rate_data %>%
  spread(key = get, value = average_daily_change)

# Convert dates to a factor
wide_change_data$date <- as.factor(wide_change_data$date)

# Melting the data into a long format for ggplot
melted_change_data <- melt(wide_change_data, id.vars = 'date')


# Ensure that 'last_values$get' contains the currency names in the desired order
ordered_currencies <- last_values$get

# Filter 'wide_change_data' to include only the top currencies and reorder columns
ordered_change_data <- wide_change_data %>%
  select(c("date", intersect(ordered_currencies, names(wide_change_data))))

# Convert dates to a factor (if not already done)
ordered_change_data$date <- as.factor(ordered_change_data$date)

# Melt the ordered data for plotting
melted_ordered_change_data <- melt(ordered_change_data, id.vars = 'date')

# Plotting the heatmap with ordered currencies
ggplot(melted_ordered_change_data, aes(x = date, y = variable, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "%") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(x = "Date", y = "Currency Type", title = "Daily Change Rate for Top 30 Fluctuating Currencies")




