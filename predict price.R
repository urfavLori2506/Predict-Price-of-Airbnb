# Load required libraries
library(lmtest)
library(tidyverse)
library(car)
library(plotly)
library(ggplot2)
library(naniar)
library(leaflet)
library(htmltools)
library(choroplethr)
library(dplyr)
library(choroplethrMaps)
library(gridExtra)
library(ggmap)
library(htmlwidgets)
library(mapview)
library(dplyr)
library(plotly)
library(devtools)
library(kableExtra)
library(here)
library(janitor)
library(Hmisc)
library(corrplot)
library(jtools)
library(caret)
library(MASS)

1. Data Processing
# Importing data from your local path
data_train <- read.csv("D:/Uni/model1/processed_data_train.csv", header = TRUE)
str(data_train)
# Data Cleaning - removing unnecessary columns
data_train <- data_train %>%
  select(-last_scraped, -thumbnail_url, -host_picture_url, -medium_url, 
         -picture_url, -xl_picture_url, -host_thumbnail_url, -host_url,
         -scrape_id, -experiences_offered, -neighborhood_overview, 
         -host_about, -host_id, -host_verifications, -host_has_profile_pic, 
         -host_identity_verified, -calendar_last_scraped, -license, 
         -require_guest_profile_picture, -require_guest_phone_verification, 
         -summary, -notes, -space, -description, -transit, 
         -host_neighbourhood, -market, -country_code, -is_location_exact)

# Convert data types for specific columns
data_train <- data_train %>%
  mutate(
    price = as.integer(gsub("[$,]", "", price)),
    weekly_price = as.integer(gsub("[$,]", "", weekly_price)),
    monthly_price = as.integer(gsub("[$,]", "", monthly_price)),
    cleaning_fee = as.integer(gsub("[$,]", "", cleaning_fee)),
    extra_people = as.integer(gsub("[$,]", "", extra_people)),
    security_deposit = as.integer(gsub("[$,]", "", security_deposit)),
    host_response_time = as.factor(host_response_time),
    host_is_superhost = as.factor(host_is_superhost),
    neighbourhood_cleansed = as.factor(neighbourhood_cleansed),
    property_type = as.factor(property_type),
    room_type = as.factor(room_type),
    bed_type = as.factor(bed_type),
    calendar_updated = as.factor(calendar_updated),
    cancellation_policy = as.factor(cancellation_policy),
    instant_bookable = as.factor(instant_bookable),
    bathrooms = as.numeric(bathrooms),
    id = as.character(id)
  )

# Change host response rate and acceptance rate to numeric
data_train <- data_train %>%
  mutate(
    host_response_rate = as.numeric(gsub("%", "", host_response_rate)) / 100,
    host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)) / 100
  )

# Replace rare property types with "Other"
data_train$property_type <- data_train$property_type %>%
  fct_collapse(Other = c("Boat", "Bungalow", "Dorm", "Chalet", "Treehouse", "Yurt", "Camper/RV"))

# Add log-transformed columns
data_train$log_price <- log(data_train$price)
data_train$log_acc <- log(data_train$accommodates)

ggplot(data_train, aes(price)) + geom_bar() + theme_bw()
ggplot(data_train, aes(log_price)) + geom_bar() + theme_bw()

ggplot(data_train, aes(accommodates)) + geom_bar() + theme_bw()
ggplot(data_train, aes(log_acc)) + geom_bar() + theme_bw()
# Replace missing values with mean for numeric columns
data_train <- data_train %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Select numeric columns to visualize outliers
numeric_columns <- data_train[, sapply(data_train, is.numeric)]

# Function to detect outliers for a single column
detect_outliers <- function(column) {
  q1 <- quantile(column, 0.25, na.rm = TRUE)
  q3 <- quantile(column, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  outliers <- sum(column < lower_bound | column > upper_bound, na.rm = TRUE)
  return(outliers)
}

# Check for outliers in each numeric column before handling
outlier_summary_before <- sapply(numeric_columns, detect_outliers)
outlier_summary_before <- outlier_summary_before[outlier_summary_before > 0] # Filter columns with outliers

# Print the summary of outliers before handling
print("Columns with outliers and their counts (before handling):")
print(outlier_summary_before)

# Function to handle outliers for all numeric columns
handle_outliers <- function(column) {
  q1 <- quantile(column, 0.25, na.rm = TRUE)
  q3 <- quantile(column, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  column <- ifelse(column < lower_bound, lower_bound, 
                   ifelse(column > upper_bound, upper_bound, column))
  return(column)
}

# Apply the function to all numeric columns
numeric_columns <- as.data.frame(lapply(numeric_columns, handle_outliers))

# Combine numeric and non-numeric columns back together
non_numeric_columns <- data_train[, !sapply(data_train, is.numeric)]
cleaned_data <- cbind(non_numeric_columns, numeric_columns)

# Check for outliers in each numeric column after handling
outlier_summary_after <- sapply(numeric_columns, detect_outliers)
outlier_summary_after <- outlier_summary_after[outlier_summary_after > 0] # Filter columns with remaining outliers

# Print the summary of outliers after handling
print("Columns with outliers and their counts (after handling):")
print(outlier_summary_after)

# Visualize before and after handling outliers
par(mfrow = c(2, 1)) # Create a 2-row layout for plots

# Before handling outliers
boxplot(data_train[, sapply(data_train, is.numeric)],
        main = "Boxplot Before Handling Outliers",
        col = "lightblue",
        las = 2) # Rotate x-axis labels

# After handling outliers
boxplot(numeric_columns,
        main = "Boxplot After Handling Outliers",
        col = "lightgreen",
        las = 2) # Rotate x-axis labels

#Model Selection using AIC/ BIC
# Load necessary library
library(broom)

# Model 1
model1 <- lm(formula = price ~ room_type, data = data_train)
model1 <- broom::glance(model1)
print(model1)
# Model 2
model2 <- lm(formula = price ~ room_type + accommodates, data = data_train)
model2 <- broom::glance(model2)
print(model2)
# Model 3
model3 <- lm(formula = price ~ room_type + accommodates + bedrooms, data = data_train)
model3 <- broom::glance(model3)
print(model3)
# Model 4
model4 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms, data = data_train)
model4 <- broom::glance(model4)
print(model4)

# Model 5
model5 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type, data = data_train)
model5 <- broom::glance(model5)
print(model5)

# Model 6
model6 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost, data = data_train)
model6 <- broom::glance(model6)
print(model6)

# Model 7
model7 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee, data = data_train)
model7 <- broom::glance(model7)
print(model7)

# Model 8
model8 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee + cancellation_policy, data = data_train)
model8 <- broom::glance(model8)
print(model8)

# Model 9
model9 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee + cancellation_policy + extra_people, data = data_train)
model9 <- broom::glance(model9)
print(model9)

# Model 10
model10 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit, data = data_train)
model10 <- broom::glance(model10)
print(model10)

# Model 11
model11 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + instant_bookable, data = data_train)
model11 <- broom::glance(model11)
print(model11)

# Model 12
model12 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + instant_bookable + maximum_nights, data = data_train)
model12 <- broom::glance(model12)
print(model12)

# Model 13
model13 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + instant_bookable + maximum_nights + minimum_nights, data = data_train)
model13 <- broom::glance(model13)
print(model13)

# Model 14
model14 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + instant_bookable + maximum_nights + minimum_nights + bed_type, data = data_train)
model14 <- broom::glance(model14)
print(model14)

# Model 15
model15 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + instant_bookable + maximum_nights + minimum_nights + bed_type + latitude, data = data_train)
model15 <- broom::glance(model15)
print(model15)

# Model 16
model16 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + instant_bookable + maximum_nights + minimum_nights + bed_type + latitude + longitude, data = data_train)
model16 <- broom::glance(model16)
print(model16)

# Model 17
model17 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type +
                host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit +
                instant_bookable + maximum_nights + minimum_nights + bed_type + latitude +
                longitude + number_of_reviews, data = data_train)
model17 <- broom::glance(model17)
print(model17)

# Model 18
model18 <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type +
                host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit +
                instant_bookable + maximum_nights + minimum_nights + bed_type + latitude + longitude +
                number_of_reviews + availability_365, data = data_train)
model18 <- broom::glance(model18)
print(model18)

# Lưu trữ kết quả của các model vào danh sách
model_list <- list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
                   model11, model12, model13, model14, model15, model16, model17, model18)

# Tạo một bảng tổng hợp các kết quả từ danh sách các model
summary_table <- do.call(rbind, model_list)

# Hiển thị bảng tổng hợp
print(summary_table)

2. Mutiple Linear Regression
2.1.Feature Engineering and Transformation
# Describe the initial price variables 
ggplot(data1, aes(x = price)) +
  geom_histogram(binwidth = 10, fill = "black", color = "white") +
  labs(title = "Distribution of Price", x = "price", y = "count") +
  theme_minimal()

# Creating log-transformed variables for price and accommodates
data1$log_price <- log(data1$price)
data1$log_acc <- log(data1$accommodates)

# Describe the log_price variables 
ggplot(data1, aes(x = log_price)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = "white") +
  labs(title = "Distribution of log_price",
       x = "log_price",
       y = "Count") +
  theme_minimal()

#Describe the accommodates and log accommodates variable --- Transformations
ggplot(data1, aes(accommodates)) + geom_bar() + theme_bw()
ggplot(data1, aes(log_acc)) + geom_bar() + theme_bw()


#Changing data type of categorical variables to factor
data1$room_type <- as.factor(data1$room_type)
data1$property_type <- as.factor(data1$property_type)
data1$host_is_superhost <- as.factor(data1$host_is_superhost)
data1$cancellation_policy <- as.factor(data1$cancellation_policy)
data1$instant_bookable <- as.factor(data1$instant_bookable)
data1$bed_type <- as.factor(data1$bed_type)

View(data1)

# Select columns containing only numeric data to calculate correlation
numeric_columns <- sapply(data1, is.numeric)
numeric_data <- data1[, numeric_columns]

# calculate correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs") 

# Show the correlation matrix table
print(correlation_matrix)


# Làm đẹp ma trận bằng gói kableExtra (nếu cần)
if (!require(kableExtra)) install.packages("kableExtra")
library(kableExtra)

# Hiển thị ma trận tương quan với đường kẻ đẹp
kable(correlation_matrix, digits = 2, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

#Delete variables that are not of my interest
data_train <- data1[c("price","accommodates","log_price", "room_type","log_acc","accommodates","bedrooms", "bathrooms", 
                      "property_type", "host_is_superhost","cleaning_fee", "cancellation_policy","extra_people", 
                      "security_deposit", "instant_bookable","maximum_nights","minimum_nights", "bed_type", 
                      "latitude", "longitude", "number_of_reviews",
                      "availability_365")]
view(data_train)
str(data_train)
summary(data_train)

#2.2. Run the multiple linear regression model

#Model1: price-accommodate
lm_model_no_log <- lm(formula = price ~ room_type + accommodates + bedrooms + bathrooms + property_type + 
                        host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + 
                        instant_bookable + maximum_nights + minimum_nights + bed_type + latitude + longitude + 
                        number_of_reviews + availability_365, data = data_train)
summary(lm_model_no_log)
plot(lm_model_no_log)

#Model2: log price-accommodate
lm_model_with_log_price <- lm(formula = log_price ~ room_type + accommodates + bedrooms + bathrooms + property_type + 
                                host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + 
                                instant_bookable + maximum_nights + minimum_nights + bed_type + latitude + longitude + 
                                number_of_reviews + availability_365, data = data_train)
summary(lm_model_with_log_price)
plot(lm_model_with_log_price)

#Model3: log price - log accommodate
lm_model <- lm(formula = log_price ~ room_type + log_acc + bedrooms + bathrooms + property_type + 
                 host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + 
                 instant_bookable + maximum_nights + minimum_nights + bed_type + latitude + longitude + 
                 number_of_reviews + availability_365, data = data_train)
summary(lm_model)
plot(lm_model)

# Show the comparison table 
# Extract metrics from models
model_comparison <- data.frame(
  Model = c("lm_model_no_log", "lm_model_with_log_price", "lm_model"),
  R_squared = c(
    summary(lm_model_no_log)$r.squared,
    summary(lm_model_with_log_price)$r.squared,
    summary(lm_model)$r.squared
  ),
  Adjusted_R_squared = c(
    summary(lm_model_no_log)$adj.r.squared,
    summary(lm_model_with_log_price)$adj.r.squared,
    summary(lm_model)$adj.r.squared
  ),  
  RMSE = c(
    sqrt(mean(residuals(lm_model_no_log)^2)),
    sqrt(mean(residuals(lm_model_with_log_price)^2)),
    sqrt(mean(residuals(lm_model)^2))
  ),
  Residual_Std_Error = c(
    summary(lm_model_no_log)$sigma,
    summary(lm_model_with_log_price)$sigma,
    summary(lm_model)$sigma
  ),
  
  F_statistic = c(
    summary(lm_model_no_log)$fstatistic[1],
    summary(lm_model_with_log_price)$fstatistic[1],
    summary(lm_model)$fstatistic[1]
  )
)
# Load library
library(knitr)

# Create comparision table với kable()
model_comparison %>%
  kable(format = "pipe", align = "c", caption = "Comparison of Regression Models")

#Plot the fitted line
fitted_values <- fitted(lm_model)
actual_values <- data_train$log_price

plot(actual_values, fitted_values,
     xlab = "Actual log(price)",
     ylab = "Fitted log(price)",
     main = "Actual vs Fitted Values",
     pch = 20, col = "blue")
abline(0, 1, col = "red")
plot(lm_model)

#2.3 Interaction model
# Interaction Model 1: room_type * log_acc
lm_model_interaction1 <- lm(formula = log_price ~ room_type * log_acc + bedrooms + bathrooms + property_type + 
                              host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + 
                              instant_bookable + maximum_nights + minimum_nights + bed_type + latitude + longitude + 
                              number_of_reviews + availability_365, data = data_train)
# Interaction Model 2: bedrooms * bathrooms
lm_model_interaction2 <- lm(formula = log_price ~ room_type + log_acc + bedrooms * bathrooms + property_type + 
                              host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + 
                              instant_bookable + maximum_nights + minimum_nights + bed_type + latitude + longitude + 
                              number_of_reviews + availability_365, data = data_train)

# Interaction Model 3: latitude * longitude
lm_model_interaction3 <- lm(formula = log_price ~ room_type + log_acc + bedrooms + bathrooms + property_type + 
                              host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + 
                              instant_bookable + maximum_nights + minimum_nights + bed_type + latitude * longitude + 
                              number_of_reviews + availability_365, data = data_train)
# Calculate R-squared và RMSE for each model
model_comparison1 <- data.frame(
  Model = c( "lm_model", "lm_model_interaction1","lm_model_interaction2", "lm_model_interaction3" ),
  R_squared = c(
    summary(lm_model)$r.squared,
    summary(lm_model_interaction1)$r.squared,
    summary(lm_model_interaction2)$r.squared,
    summary(lm_model_interaction3)$r.squared
  ),
  RMSE = c(
    sqrt(mean(residuals(lm_model)^2)),
    sqrt(mean(residuals(lm_model_interaction1)^2)),
    sqrt(mean(residuals(lm_model_interaction2)^2)),
    sqrt(mean(residuals(lm_model_interaction3)^2))
  )
)
# Create comparision table với kable()
model_comparison1 %>%
  kable(format = "pipe", align = "c", caption = "Comparison of Interaction Models")

#3. Asumption
Using log transformed Price and Accommodates 
lm_model <- lm(formula = log_price ~ room_type + log_acc + bedrooms + bathrooms + property_type + host_is_superhost + cleaning_fee + cancellation_policy + extra_people + security_deposit + instant_bookable + maximum_nights + minimum_nights + bed_type + latitude + longitude + number_of_reviews + availability_365, data = train)

#3.1. Testing Collinearity
set.seed(891)
data.corr <- data_train %>% filter(!is.na(log_acc)) %>% filter(!is.na(bathrooms)) %>% filter(!is.na(bedrooms)) %>% filter(!is.na(availability_365)) %>% filter(!is.na(cleaning_fee)) %>% filter(!is.na(extra_people)) %>% filter(!is.na(security_deposit)) %>% filter(!is.na(maximum_nights)) %>% filter(!is.na(minimum_nights)) %>% filter(!is.na(latitude)) %>% filter(!is.na(longitude)) %>% filter(!is.na(number_of_reviews)) %>%
  select(log_price, price, bedrooms, bathrooms, log_acc, availability_365, cleaning_fee, security_deposit, maximum_nights, minimum_nights, extra_people, latitude, longitude, number_of_reviews)
kable(cor(data.corr)) %>% kable_styling()

There is some correlation between the variables, but not strong enough to worry about multicollinearity.

# Calculate VIF
vif_values <- vif(lm_model)
print(vif_values)

#3.2. Assumptions
#1. Check for Linearity
# Plot residuals vs. fitted values
plot(lm_model$fitted.values, resid(lm_model), 
     xlab = "Fitted values", 
     ylab = "Residuals", 
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")  # Horizontal line at 0

#2. Check for Independence
# Durbin-Watson Test
durbinWatsonTest(lm_model)  # Results indicate whether residuals are independent

# Generate residuals from the linear model
residuals <- resid(lm_model)

# Plot residuals against the order of observations
plot(residuals, 
     type = "o",  # Line and point plot for clarity
     xlab = "Observation Order", 
     ylab = "Residuals", 
     main = "Residuals vs Observation Order")
abline(h = 0, col = "red", lty = 2)  # Add a horizontal reference line at 0

#3. Check for Normality
# Q-Q Plot
qqnorm(resid(lm_model), main = "Q-Q Plot of Residuals")
qqline(resid(lm_model), col = "red")

# Shapiro-Wilk Test for Normality
shapiro.test(resid(lm_model))  # p-value > 0.05 indicates normality

#4. Check for Equal Variance (Homoscedasticity)
# Residuals vs Fitted values plot
dev.off()

par(mar = c(5, 5, 4, 2)) 
plot(lm_model$fitted.values, abs(resid(lm_model)), 
     xlab = "Fitted values", 
     ylab = "Absolute Residuals", 
     main = "Scale-Location Plot")
abline(h = 0, col = "red")

# Breusch-Pagan Test
bptest(lm_model) 

#3.3. Assumption Handling
###Method
## Polynomial
lm_model_poly <- lm(log_price ~ room_type * log_acc + I(log_acc^2) + bedrooms + bathrooms + 
                      property_type + host_is_superhost + cleaning_fee + cancellation_policy + 
                      extra_people + security_deposit + instant_bookable + maximum_nights + 
                      minimum_nights + bed_type + latitude + longitude + number_of_reviews + 
                      availability_365, data = data_train)

## Cook's distance
#find cause of non-normality
plot(lm_model, which = 4)

# Refit the model without the outliers
lm_model_cook <- lm(log_price ~ room_type + log_acc + bedrooms + bathrooms + property_type + 
                      host_is_superhost + cleaning_fee + cancellation_policy + extra_people +
                      security_deposit + instant_bookable + maximum_nights + minimum_nights + 
                      bed_type + latitude + longitude + number_of_reviews + availability_365, 
                    data = data_train[-c(1298, 2185, 2618), ])

## Box-Cox transformation
boxcox_result <- boxcox(lm_model, lambda = seq(-2, 2, 0.1))
# Find Optimal Lambda
optimal_lambda <- boxcox_result$x[which.max(boxcox_result$y)]
cat("Optimal Lambda:", optimal_lambda)
if (optimal_lambda == 0) {
  # Log Transformation
  data_train$transformed_price <- log(data_train$price)
} else {
  # Power Transformation
  data_train$transformed_price <- (data_train$price^optimal_lambda - 1) / optimal_lambda
}

#Box-cox transformation
lm_model_boxcox <- lm(transformed_price ~ room_type + log_acc + bedrooms + bathrooms + 
                        property_type + host_is_superhost + cleaning_fee + 
                        cancellation_policy + extra_people + security_deposit + 
                        instant_bookable + maximum_nights + minimum_nights + bed_type + 
                        latitude + longitude + number_of_reviews + availability_365, 
                      data = data_train)

## Log independence transform
# Log-transforming numerical variables with handling for invalid values
log_transform_vars <- c("accommodates", "bedrooms", "bathrooms", 
                        "cleaning_fee", "extra_people", "security_deposit", 
                        "latitude", "longitude", "number_of_reviews")

for (var in log_transform_vars) {
  # Ensure values are numeric and non-negative
  if (!is.numeric(data_train[[var]])) {
    data_train[[var]] <- as.numeric(data_train[[var]])
  }
  
  # Replace negative values with NA
  data_train[[var]][data_train[[var]] < 0] <- NA
  
  # Log-transform and handle zeros by adding a small constant
  data_train[[paste0("log_", var)]] <- ifelse(data_train[[var]] > 0,
                                              log(data_train[[var]] + 1e-5),
                                              NA)
}

# Check for NaN or NA values in the transformed variables and handle them
for (var in log_transform_vars) {
  transformed_var <- paste0("log_", var)
  if (any(is.na(data_train[[transformed_var]]))) {
    # Replace NaNs with median of the column (ignoring NA)
    data_train[[transformed_var]] <- ifelse(is.na(data_train[[transformed_var]]), 
                                            median(data_train[[transformed_var]], na.rm = TRUE), 
                                            data_train[[transformed_var]])
  }
}

# Verify the structure after corrections
str(data_train)

# Log Independence 
lm_model_log <- lm(log_price ~ room_type + log_accommodates + log_bedrooms + 
                     log_bathrooms + property_type + host_is_superhost + 
                     log_cleaning_fee + cancellation_policy + log_extra_people + 
                     log_security_deposit + instant_bookable + maximum_nights + 
                     minimum_nights + bed_type + log_latitude + 
                     log_number_of_reviews + availability_365, 
                   data = data_train)

### Normality after Transformation
# Function to generate Q-Q plot for a model
generate_qq_plot <- function(model, title) {
  residuals <- residuals(model)  # Extract residuals
  ggplot(data = data.frame(residuals), aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line(color = "blue") +
    labs(title = title, x = "Theoretical Quantiles", y = "Sample Quantiles") +
    theme_minimal()
}
# Generate Q-Q plots
plot_origin <- generate_qq_plot(lm_model, "Model_Orginal")
plot_log <- generate_qq_plot(lm_model_log, "Model_Log Independent")
plot_boxcox <- generate_qq_plot(lm_model_boxcox, "Model_Boxcox")
plot_poly <- generate_qq_plot(lm_model_poly, "Model_Poly")

# Combine plots in a grid layout
grid.arrange(plot_origin, plot_log, plot_boxcox, plot_poly, 
             ncol = 2, top = "Figure: Q-Q Plot Comparison for Multiple Models")

shapiro.test(resid(lm_model_log))
shapiro.test(resid(lm_model_boxcox))
shapiro.test(resid(lm_model_poly))

### Equal Variance after Transformation
# Function to generate Scale-Location plot for a model
generate_scale_location_plot <- function(model, title) {
  fitted_vals <- fitted(model)  # Extract fitted values
  residuals_vals <- residuals(model)  # Extract residuals
  sqrt_std_residuals <- sqrt(abs(residuals_vals))  # Calculate sqrt of absolute standardized residuals
  
  ggplot(data = data.frame(Fitted = fitted_vals, SqrtStdResiduals = sqrt_std_residuals), 
         aes(x = Fitted, y = SqrtStdResiduals)) +
    geom_point(size = 2, alpha = 0.6) +
    geom_smooth(method = "loess", color = "blue", se = FALSE) +
    labs(title = title, x = "Fitted Values", y = "√|Standardized Residuals|") +
    theme_minimal()
}

# Generate Scale-Location plots for each model
plot_origin <- generate_scale_location_plot(lm_model, "Model_Origin")
plot_log <- generate_scale_location_plot(lm_model_log, "Model_Log Independence")
plot_boxcox <- generate_scale_location_plot(lm_model_boxcox, "Model_Boxcox")
plot_poly <- generate_scale_location_plot(lm_model_poly, "Model_Poly")

# Combine the plots in a grid layout
grid.arrange(plot_origin, plot_log, plot_boxcox, plot_poly, 
             ncol = 2, top = "Scale-Location Plots for Model Comparison")

bptest(lm_model_log) 
bptest(lm_model_boxcox) 
bptest(lm_model_poly) 

### Recheck new model

# Fit the corrected model
lm_model_corrected <- lm(formula = log_price ~ room_type + log_acc + I(log_acc^2) +
                           bedrooms + bathrooms + property_type + host_is_superhost +
                           log_cleaning_fee + cancellation_policy + log_extra_people +
                           log_security_deposit + instant_bookable + maximum_nights +
                           minimum_nights + bed_type +
                           number_of_reviews + availability_365,
                         data = data_train
)
#Calculate RMSE
RMSE <- sqrt(mean(residuals(lm_model_corrected)^2))
cat("RMSE = ", RMSE)





`