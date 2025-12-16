# Airbnb Pricing Regression Analysis
# SDS 301 Modern Regression Analysis: Final Project

# Add user library to path if packages are installed there
userLib <- file.path(Sys.getenv("USERPROFILE"), "Documents", "R", "win-library", 
                     paste(R.version$major, R.version$minor, sep = "."))
if (dir.exists(userLib)) {
  .libPaths(c(userLib, .libPaths()))
}

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(car)
library(MASS)
library(broom)
library(gridExtra)

# Check and install glmnet for Ridge/Lasso regression
if (!require(glmnet, quietly = TRUE)) {
  install.packages("glmnet", repos = "https://cran.rstudio.com/")
  library(glmnet)
}

# 1. DATA LOADING AND PREPARATION

cat("Loading data...\n")

# Load main dataset
listings <- read_csv("data/listings_gz.csv", show_col_types = FALSE)
cat("Loaded rows:", nrow(listings), "\n")
cat("Loaded columns:", ncol(listings), "\n")

# Load additional datasets
cat("\nLoading additional data...\n")

cat("Loading reviews.csv...\n")
reviews <- read_csv("data/reviews.csv", show_col_types = FALSE)
cat("Loaded reviews:", nrow(reviews), "\n")

cat("Loading neighbourhoods.csv...\n")
neighbourhoods <- read_csv("data/neighbourhoods.csv", show_col_types = FALSE)
cat("Loaded neighbourhoods:", nrow(neighbourhoods), "\n")

cat("\nData structure:\n")
str(listings, give.attr = FALSE)

cat("\nFirst rows:\n")
head(listings)

# 2. DATA PREPROCESSING

cat("\nData preprocessing...\n")

# Convert price from "$132.00" format to numeric
listings$price_numeric <- as.numeric(gsub("[$,]", "", listings$price))
cat("Missing prices:", sum(is.na(listings$price_numeric)), "\n")

# Remove rows with missing prices
listings_clean <- listings %>% 
  filter(!is.na(price_numeric) & price_numeric > 0)

cat("Rows after removing missing values:", nrow(listings_clean), "\n")

cat("\nPrice statistics:\n")
summary(listings_clean$price_numeric)

# 2.5 OUTLIER TREATMENT

cat("\nOutlier treatment...\n")

# Function to remove outliers using IQR method
remove_outliers_iqr <- function(data, columns, factor = 1.5) {
  data_clean <- data
  total_before <- nrow(data_clean)
  
  # Create vector of row indices to keep
  keep_rows <- rep(TRUE, nrow(data_clean))
  
  for (col in columns) {
    if (col %in% names(data_clean)) {
      Q1 <- quantile(data_clean[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data_clean[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      if (IQR > 0) {
        lower_bound <- Q1 - factor * IQR
        upper_bound <- Q3 + factor * IQR
        
        col_values <- data_clean[[col]]
        col_keep <- is.na(col_values) | (col_values >= lower_bound & col_values <= upper_bound)
        outliers_count <- sum(!col_keep, na.rm = TRUE)
        
        keep_rows <- keep_rows & col_keep
        
        cat(sprintf("  %s: found %d outliers (bounds: [%.2f, %.2f])\n", 
                    col, outliers_count, lower_bound, upper_bound))
      } else {
        cat(sprintf("  %s: IQR = 0, skipping\n", col))
      }
    }
  }
  
  # Apply filtering once
  data_clean <- data_clean[keep_rows, ]
  total_removed <- total_before - nrow(data_clean)
  
  cat(sprintf("\nTotal rows removed with outliers: %d (from %d, remaining %d)\n", 
              total_removed, total_before, nrow(data_clean)))
  return(data_clean)
}

# Define numeric columns for outlier treatment
numeric_cols_for_outliers <- c("price_numeric", "accommodates", "bedrooms", "beds", 
                                "bathrooms", "minimum_nights", "number_of_reviews",
                                "reviews_per_month", "calculated_host_listings_count",
                                "availability_365")

# Remove outliers (using factor 2.0 to keep more data)
cat("\nApplying IQR method for outlier removal (factor=2.0)...\n")
listings_clean <- remove_outliers_iqr(listings_clean, numeric_cols_for_outliers, factor = 2.0)

cat("Rows after outlier treatment:", nrow(listings_clean), "\n")
cat("\nPrice statistics after outlier treatment:\n")
summary(listings_clean$price_numeric)

# 3. EXPLORATORY DATA ANALYSIS (EDA)

cat("\nExploratory Data Analysis...\n")

# Create directories for results
if (!dir.exists("results")) {
  dir.create("results")
}
if (!dir.exists("results/plots")) {
  dir.create("results/plots")
}

# 3.1 Price distribution
p1 <- ggplot(listings_clean, aes(x = price_numeric)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Price Distribution",
       x = "Price (EUR)", y = "Frequency") +
  theme_minimal()

p2 <- ggplot(listings_clean, aes(x = log(price_numeric))) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Log Price Distribution",
       x = "log(Price)", y = "Frequency") +
  theme_minimal()

ggsave("results/plots/01_price_distribution.png", 
       grid.arrange(p1, p2, ncol = 2), 
       width = 12, height = 5)

# 3.2 Price by room type
p3 <- ggplot(listings_clean, aes(x = room_type, y = price_numeric)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Price by Room Type",
       x = "Room Type", y = "Price (EUR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("results/plots/02_price_by_room_type.png", p3, width = 10, height = 6)

# 3.3 Price by neighbourhood
top_neighbourhoods <- listings_clean %>%
  count(neighbourhood_cleansed, sort = TRUE) %>%
  head(10) %>%
  pull(neighbourhood_cleansed)

listings_top_neigh <- listings_clean %>%
  filter(neighbourhood_cleansed %in% top_neighbourhoods)

p4 <- ggplot(listings_top_neigh, 
             aes(x = reorder(neighbourhood_cleansed, price_numeric, median), 
                 y = price_numeric)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Price by Neighbourhood (Top 10)",
       x = "Neighbourhood", y = "Price (EUR)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

ggsave("results/plots/03_price_by_neighbourhood.png", p4, width = 10, height = 8)

# 3.4 Numeric variables
cat("\nNumeric variables:\n")
numeric_vars <- listings_clean %>%
  dplyr::select(price_numeric, accommodates, bathrooms, bedrooms, beds, 
         minimum_nights, number_of_reviews, reviews_per_month,
         calculated_host_listings_count, availability_365,
         review_scores_rating)

cat("Number of numeric variables:", ncol(numeric_vars), "\n")
print(summary(numeric_vars))

# 3.5 Correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")
png("results/plots/04_correlation_matrix.png", width = 1000, height = 1000)
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black")
dev.off()

# 3.6 Scatter plots
p5 <- ggplot(listings_clean, aes(x = accommodates, y = price_numeric)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Price vs Accommodates",
       x = "Accommodates", y = "Price (EUR)") +
  theme_minimal()

p6 <- ggplot(listings_clean, aes(x = bedrooms, y = price_numeric)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Price vs Bedrooms",
       x = "Bedrooms", y = "Price (EUR)") +
  theme_minimal()

p7 <- ggplot(listings_clean, aes(x = number_of_reviews, y = price_numeric)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Price vs Number of Reviews",
       x = "Number of Reviews", y = "Price (EUR)") +
  theme_minimal()

p8 <- ggplot(listings_clean, aes(x = review_scores_rating, y = price_numeric)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Price vs Rating",
       x = "Rating", y = "Price (EUR)") +
  theme_minimal()

ggsave("results/plots/05_scatterplots.png", 
       grid.arrange(p5, p6, p7, p8, ncol = 2), 
       width = 12, height = 10)

cat("\nEDA completed. Plots saved to results/plots/\n")

# 3.7 MERGING WITH ADDITIONAL DATA

cat("\nMerging with additional data...\n")

cat("Processing review data...\n")
reviews$date <- as.Date(reviews$date)
current_date <- max(reviews$date, na.rm = TRUE)

# Create aggregated features from reviews
reviews_features <- reviews %>%
  group_by(listing_id) %>%
  summarise(
    total_reviews_count = n(),
    last_review_date = max(date, na.rm = TRUE),
    days_since_last_review = as.numeric(current_date - max(date, na.rm = TRUE)),
    reviews_last_30_days = sum(date >= (current_date - 30), na.rm = TRUE),
    reviews_last_90_days = sum(date >= (current_date - 90), na.rm = TRUE),
    reviews_last_180_days = sum(date >= (current_date - 180), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    days_since_last_review = ifelse(is.infinite(days_since_last_review), NA, days_since_last_review),
    has_recent_reviews = reviews_last_30_days > 0,
    review_activity_score = (reviews_last_30_days * 3 + reviews_last_90_days * 2 + reviews_last_180_days) / 6
  )

cat("Created features from reviews:", ncol(reviews_features) - 1, "\n")

# Merge with main dataset
listings_clean <- listings_clean %>%
  left_join(reviews_features, by = c("id" = "listing_id"))

cat("Merged with review data. Rows:", nrow(listings_clean), "\n")

# Merge with neighbourhood data if available
if (ncol(neighbourhoods) > 2) {
  listings_clean <- listings_clean %>%
    left_join(neighbourhoods, by = "neighbourhood_cleansed")
  cat("Merged with neighbourhood data.\n")
}

# 3.8 FEATURE ENGINEERING

cat("\nFeature engineering...\n")

# Host experience in years
if ("host_since" %in% names(listings_clean)) {
  listings_clean$host_since_date <- as.Date(listings_clean$host_since)
  current_date_host <- max(listings_clean$host_since_date, na.rm = TRUE)
  listings_clean$host_experience_days <- as.numeric(current_date_host - listings_clean$host_since_date)
  listings_clean$host_experience_years <- listings_clean$host_experience_days / 365.25
  cat("Created feature: host_experience_years\n")
}

# Number of amenities
if ("amenities" %in% names(listings_clean)) {
  listings_clean$amenities_count <- sapply(listings_clean$amenities, function(x) {
    if (is.na(x)) return(0)
    count <- length(strsplit(gsub("[\\[\\]\"]", "", x), ",")[[1]])
    return(max(0, count))
  })
  cat("Created feature: amenities_count\n")
}

# Distance to Amsterdam center (coordinates: 52.3676, 4.9041)
if ("latitude" %in% names(listings_clean) && "longitude" %in% names(listings_clean)) {
  center_lat <- 52.3676
  center_lon <- 4.9041
  
  listings_clean$distance_to_center <- sqrt(
    (listings_clean$latitude - center_lat)^2 + 
    (listings_clean$longitude - center_lon)^2
  ) * 111
  cat("Created feature: distance_to_center\n")
}

# Convert host_response_rate and host_acceptance_rate to numeric
if ("host_response_rate" %in% names(listings_clean)) {
  listings_clean$host_response_rate_num <- as.numeric(gsub("%", "", listings_clean$host_response_rate)) / 100
  listings_clean$host_response_rate_num[is.na(listings_clean$host_response_rate_num)] <- median(listings_clean$host_response_rate_num, na.rm = TRUE)
  cat("Created feature: host_response_rate_num\n")
}

if ("host_acceptance_rate" %in% names(listings_clean)) {
  listings_clean$host_acceptance_rate_num <- as.numeric(gsub("%", "", listings_clean$host_acceptance_rate)) / 100
  listings_clean$host_acceptance_rate_num[is.na(listings_clean$host_acceptance_rate_num)] <- median(listings_clean$host_acceptance_rate_num, na.rm = TRUE)
  cat("Created feature: host_acceptance_rate_num\n")
}

# Days since first review
if ("first_review" %in% names(listings_clean)) {
  listings_clean$first_review_date <- as.Date(listings_clean$first_review)
  listings_clean$days_since_first_review <- as.numeric(Sys.Date() - listings_clean$first_review_date)
  listings_clean$days_since_first_review[is.na(listings_clean$days_since_first_review)] <- median(listings_clean$days_since_first_review, na.rm = TRUE)
  cat("Created feature: days_since_first_review\n")
}

# Review density (reviews per day of existence)
if ("first_review_date" %in% names(listings_clean) && "number_of_reviews" %in% names(listings_clean)) {
  listings_clean$review_density <- listings_clean$number_of_reviews / 
    pmax(listings_clean$days_since_first_review, 1)
  listings_clean$review_density[is.na(listings_clean$review_density) | is.infinite(listings_clean$review_density)] <- 0
  cat("Created feature: review_density\n")
}

# Feature ratios
listings_clean$price_per_person <- listings_clean$price_numeric / pmax(listings_clean$accommodates, 1)
listings_clean$price_per_bedroom <- listings_clean$price_numeric / pmax(listings_clean$bedrooms, 1)
listings_clean$beds_per_person <- listings_clean$beds / pmax(listings_clean$accommodates, 1)
cat("Created features: price_per_person, price_per_bedroom, beds_per_person\n")

# 4. PREPARING DATA FOR MODELING

cat("\nPreparing data for modeling...\n")

# Create dataset for modeling
model_data <- listings_clean %>%
  dplyr::select(
    price = price_numeric,
    room_type,
    neighbourhood_cleansed,
    property_type,
    host_is_superhost,
    instant_bookable,
    accommodates,
    bathrooms,
    bedrooms,
    beds,
    minimum_nights,
    number_of_reviews,
    reviews_per_month,
    calculated_host_listings_count,
    availability_365,
    availability_30,
    availability_60,
    availability_90,
    review_scores_rating,
    review_scores_accuracy,
    review_scores_cleanliness,
    review_scores_checkin,
    review_scores_communication,
    review_scores_location,
    review_scores_value,
    total_reviews_count,
    days_since_last_review,
    reviews_last_30_days,
    reviews_last_90_days,
    reviews_last_180_days,
    has_recent_reviews,
    review_activity_score,
    host_experience_years,
    amenities_count,
    distance_to_center,
    host_response_rate_num,
    host_acceptance_rate_num,
    days_since_first_review,
    review_density,
    price_per_person,
    price_per_bedroom,
    beds_per_person,
    number_of_reviews_ltm,
    number_of_reviews_l30d
  ) %>%
  filter(!is.na(accommodates) & 
         !is.na(bedrooms) & 
         !is.na(beds) &
         !is.na(room_type) &
         !is.na(neighbourhood_cleansed))

cat("Rows in modeling dataset:", nrow(model_data), "\n")
cat("Missing values:\n")
print(colSums(is.na(model_data)))

# Fill missing values in bathrooms with median
model_data$bathrooms[is.na(model_data$bathrooms)] <- median(model_data$bathrooms, na.rm = TRUE)

# Fill missing values in review scores with median
review_cols <- c("review_scores_rating", "review_scores_accuracy", 
                 "review_scores_cleanliness", "review_scores_checkin",
                 "review_scores_communication", "review_scores_location",
                 "review_scores_value")

for (col in review_cols) {
  model_data[[col]][is.na(model_data[[col]])] <- median(model_data[[col]], na.rm = TRUE)
}

# Fill missing values in new review features
model_data$total_reviews_count[is.na(model_data$total_reviews_count)] <- 0
model_data$days_since_last_review[is.na(model_data$days_since_last_review)] <- median(model_data$days_since_last_review, na.rm = TRUE)
model_data$reviews_last_30_days[is.na(model_data$reviews_last_30_days)] <- 0
model_data$reviews_last_90_days[is.na(model_data$reviews_last_90_days)] <- 0
model_data$reviews_last_180_days[is.na(model_data$reviews_last_180_days)] <- 0
model_data$has_recent_reviews[is.na(model_data$has_recent_reviews)] <- FALSE
model_data$review_activity_score[is.na(model_data$review_activity_score)] <- 0

# Fill missing values in engineered features
if ("host_experience_years" %in% names(model_data)) {
  model_data$host_experience_years[is.na(model_data$host_experience_years)] <- median(model_data$host_experience_years, na.rm = TRUE)
}
if ("amenities_count" %in% names(model_data)) {
  model_data$amenities_count[is.na(model_data$amenities_count)] <- median(model_data$amenities_count, na.rm = TRUE)
}
if ("distance_to_center" %in% names(model_data)) {
  model_data$distance_to_center[is.na(model_data$distance_to_center)] <- median(model_data$distance_to_center, na.rm = TRUE)
}
if ("host_response_rate_num" %in% names(model_data)) {
  model_data$host_response_rate_num[is.na(model_data$host_response_rate_num)] <- median(model_data$host_response_rate_num, na.rm = TRUE)
}
if ("host_acceptance_rate_num" %in% names(model_data)) {
  model_data$host_acceptance_rate_num[is.na(model_data$host_acceptance_rate_num)] <- median(model_data$host_acceptance_rate_num, na.rm = TRUE)
}
if ("days_since_first_review" %in% names(model_data)) {
  model_data$days_since_first_review[is.na(model_data$days_since_first_review)] <- median(model_data$days_since_first_review, na.rm = TRUE)
}
if ("review_density" %in% names(model_data)) {
  model_data$review_density[is.na(model_data$review_density)] <- 0
}
if ("availability_30" %in% names(model_data)) {
  model_data$availability_30[is.na(model_data$availability_30)] <- median(model_data$availability_30, na.rm = TRUE)
}
if ("availability_60" %in% names(model_data)) {
  model_data$availability_60[is.na(model_data$availability_60)] <- median(model_data$availability_60, na.rm = TRUE)
}
if ("availability_90" %in% names(model_data)) {
  model_data$availability_90[is.na(model_data$availability_90)] <- median(model_data$availability_90, na.rm = TRUE)
}
if ("number_of_reviews_ltm" %in% names(model_data)) {
  model_data$number_of_reviews_ltm[is.na(model_data$number_of_reviews_ltm)] <- 0
}
if ("number_of_reviews_l30d" %in% names(model_data)) {
  model_data$number_of_reviews_l30d[is.na(model_data$number_of_reviews_l30d)] <- 0
}

# Convert logical variables
model_data$host_is_superhost <- as.logical(model_data$host_is_superhost == "t")
model_data$instant_bookable <- as.logical(model_data$instant_bookable == "t")

cat("\nFinal dataset size:", nrow(model_data), "rows\n")
cat("Missing values after processing:", sum(is.na(model_data)), "\n")

# Save prepared dataset
write_csv(model_data, "data/model_data.csv")
cat("Prepared dataset saved to data/model_data.csv\n")

# 5. BUILDING REGRESSION MODELS

cat("\nBuilding regression models...\n")

# 5.1 Extended basic model
cat("\n--- Model 1: Extended basic model ---\n")
model1_formula <- price ~ accommodates + bedrooms + beds + bathrooms + 
  minimum_nights + number_of_reviews + reviews_per_month +
  calculated_host_listings_count + availability_365 + availability_30 + availability_60 + availability_90 +
  review_scores_rating + review_scores_accuracy + review_scores_cleanliness +
  room_type + neighbourhood_cleansed + property_type +
  host_is_superhost + instant_bookable +
  total_reviews_count + review_activity_score

if ("host_experience_years" %in% names(model_data)) {
  model1_formula <- update(model1_formula, ~ . + host_experience_years)
}
if ("amenities_count" %in% names(model_data)) {
  model1_formula <- update(model1_formula, ~ . + amenities_count)
}
if ("distance_to_center" %in% names(model_data)) {
  model1_formula <- update(model1_formula, ~ . + distance_to_center)
}
if ("host_response_rate_num" %in% names(model_data)) {
  model1_formula <- update(model1_formula, ~ . + host_response_rate_num)
}
if ("number_of_reviews_ltm" %in% names(model_data)) {
  model1_formula <- update(model1_formula, ~ . + number_of_reviews_ltm)
}

model1 <- lm(model1_formula, data = model_data)

summary(model1)
cat("R-squared:", summary(model1)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model1)$adj.r.squared, "\n")

# 5.2 Model with categorical variables and all features
cat("\n--- Model 2: With categorical variables and all features ---\n")
model2_formula <- price ~ accommodates + bedrooms + beds + bathrooms + 
  minimum_nights + number_of_reviews + reviews_per_month +
  calculated_host_listings_count + availability_365 + availability_30 + availability_60 + availability_90 +
  review_scores_rating + room_type + neighbourhood_cleansed + property_type +
  host_is_superhost + instant_bookable +
  total_reviews_count + days_since_last_review +
  reviews_last_30_days + has_recent_reviews + review_activity_score

if ("host_experience_years" %in% names(model_data)) {
  model2_formula <- update(model2_formula, ~ . + host_experience_years)
}
if ("amenities_count" %in% names(model_data)) {
  model2_formula <- update(model2_formula, ~ . + amenities_count)
}
if ("distance_to_center" %in% names(model_data)) {
  model2_formula <- update(model2_formula, ~ . + distance_to_center)
}
if ("host_response_rate_num" %in% names(model_data)) {
  model2_formula <- update(model2_formula, ~ . + host_response_rate_num)
}
if ("host_acceptance_rate_num" %in% names(model_data)) {
  model2_formula <- update(model2_formula, ~ . + host_acceptance_rate_num)
}
if ("review_density" %in% names(model_data)) {
  model2_formula <- update(model2_formula, ~ . + review_density)
}
if ("number_of_reviews_ltm" %in% names(model_data)) {
  model2_formula <- update(model2_formula, ~ . + number_of_reviews_ltm)
}
if ("number_of_reviews_l30d" %in% names(model_data)) {
  model2_formula <- update(model2_formula, ~ . + number_of_reviews_l30d)
}

model2 <- lm(model2_formula, data = model_data)

summary(model2)
cat("R-squared:", summary(model2)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model2)$adj.r.squared, "\n")

# 5.3 Model with log transformation and all features
cat("\n--- Model 3: Log transformation with all features ---\n")
model3_formula <- log(price) ~ accommodates + bedrooms + beds + bathrooms + 
  minimum_nights + number_of_reviews + reviews_per_month +
  calculated_host_listings_count + availability_365 + availability_30 + availability_60 + availability_90 +
  review_scores_rating + room_type + neighbourhood_cleansed + property_type +
  host_is_superhost + instant_bookable +
  total_reviews_count + days_since_last_review +
  reviews_last_30_days + has_recent_reviews + review_activity_score +
  I(accommodates^2) + I(bedrooms^2)

if ("host_experience_years" %in% names(model_data)) {
  model3_formula <- update(model3_formula, ~ . + host_experience_years)
}
if ("amenities_count" %in% names(model_data)) {
  model3_formula <- update(model3_formula, ~ . + amenities_count)
}
if ("distance_to_center" %in% names(model_data)) {
  model3_formula <- update(model3_formula, ~ . + distance_to_center)
}
if ("host_response_rate_num" %in% names(model_data)) {
  model3_formula <- update(model3_formula, ~ . + host_response_rate_num)
}
if ("host_acceptance_rate_num" %in% names(model_data)) {
  model3_formula <- update(model3_formula, ~ . + host_acceptance_rate_num)
}
if ("review_density" %in% names(model_data)) {
  model3_formula <- update(model3_formula, ~ . + review_density)
}
if ("number_of_reviews_ltm" %in% names(model_data)) {
  model3_formula <- update(model3_formula, ~ . + number_of_reviews_ltm)
}
if ("number_of_reviews_l30d" %in% names(model_data)) {
  model3_formula <- update(model3_formula, ~ . + number_of_reviews_l30d)
}

model3 <- lm(model3_formula, data = model_data)

summary(model3)
cat("R-squared:", summary(model3)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model3)$adj.r.squared, "\n")

# 5.4 Model with interactions and polynomials
cat("\n--- Model 4: With interactions, polynomials and all features ---\n")
model4_formula <- log(price) ~ accommodates + bedrooms + beds + bathrooms + 
  minimum_nights + number_of_reviews + reviews_per_month +
  calculated_host_listings_count + availability_365 + availability_30 + availability_60 + availability_90 +
  review_scores_rating + review_scores_accuracy + review_scores_cleanliness +
  room_type + neighbourhood_cleansed + property_type +
  host_is_superhost + instant_bookable +
  total_reviews_count + days_since_last_review +
  reviews_last_30_days + has_recent_reviews + review_activity_score +
  I(accommodates^2) + I(bedrooms^2) + I(bathrooms^2) + I(review_scores_rating^2) +
  accommodates:bedrooms + accommodates:bathrooms + bedrooms:bathrooms +
  room_type:accommodates + room_type:bedrooms + room_type:review_scores_rating +
  neighbourhood_cleansed:accommodates + neighbourhood_cleansed:review_scores_rating +
  host_is_superhost:review_scores_rating

if ("host_experience_years" %in% names(model_data)) {
  model4_formula <- update(model4_formula, ~ . + host_experience_years + I(host_experience_years^2))
}
if ("amenities_count" %in% names(model_data)) {
  model4_formula <- update(model4_formula, ~ . + amenities_count + I(amenities_count^2))
}
if ("distance_to_center" %in% names(model_data)) {
  model4_formula <- update(model4_formula, ~ . + distance_to_center + I(distance_to_center^2))
}
if ("host_response_rate_num" %in% names(model_data)) {
  model4_formula <- update(model4_formula, ~ . + host_response_rate_num)
}
if ("host_acceptance_rate_num" %in% names(model_data)) {
  model4_formula <- update(model4_formula, ~ . + host_acceptance_rate_num)
}
if ("review_density" %in% names(model_data)) {
  model4_formula <- update(model4_formula, ~ . + review_density)
}
if ("number_of_reviews_ltm" %in% names(model_data)) {
  model4_formula <- update(model4_formula, ~ . + number_of_reviews_ltm)
}
if ("number_of_reviews_l30d" %in% names(model_data)) {
  model4_formula <- update(model4_formula, ~ . + number_of_reviews_l30d)
}

model4 <- lm(model4_formula, data = model_data)

summary(model4)
cat("R-squared:", summary(model4)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model4)$adj.r.squared, "\n")

# 5.5 Stepwise regression
cat("\n--- Model 5: Stepwise regression ---\n")
model_data_complete <- model_data[complete.cases(model_data), ]

model_null <- lm(log(price) ~ 1, data = model_data_complete)
model_full_formula <- log(price) ~ accommodates + bedrooms + beds + bathrooms + 
  minimum_nights + number_of_reviews + reviews_per_month +
  calculated_host_listings_count + availability_365 + availability_30 + availability_60 + availability_90 +
  review_scores_rating + review_scores_accuracy +
  review_scores_cleanliness + review_scores_checkin +
  review_scores_communication + review_scores_location +
  review_scores_value + room_type + neighbourhood_cleansed + property_type +
  host_is_superhost + instant_bookable +
  total_reviews_count + days_since_last_review +
  reviews_last_30_days + reviews_last_90_days +
  has_recent_reviews + review_activity_score +
  I(accommodates^2) + I(bedrooms^2) + I(bathrooms^2) +
  accommodates:bedrooms + accommodates:bathrooms + bedrooms:bathrooms +
  room_type:accommodates + room_type:bedrooms + room_type:review_scores_rating +
  neighbourhood_cleansed:accommodates

if ("host_experience_years" %in% names(model_data_complete)) {
  model_full_formula <- update(model_full_formula, ~ . + host_experience_years + I(host_experience_years^2))
}
if ("amenities_count" %in% names(model_data_complete)) {
  model_full_formula <- update(model_full_formula, ~ . + amenities_count + I(amenities_count^2))
}
if ("distance_to_center" %in% names(model_data_complete)) {
  model_full_formula <- update(model_full_formula, ~ . + distance_to_center + I(distance_to_center^2))
}
if ("host_response_rate_num" %in% names(model_data_complete)) {
  model_full_formula <- update(model_full_formula, ~ . + host_response_rate_num)
}
if ("host_acceptance_rate_num" %in% names(model_data_complete)) {
  model_full_formula <- update(model_full_formula, ~ . + host_acceptance_rate_num)
}
if ("review_density" %in% names(model_data_complete)) {
  model_full_formula <- update(model_full_formula, ~ . + review_density)
}
if ("price_per_person" %in% names(model_data_complete)) {
  model_full_formula <- update(model_full_formula, ~ . + price_per_person)
}
if ("number_of_reviews_ltm" %in% names(model_data_complete)) {
  model_full_formula <- update(model_full_formula, ~ . + number_of_reviews_ltm)
}
if ("number_of_reviews_l30d" %in% names(model_data_complete)) {
  model_full_formula <- update(model_full_formula, ~ . + number_of_reviews_l30d)
}

model_full <- lm(model_full_formula, data = model_data_complete)

model5 <- stepAIC(model_null, 
                  scope = list(lower = model_null, upper = model_full),
                  direction = "both", trace = FALSE)

summary(model5)
cat("R-squared:", summary(model5)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model5)$adj.r.squared, "\n")

# 5.6 Ridge and Lasso regression
cat("\n--- Model 6: Ridge Regression ---\n")
cat("--- Model 7: Lasso Regression ---\n")

# Prepare data for glmnet
model_matrix_full <- model.matrix(model_full_formula, data = model_data_complete)
y_log <- log(model_data_complete$price)

# Remove intercept (glmnet adds it automatically)
model_matrix_full <- model_matrix_full[, -1]

# Ridge regression (alpha = 0)
set.seed(123)
cv_ridge <- cv.glmnet(model_matrix_full, y_log, alpha = 0, nfolds = 5)
model6_ridge <- glmnet(model_matrix_full, y_log, alpha = 0, lambda = cv_ridge$lambda.min)

# Calculate R²
y_pred_ridge <- predict(model6_ridge, newx = model_matrix_full, s = cv_ridge$lambda.min)
ss_res_ridge <- sum((y_log - y_pred_ridge)^2)
ss_tot_ridge <- sum((y_log - mean(y_log))^2)
r2_ridge <- 1 - ss_res_ridge / ss_tot_ridge

cat("Ridge Regression - Lambda:", cv_ridge$lambda.min, "\n")
cat("Ridge Regression - R-squared:", r2_ridge, "\n")

# Lasso regression (alpha = 1)
set.seed(123)
cv_lasso <- cv.glmnet(model_matrix_full, y_log, alpha = 1, nfolds = 5)
model7_lasso <- glmnet(model_matrix_full, y_log, alpha = 1, lambda = cv_lasso$lambda.min)

# Calculate R²
y_pred_lasso <- predict(model7_lasso, newx = model_matrix_full, s = cv_lasso$lambda.min)
ss_res_lasso <- sum((y_log - y_pred_lasso)^2)
ss_tot_lasso <- sum((y_log - mean(y_log))^2)
r2_lasso <- 1 - ss_res_lasso / ss_tot_lasso

cat("Lasso Regression - Lambda:", cv_lasso$lambda.min, "\n")
cat("Lasso Regression - R-squared:", r2_lasso, "\n")
cat("Lasso Regression - Number of non-zero coefficients:", sum(coef(model7_lasso, s = cv_lasso$lambda.min) != 0), "\n")

# Save models
models_list <- list(
  model1 = model1,
  model2 = model2,
  model3 = model3,
  model4 = model4,
  model5 = model5,
  model6_ridge = model6_ridge,
  model7_lasso = model7_lasso,
  cv_ridge = cv_ridge,
  cv_lasso = cv_lasso,
  model_matrix_full = model_matrix_full,
  y_log = y_log
)

save(models_list, file = "models.RData")
cat("\nModels saved to models.RData\n")

# 6. MODEL DIAGNOSTICS

cat("\nModel diagnostics...\n")

# Select best model for diagnostics
best_model <- model5

# 6.1 Residual plots
png("results/plots/06_residuals_diagnostics.png", width = 1200, height = 1000)

par(mfrow = c(2, 2))

plot(fitted(best_model), residuals(best_model),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(best_model), main = "Q-Q Plot of Residuals")
qqline(residuals(best_model), col = "red")

plot(fitted(best_model), sqrt(abs(residuals(best_model))),
     xlab = "Fitted values", 
     ylab = "sqrt(|Residuals|)",
     main = "Scale-Location Plot")

plot(residuals(best_model), 
     xlab = "Observation order", ylab = "Residuals",
     main = "Residuals vs Order")
abline(h = 0, col = "red", lty = 2)

dev.off()

# 6.2 Multicollinearity check
cat("\n--- Multicollinearity check (VIF) ---\n")
tryCatch({
  vif_values <- vif(best_model, type = "predictor")
  print(vif_values)
  cat("\nVIF > 10 indicates multicollinearity problem\n")
}, error = function(e) {
  cat("Could not compute VIF due to interactions in model:", e$message, "\n")
  cat("This is normal for models with polynomials and interactions.\n")
})

# 6.3 Outlier check
cat("\n--- Outlier check ---\n")
cooks_d <- cooks.distance(best_model)
outliers_cooks <- which(cooks_d > 4/nrow(model_data))
cat("Outliers by Cook's distance (> 4/n):", length(outliers_cooks), "\n")

standard_residuals <- rstandard(best_model)
outliers_standard <- which(abs(standard_residuals) > 3)
cat("Outliers by standardized residuals (> 3):", length(outliers_standard), "\n")

# Cook's distance plot
png("results/plots/07_cooks_distance.png", width = 800, height = 600)
plot(cooks_d, type = "h", 
     xlab = "Observation", ylab = "Cook's Distance",
     main = "Cook's Distance")
abline(h = 4/nrow(model_data), col = "red", lty = 2)
dev.off()

# 6.4 Normality test
cat("\n--- Normality test (Shapiro-Wilk) ---\n")
sample_size <- min(5000, length(residuals(best_model)))
shapiro_test <- shapiro.test(sample(residuals(best_model), sample_size))
print(shapiro_test)

# 6.5 Heteroscedasticity test
cat("\n--- Heteroscedasticity test (Breusch-Pagan) ---\n")
bp_test <- ncvTest(best_model)
print(bp_test)

# 6.6 Additional diagnostic plots

# Plot 8: Predictions vs Actual values
png("results/plots/08_predictions_vs_actual.png", width = 1000, height = 800)
par(mfrow = c(1, 2))

predicted_log <- fitted(best_model)
actual_log <- log(model_data_complete$price)

plot(actual_log, predicted_log,
     xlab = "Actual values (log(price))", 
     ylab = "Predicted values (log(price))",
     main = "Predictions vs Actual (log scale)",
     pch = 19, col = rgb(70, 130, 180, alpha = 80, maxColorValue = 255))
abline(0, 1, col = "red", lwd = 2)

predicted_price <- exp(predicted_log)
actual_price <- model_data_complete$price

plot(actual_price, predicted_price,
     xlab = "Actual values (price)", 
     ylab = "Predicted values (price)",
     main = "Predictions vs Actual (original scale)",
     pch = 19, col = rgb(70, 130, 180, alpha = 80, maxColorValue = 255))
abline(0, 1, col = "red", lwd = 2)

dev.off()

# Plot 9: Feature importance (top-20 coefficients)
png("results/plots/09_feature_importance.png", width = 1200, height = 800)

coef_summary <- summary(best_model)$coefficients
coef_df <- data.frame(
  Feature = rownames(coef_summary),
  Coefficient = coef_summary[, "Estimate"],
  PValue = coef_summary[, "Pr(>|t|)"]
)

coef_df <- coef_df[coef_df$Feature != "(Intercept)", ]

coef_df$AbsCoeff <- abs(coef_df$Coefficient)
coef_df <- coef_df[order(coef_df$AbsCoeff, decreasing = TRUE), ]
top_features <- head(coef_df, 20)

par(mar = c(5, 12, 4, 2))
barplot(top_features$Coefficient, 
        names.arg = top_features$Feature,
        horiz = TRUE,
        las = 1,
        main = "Top 20 Important Features (Coefficients)",
        xlab = "Coefficient",
        col = ifelse(top_features$Coefficient > 0, "steelblue", "coral"),
        cex.names = 0.7)
abline(v = 0, col = "black", lwd = 1)

dev.off()

# Plot 10: Engineered features impact
png("results/plots/10_engineered_features_impact.png", width = 1400, height = 1000)

par(mfrow = c(2, 3))

if ("host_experience_years" %in% names(model_data_complete)) {
  plot(model_data_complete$host_experience_years, log(model_data_complete$price),
       xlab = "Host Experience (years)", ylab = "log(price)",
       main = "Host Experience Impact on Price",
       pch = 19, col = rgb(70, 130, 180, alpha = 80, maxColorValue = 255))
  if (sum(!is.na(model_data_complete$host_experience_years)) > 10) {
    lines(lowess(model_data_complete$host_experience_years, log(model_data_complete$price)), 
          col = "red", lwd = 2)
  }
}

if ("amenities_count" %in% names(model_data_complete)) {
  plot(model_data_complete$amenities_count, log(model_data_complete$price),
       xlab = "Number of Amenities", ylab = "log(price)",
       main = "Amenities Count Impact on Price",
       pch = 19, col = rgb(70, 130, 180, alpha = 80, maxColorValue = 255))
  if (sum(!is.na(model_data_complete$amenities_count)) > 10) {
    lines(lowess(model_data_complete$amenities_count, log(model_data_complete$price)), 
          col = "red", lwd = 2)
  }
}

if ("distance_to_center" %in% names(model_data_complete)) {
  plot(model_data_complete$distance_to_center, log(model_data_complete$price),
       xlab = "Distance to Center (km)", ylab = "log(price)",
       main = "Distance to Center Impact on Price",
       pch = 19, col = rgb(70, 130, 180, alpha = 80, maxColorValue = 255))
  if (sum(!is.na(model_data_complete$distance_to_center)) > 10) {
    lines(lowess(model_data_complete$distance_to_center, log(model_data_complete$price)), 
          col = "red", lwd = 2)
  }
}

if ("review_density" %in% names(model_data_complete)) {
  plot(model_data_complete$review_density, log(model_data_complete$price),
       xlab = "Review Density", ylab = "log(price)",
       main = "Review Density Impact on Price",
       pch = 19, col = rgb(70, 130, 180, alpha = 80, maxColorValue = 255))
  if (sum(!is.na(model_data_complete$review_density)) > 10) {
    lines(lowess(model_data_complete$review_density, log(model_data_complete$price)), 
          col = "red", lwd = 2)
  }
}

if ("host_response_rate_num" %in% names(model_data_complete)) {
  plot(model_data_complete$host_response_rate_num, log(model_data_complete$price),
       xlab = "Host Response Rate", ylab = "log(price)",
       main = "Host Response Rate Impact on Price",
       pch = 19, col = rgb(70, 130, 180, alpha = 80, maxColorValue = 255))
  if (sum(!is.na(model_data_complete$host_response_rate_num)) > 10) {
    lines(lowess(model_data_complete$host_response_rate_num, log(model_data_complete$price)), 
          col = "red", lwd = 2)
  }
}

if ("review_activity_score" %in% names(model_data_complete)) {
  plot(model_data_complete$review_activity_score, log(model_data_complete$price),
       xlab = "Review Activity Score", ylab = "log(price)",
       main = "Review Activity Impact on Price",
       pch = 19, col = rgb(70, 130, 180, alpha = 80, maxColorValue = 255))
  if (sum(!is.na(model_data_complete$review_activity_score)) > 10) {
    lines(lowess(model_data_complete$review_activity_score, log(model_data_complete$price)), 
          col = "red", lwd = 2)
  }
}

dev.off()

# Plot 11: Model comparison (R²)
png("results/plots/11_model_comparison.png", width = 1000, height = 600)

model_names <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6\n(Ridge)", "Model 7\n(Lasso)")
r2_values <- c(summary(model1)$r.squared,
               summary(model2)$r.squared,
               summary(model3)$r.squared,
               summary(model4)$r.squared,
               summary(model5)$r.squared,
               r2_ridge,
               r2_lasso)

par(mar = c(8, 5, 4, 2))
barplot(r2_values,
        names.arg = model_names,
        ylab = "R²",
        main = "Model Comparison by R²",
        col = "steelblue",
        las = 2,
        ylim = c(0, 1))
abline(h = 0.7, col = "red", lty = 2, lwd = 2)
text(length(model_names)/2, 0.72, "Target: R² > 0.7", col = "red", cex = 1.2)

dev.off()

# Plot 12 will be created after cross-validation

# Plot 13: Residual distribution
png("results/plots/13_residuals_distribution.png", width = 1200, height = 800)

par(mfrow = c(2, 2))

hist(residuals(best_model),
     breaks = 50,
     main = "Residual Distribution",
     xlab = "Residuals",
     ylab = "Frequency",
     col = "steelblue",
     border = "white")

plot(density(residuals(best_model)),
     main = "Residual Density",
     xlab = "Residuals",
     ylab = "Density",
     col = "steelblue",
     lwd = 2)
lines(density(rnorm(10000, mean(residuals(best_model)), sd(residuals(best_model)))),
      col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Actual", "Normal"), 
       col = c("steelblue", "red"), lty = c(1, 2), lwd = 2)

hist(standard_residuals,
     breaks = 50,
     main = "Standardized Residual Distribution",
     xlab = "Standardized Residuals",
     ylab = "Frequency",
     col = "coral",
     border = "white")
abline(v = c(-3, 3), col = "red", lty = 2, lwd = 2)

qqnorm(residuals(best_model), main = "Q-Q Plot of Residuals (Detailed)")
qqline(residuals(best_model), col = "red", lwd = 2)

dev.off()

# Plot 14: Leverage vs Residuals
png("results/plots/14_leverage_vs_residuals.png", width = 1000, height = 800)

par(mfrow = c(1, 2))

leverage <- hatvalues(best_model)

plot(leverage, residuals(best_model),
     xlab = "Leverage (Hat values)",
     ylab = "Residuals",
     main = "Leverage vs Residuals",
     pch = 19,
     col = rgb(70, 130, 180, alpha = 80, maxColorValue = 255))
abline(h = 0, col = "red", lty = 2)
abline(v = 2 * mean(leverage), col = "orange", lty = 2, lwd = 2)

plot(leverage, standard_residuals,
     xlab = "Leverage (Hat values)",
     ylab = "Standardized Residuals",
     main = "Leverage vs Standardized Residuals",
     pch = 19,
     col = rgb(255, 127, 80, alpha = 80, maxColorValue = 255))
abline(h = c(-3, 3), col = "red", lty = 2)
abline(v = 2 * mean(leverage), col = "orange", lty = 2, lwd = 2)

dev.off()

cat("\nAdditional diagnostic plots saved:\n")
cat("- 08_predictions_vs_actual.png\n")
cat("- 09_feature_importance.png\n")
cat("- 10_engineered_features_impact.png\n")
cat("- 11_model_comparison.png\n")
cat("- 12_cv_results.png (will be created after cross-validation)\n")
cat("- 13_residuals_distribution.png\n")
cat("- 14_leverage_vs_residuals.png\n")

# 7. MODEL COMPARISON

cat("\nModel comparison...\n")

model_comparison <- data.frame(
  Model = c("Model 1 (extended basic)", 
            "Model 2 (+ categorical)", 
            "Model 3 (log transformation)",
            "Model 4 (+ interactions + polynomials)",
            "Model 5 (stepwise)",
            "Model 6 (Ridge)",
            "Model 7 (Lasso)"),
  R_squared = c(summary(model1)$r.squared,
                summary(model2)$r.squared,
                summary(model3)$r.squared,
                summary(model4)$r.squared,
                summary(model5)$r.squared,
                r2_ridge,
                r2_lasso),
  Adj_R_squared = c(summary(model1)$adj.r.squared,
                    summary(model2)$adj.r.squared,
                    summary(model3)$adj.r.squared,
                    summary(model4)$adj.r.squared,
                    summary(model5)$adj.r.squared,
                    NA,
                    NA),
  AIC = c(AIC(model1),
          AIC(model2),
          AIC(model3),
          AIC(model4),
          AIC(model5),
          NA,
          NA),
  BIC = c(BIC(model1),
          BIC(model2),
          BIC(model3),
          BIC(model4),
          BIC(model5),
          NA,
          NA)
)

print(model_comparison)

write_csv(model_comparison, "results/model_comparison.csv")
cat("\nModel comparison table saved to results/model_comparison.csv\n")

# F-tests for model comparison
cat("\n--- F-tests for model comparison ---\n")
cat("Note: F-tests require models to be built on the same dataset.\n")
cat("Models 3, 4 and 5 use log(price), so they can be compared.\n")

tryCatch({
  anova_test_3_4 <- anova(model3, model4)
  cat("\nModel 3 vs Model 4:\n")
  print(anova_test_3_4)
}, error = function(e) {
  cat("Could not compare Model 3 and Model 4:", e$message, "\n")
})

tryCatch({
  model3_complete <- update(model3, data = model_data_complete)
  anova_test_3_5 <- anova(model3_complete, model5)
  cat("\nModel 3 vs Model 5:\n")
  print(anova_test_3_5)
}, error = function(e) {
  cat("Could not compare Model 3 and Model 5:", e$message, "\n")
})

# 8. CROSS-VALIDATION

cat("\nCross-validation...\n")

set.seed(123)
n <- nrow(model_data_complete)
k <- 5
folds <- sample(rep(1:k, length.out = n))

cv_errors <- numeric(k)

for (i in 1:k) {
  train_data <- model_data_complete[folds != i, ]
  test_data <- model_data_complete[folds == i, ]
  
  # Ensure all categorical variable levels are present in train_data
  if ("property_type" %in% names(train_data)) {
    train_levels <- unique(train_data$property_type)
    test_data$property_type[!test_data$property_type %in% train_levels] <- NA
  }
  if ("room_type" %in% names(train_data)) {
    train_levels <- unique(train_data$room_type)
    test_data$room_type[!test_data$room_type %in% train_levels] <- NA
  }
  if ("neighbourhood_cleansed" %in% names(train_data)) {
    train_levels <- unique(train_data$neighbourhood_cleansed)
    test_data$neighbourhood_cleansed[!test_data$neighbourhood_cleansed %in% train_levels] <- NA
  }
  
  test_data <- test_data[complete.cases(test_data[, c("property_type", "room_type", "neighbourhood_cleansed")]), ]
  
  if (nrow(test_data) == 0) {
    cv_errors[i] <- NA
    next
  }
  
  tryCatch({
    cv_model <- update(best_model, data = train_data)
    
    predictions <- exp(predict(cv_model, newdata = test_data))
    actual <- test_data$price
    
    cv_errors[i] <- sqrt(mean((predictions - actual)^2, na.rm = TRUE))
  }, error = function(e) {
    cat("Error in fold", i, ":", e$message, "\n")
    cv_errors[i] <- NA
  })
}

cat("RMSE for each fold:\n")
print(cv_errors)
cat("Mean RMSE:", mean(cv_errors, na.rm = TRUE), "\n")
cat("RMSE standard deviation:", sd(cv_errors, na.rm = TRUE), "\n")

cv_results <- data.frame(
  Fold = 1:k,
  RMSE = cv_errors
)
write_csv(cv_results, "results/cv_results.csv")
cat("Cross-validation results saved to results/cv_results.csv\n")

# Plot 12: Cross-validation results
png("results/plots/12_cv_results.png", width = 1000, height = 600)

par(mfrow = c(1, 2))

barplot(cv_errors,
        names.arg = paste("Fold", 1:length(cv_errors)),
        ylab = "RMSE",
        main = "RMSE by Cross-Validation Fold",
        col = "steelblue",
        ylim = c(0, max(cv_errors, na.rm = TRUE) * 1.2))
abline(h = mean(cv_errors, na.rm = TRUE), col = "red", lty = 2, lwd = 2)
text(length(cv_errors)/2, mean(cv_errors, na.rm = TRUE) * 1.1, 
     paste("Mean RMSE:", round(mean(cv_errors, na.rm = TRUE), 2)), 
     col = "red", cex = 1.1)

boxplot(cv_errors,
        ylab = "RMSE",
        main = "RMSE Distribution",
        col = "lightblue")

dev.off()
cat("Plot 12 (Cross-validation) saved\n")

# 9. FINAL MODEL AND INTERPRETATION

cat("\nFinal model...\n")
cat("\nSelected model: Model 5 (Stepwise Regression)\n")
cat("\nModel coefficients:\n")
print(summary(best_model)$coefficients)

cat("\nConfidence intervals (95%):\n")
print(confint(best_model))

final_model_summary <- tidy(best_model)
final_model_summary$conf_low <- confint(best_model)[, 1]
final_model_summary$conf_high <- confint(best_model)[, 2]

write_csv(final_model_summary, "results/final_model_coefficients.csv")
cat("\nFinal model coefficients saved to results/final_model_coefficients.csv\n")

cat("\nAnalysis completed.\n")
cat("All results saved:\n")
cat("- Plots (14 total): results/plots/\n")
cat("  01_price_distribution.png\n")
cat("  02_price_by_room_type.png\n")
cat("  03_price_by_neighbourhood.png\n")
cat("  04_correlation_matrix.png\n")
cat("  05_scatterplots.png\n")
cat("  06_residuals_diagnostics.png\n")
cat("  07_cooks_distance.png\n")
cat("  08_predictions_vs_actual.png\n")
cat("  09_feature_importance.png\n")
cat("  10_engineered_features_impact.png\n")
cat("  11_model_comparison.png\n")
cat("  12_cv_results.png\n")
cat("  13_residuals_distribution.png\n")
cat("  14_leverage_vs_residuals.png\n")
cat("- Models: models.RData\n")
cat("- Model comparison: results/model_comparison.csv\n")
cat("- Cross-validation: results/cv_results.csv\n")
cat("- Final model coefficients: results/final_model_coefficients.csv\n")
cat("\nImprovements:\n")
cat("- Applied IQR method for outlier treatment\n")
cat("- Added features from review data\n")
cat("- Merged data from multiple sources\n")
