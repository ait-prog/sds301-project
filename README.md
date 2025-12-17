# SDS 301: Airbnb Pricing Regression Analysis

Regression analysis project for Airbnb pricing in Amsterdam.

https://insideairbnb.com/get-the-data/

## Project Structure

```
├── data/                    # Source data
│   ├── listings_gz.csv     # Detailed listing information
│   ├── listings.csv        # Brief listing information
│   ├── reviews.csv         # Reviews
│   ├── reviews_gz.csv      # Detailed reviews
│   └── neighbourhoods.csv  # Neighborhoods
├── results/                 # Analysis results
│   ├── plots/              # EDA and diagnostic plots
│   ├── model_comparison.csv
│   ├── cv_results.csv
│   └── final_model_coefficients.csv
├── airbnb_regression_analysis.R  # Main analysis script
└── README.md
```

## Description

The project includes:

1. **EDA (Exploratory Data Analysis)** - exploratory data analysis
2. **Modeling** - building multiple linear regression models
3. **Diagnostics** - checking models for multicollinearity, outliers, residual normality
4. **Model Selection** - comparing models using F-tests and cross-validation

## Results

### Best Model

- **Model 5 (Stepwise)**: R² = 0.942, Adjusted R² = 0.940
- **Model 7 (Lasso)**: R² = 0.942
- **Model 6 (Ridge)**: R² = 0.922

### Quality Metrics

- Mean Absolute Error (MAE): ~37 EUR (for log(price))
- Root Mean Squared Error (RMSE): ~37 EUR (cross-validation)

### Improvements Applied

- IQR method for outlier treatment
- Features from review data
- Merged data from multiple sources
- Feature engineering (host experience, amenities count, distance to center, etc.)
- Polynomial features and interactions
- Ridge and Lasso regularization

## Running

### Option 1: Locally (without Docker)

1. Install required R packages:
```r
install.packages(c('readr', 'dplyr', 'ggplot2', 'corrplot', 'car', 'MASS', 'broom', 'gridExtra', 'glmnet'))
```

2. Run the main script:
```r
source("airbnb_regression_analysis.R")
```

### Option 2: With Docker (recommended for reproducibility)

1. Make sure Docker is installed:
```bash
docker --version
```

2. Run analysis in container:
```bash
docker-compose up r-analysis
```

Or build and run manually:
```bash
docker build -t sds301-r .
docker run -v $(pwd)/results:/app/results sds301-r
```

### Future Services

The project is prepared for expansion:
- **Python service**: `docker-compose --profile python up python-service`
- **Next.js application**: `docker-compose --profile web up nextjs-app`

## Authors

- **Yerkezhan Burambay**
- **Zhanahmetov Ansar**
- **Ruslan Kudaibergenov**
- **Alibek Aitbekov**

SDS 301 Modern Regression Analysis - Final Project
