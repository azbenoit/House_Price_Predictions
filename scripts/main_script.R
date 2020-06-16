# Required packages:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")


# Loading training and testing sets
test <- read.csv("datasets/test.csv")
train <- read.csv("datasets/train.csv")
sample_submission <- read.csv("datasets/sample_submission.csv")
# actual prices for testing purposes:
actual_prices <- read.csv("datasets/full-score.csv")


# Function to calculate RMSE:
RMSE <- function(actual_rating, predicted_rating){
  sqrt(mean((actual_rating - predicted_rating)^2))
}

# Histogram of the SalePrice distribution
train %>% ggplot(aes(SalePrice)) +
  geom_histogram() +
  ggtitle("Sale Price Distribution Histogram")


# Baseline guessing model:
mu <- mean(train$SalePrice)

# Categorical and numerical column numbers

