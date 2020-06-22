# Required packages:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# if(!require(matrixStats)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(kknn)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("caret", repos = "http://cran.us.r-project.org")


# Loading training and testing sets
test <- read.csv("datasets/test.csv")
train <- read.csv("datasets/train.csv")
sample_submission <- read.csv("datasets/sample_submission.csv")
# actual prices for testing purposes:
actual_prices <- read.csv("datasets/full-score.csv")

# ------------------------------ Data Cleaning ----------------------------------------

# Combining test and train set in order to handle NAs
full_set <- train %>% select(-SalePrice) %>% rbind(test)
y <- train$SalePrice

# Categorical and numerical columns
numeric_cols <- full_set %>% select_if(is.numeric) %>% select(-Id)
categorical_cols <- full_set %>% select_if(function(x) ! is.numeric(x))

cat_col_names <- colnames(categorical_cols)
num_col_names <- colnames(numeric_cols)

# Setting categorical NAs as "NA" character, then setting all columns back to factors
categorical_cols <- apply(categorical_cols, 2, function(x) {
  ifelse(is.na(x), "NA", x)}) %>% as.data.frame(.) %>% 
  mutate_all(factor)

# Selecting numerical cols, and binding new cat_cols back on
full_set <- full_set %>% select(-all_of(cat_col_names)) %>% cbind(categorical_cols)



# Finding numeric NAs
NA_cols_numeric <-apply(numeric_cols, 2, function(x) any(is.na(x)))
which(NA_cols_numeric == T)

# Filling in numerical NAs

# Distribution of the lot frontage
train %>% ggplot(aes(LotFrontage)) +
  geom_histogram()
#Approx. Normal but one large outlier replace NAs with median
numeric_cols <- numeric_cols %>% 
  mutate(LotFrontage = ifelse(is.na(LotFrontage),
                              median(train$LotFrontage,na.rm = T), LotFrontage))


# Distribution of the Mas Vnr Area
train %>% ggplot(aes(MasVnrArea)) +
  geom_histogram()
# Skewed right with a mode at 0, replace Nas with median (0)
numeric_cols <- numeric_cols %>% 
  mutate(MasVnrArea = ifelse(is.na(MasVnrArea),
                             median(train$MasVnrArea,na.rm = T), MasVnrArea))


# Distribution of the Year the Garage was built
train %>% ggplot(aes(GarageYrBlt)) +
  geom_histogram()
# Skewed left, replace with median
numeric_cols <- numeric_cols %>% 
  mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),
                              median(train$GarageYrBlt,na.rm = T), GarageYrBlt))


# Since most of these disrtibutions are skewed, we will fill in remaining Nas with the median
numeric_cols <- numeric_cols %>% 
  mutate(BsmtFinSF2 = ifelse(is.na(BsmtFinSF2),
                             median(train$BsmtFinSF2,na.rm = T), BsmtFinSF2),
         BsmtFinSF1 = ifelse(is.na(BsmtFinSF1),
                             median(train$BsmtFinSF1,na.rm = T), BsmtFinSF1),
         TotalBsmtSF = ifelse(is.na(TotalBsmtSF),
                              median(train$TotalBsmtSF,na.rm = T), TotalBsmtSF),
         BsmtFullBath = ifelse(is.na(BsmtFullBath),
                               median(train$BsmtFullBath,na.rm = T), BsmtFullBath),
         BsmtHalfBath = ifelse(is.na(BsmtHalfBath),
                               median(train$BsmtHalfBath,na.rm = T), BsmtHalfBath),
         GarageCars = ifelse(is.na(GarageCars),
                             median(train$GarageCars,na.rm = T), GarageCars),
         GarageArea = ifelse(is.na(GarageArea),
                             median(train$GarageArea,na.rm = T), GarageArea),
         BsmtUnfSF  = ifelse(is.na(BsmtUnfSF ),
                             median(train$BsmtUnfSF ,na.rm = T), BsmtUnfSF ))

# Adding columns back into the full set
full_set <- full_set %>% select(-all_of(num_col_names)) %>% cbind(numeric_cols)

# No more Nas!
NA_full_set <-apply(full_set, 2, function(x) any(is.na(x)))
which(NA_full_set == T)

# Splitting back into training and testing sets
i <- 1:nrow(train)
train <- full_set[i,] %>% cbind(y)
test <- full_set[-i,]




# Function to calculate RMSE:
RMSE <- function(actual_price, predicted_price){
  sqrt(mean((actual_price - predicted_price)^2))
}
RMSE_log <- function(actual_price, predicted_price){
  sqrt(mean((log(actual_price) - log(predicted_price))^2))
}



# Histogram of the SalePrice distribution
train %>% ggplot(aes(y)) +
  geom_histogram() +
  ggtitle("Sale Price Distribution Histogram")
# Distribution is skewed right, let's try taking the log of the sales price instead
train %>% ggplot(aes(log10(y))) +
  geom_histogram() +
  ggtitle("Sale Price Distribution Histogram")

# Baseline guessing model:
mu <- 10^mean(log10(train$y))
predictions_guess <- data.frame(Id = test$Id, SalePrice = mu)
write.csv(predictions_guess, file = "predictions/predictions_guess.csv", row.names = F)
y_centered <- train$y - mu

# Adding a column with centered SalePrice
train <- train %>% mutate(y_centered = y_centered)


# Fitting a weighted k-nearest neighboors model on the numerical variables
numeric_cols_train <- numeric_cols[i,] %>% cbind(y_centered)
fit_num_kknn <- train(y_centered~., method = "kknn",
                     data = numeric_cols_train)
min(fit_num_kknn$results$RMSE)

predictions_kknn <- predict(fit_num_kknn, test, type = "raw") + mu 
predictions_kknn <- data.frame(Id = test$Id, SalePrice = predictions_kknn)
write.csv(predictions_kknn, file = "predictions/predictions_kknn.csv", row.names = F)
num_effect_train <- predict(fit_num_kknn, train, type = "raw") 
num_effect_test <- predictions_kknn$SalePrice - mu
y_minus_effects <- y_centered - num_effect_train



# Implementing random forest
categorical_cols_train <- categorical_cols[i,] %>% cbind(y_minus_effects)
fit_kknn_rf <- train(y_minus_effects~., method = "Rborist", data = categorical_cols_train,
                     verbose = T, tuneGrid = data.frame(predFixed = 2, minNode = 3))
fit_kknn_rf$results
predictions_kknn_rf <- predict(fit_kknn_rf, test, type = "raw") + mu + num_effect_test
predictions_kknn_rf <- data.frame(Id = test$Id, SalePrice = predictions_kknn_rf)
write.csv(predictions_kknn_rf, file = "predictions/predictions_kknn_rf.csv", row.names = F)

# Final RMSE
RMSE_log(actual_prices$SalePrice, predictions_kknn_rf$SalePrice)
