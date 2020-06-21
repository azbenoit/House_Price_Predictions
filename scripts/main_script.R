# Required packages:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
# if(!require(matrixStats)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(k)) install.packages("caret", repos = "http://cran.us.r-project.org")


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

# Categorical and numerical column numbers
numeric_cols <- full_set %>% select_if(is.numeric) %>% select(-Id)
categorical_cols <- full_set %>% select_if(function(x) ! is.numeric(x))

cat_col_names <- colnames(categorical_cols)
num_col_names <- colnames(numeric_cols)

# Setting categorical NAs as "NA" character, then setting all columns back to factors
categorical_cols <- apply(categorical_cols, 2, function(x) {
  ifelse(is.na(x), "NA", x)}) %>% as.data.frame(.) %>% 
  mutate_all(factor)

# Selecting numerical cols, and binding new cat_cols back on
full_set <- full_set %>% select(-cat_col_names) %>% cbind(categorical_cols)



# Finding numeric NAs
NA_cols_numeric <-apply(numeric_cols, 2, function(x) any(is.na(x)))
which(NA_cols_numeric == T)

# Filling in numerical NAs

# Distribution of the lot frontage
numeric_cols %>% ggplot(aes(LotFrontage)) +
  geom_histogram()
#Approx. Normal but one large outlier replace NAs with median
numeric_cols <- numeric_cols %>% 
  mutate(LotFrontage = ifelse(is.na(LotFrontage),
                              median(LotFrontage,na.rm = T), LotFrontage))


# Distribution of the Mas Vnr Area
numeric_cols %>% ggplot(aes(MasVnrArea)) +
  geom_histogram()
# Skewed right with a mode at 0, replace Nas with median (0)
numeric_cols <- numeric_cols %>% 
  mutate(MasVnrArea = ifelse(is.na(MasVnrArea),
                             0, MasVnrArea))


# Distribution of the Year the Garage was built
numeric_cols %>% ggplot(aes(GarageYrBlt)) +
  geom_histogram()
# Skewed left, replace with median
numeric_cols <- numeric_cols %>% 
  mutate(GarageYrBlt = ifelse(is.na(GarageYrBlt),
                              median(GarageYrBlt,na.rm = T), GarageYrBlt))
# One obvious outlier error (year 2593) to be changed to median
numeric_cols$GarageYrBlt[which(numeric_cols$GarageYrBlt > 2100)] <- median(numeric_cols$GarageYrBlt)
which(numeric_cols$GarageYrBlt > 2100)


# Since most of these disrtibutions are skewed, we will fill in remaining Nas with the median
numeric_cols <- apply(numeric_cols, 2, function(x) {
  ifelse(is.na(x), median(x, na.rm = T), x)}) %>% as.data.frame(.)
# No more Nas!
NA_cols_numeric <-apply(numeric_cols, 2, function(x) any(is.na(x)))
which(NA_cols_numeric == T)

# Adding columns back into the full set
full_set <- full_set %>% select(-num_col_names) %>% cbind(numeric_cols)






# Splitting back into training and testing sets
i <- 1:nrow(train)
train <- full_set[i,] %>% cbind(y)
test <- full_set[-i,]









# Function to calculate RMSE:
RMSE_log <- function(actual_price, predicted_price){
  sqrt(mean((log(actual_price) - log(predicted_price))^2))
}

RMSE <- function(actual_price, predicted_price){
  sqrt(mean((actual_price - predicted_price)^2))
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

RMSE(actual_prices$SalePrice, predictions_guess$SalePrice)

# Adding a column with centered SalePrice
train <- train %>% mutate(y_centered = y - mu)



# Principle component Analysis (numerical columns)
# x <- numeric_cols %>% select(-SalePrice, -Id) %>% as.matrix(.)
# pca <- prcomp(x)
# # Finding the percent variation explained by each PC
# var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
# plot(var_explained)
# # First 5 components are most important, after components after 10 are irrelevant
# pcs <- pca$x[, 1:5]
# plot(pcs[,4], y)
# fit_pca <- train(pcs, y, method = "lm")



# 
# fit_lm <- train(SalePrice~., method = "lm", data = numeric_cols)
# predictions_lm <- predict(fit_lm, test, type = "raw")
# RMSE(actual_prices$SalePrice, predictions_lm)
# 
# models <- c("rf",
#             "ranger","wsrf",  "avNNet", "mlp", "monmlp", "gbm", 
#             )
# 
# models <- c("Rborist", "glm",
            # "svmLinear", "gamLoess", "knn", "kknn")
# fits <- lapply(models, function(model){ 
#   print(model)
#   train(SalePrice ~ ., method = model, data = numeric_cols)
# }) 
# warnings()
# names(fits) <- models
# y_hat <- sapply(fits, function(x){
#   predict(x, test, type = "raw")})
# 
# apply(y_hat, 2, function(x) RMSE(actual_prices,x))
# RMSE(actual_prices, y_hat[,1])
# length(y_hat[,1])
# 







#   
# # Fitting a random forest model on categorical variables
# y_centered <- train$y_centered
# categorical_cols_train <- categorical_cols[i,] %>% cbind(y_centered)
# fit_cat_rf<- train(y_centered~., method = "Rborist", data = categorical_cols_train, verbose = T)
# fit_cat_rf$results$RMSE
# predictions_cat_rf <- predict(fit_cat_rf, test, type = "raw") + mu
# predictions_cat_rf <- data.frame(Id = test$Id, SalePrice = predictions_cat_rf)
# RMSE_log(actual_prices$SalePrice, predictions_cat_rf$SalePrice)
# 
# 
# cat_effect_test <- predictions_cat_rf$SalePrice - mu
# cat_effect_train  <- fit_cat_rf$finalModel$predictions
# # Exporting predictions
# write.csv(predictions_cat_rf, file = "predictions/predictions_cat_rf.csv", row.names = F)
# saveRDS(fit_cat_rf, file = "rds/fit_cat_ranger.Rds")
# 
# cat_effect_train  <- fit_cat_rf$finalModel$predictions



# Fitting a k-nearest neighboors model on the numerical variables
numeric_cols_train <- numeric_cols[i,] %>% cbind(y_centered)
fit_num_kknn <- train(y_centered~., method = "kknn",
                     data = numeric_cols_train)
fit_num_kknn$bestTune
predictions_kknn <- predict(fit_num_kknn, test, type = "raw") + mu 
predictions_kknn <- data.frame(Id = test$Id, SalePrice = predictions_kknn)
RMSE_log(actual_prices$SalePrice, predictions_kknn$SalePrice)
write.csv(predictions_kknn, file = "predictions/predictions_kknn.csv", row.names = F)
num_effect_train <- predict(fit_num_kknn, train, type = "raw") 
num_effect_test <- predictions_kknn$SalePrice - mu
y_minus_effects <- y_centered - num_effect_train

# RF 2, electric boogaloo
categorical_cols_train <- categorical_cols[i,] %>% cbind(y_minus_effects)
fit_cat_kknn_rf <- train(y_minus_effects~., method = "Rborist", data = categorical_cols_train, verbose = T)
fit_cat_kknn_rf$bestTune
predictions_cat_rf_2 <- predict(fit_cat_rf_2, test, type = "raw") + mu + num_effect_test
predictions_cat_rf_2 <- data.frame(Id = test$Id, SalePrice = predictions_cat_rf_2)
RMSE_log(actual_prices$SalePrice, predictions_cat_rf_2$SalePrice)
write.csv(predictions_cat_rf_2, file = "predictions/predictions_cat_rf_2.csv", row.names = F)
