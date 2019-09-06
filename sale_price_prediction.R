################################################################################
################################################################################

# SET UP THE PROJECT ENVIRONMENT

# If required packages are not installed, install them

if(!require(tidyverse)) {install.packages("tidyverse")}
if(!require(lubridate)) {install.packages("lubridate")}
if(!require(data.table)) {install.packages("data.table")}
if(!require(caret)) {install.packages("caret")}
if(!require(Hmisc)) {install.packages("Hmisc")}
if(!require(cowplot)) {install.packages("cowplot")}
if(!require(ggrepel)) {install.packages("ggrepel")}
if(!require(corrplot)) {install.packages("corrplot")}
if(!require(RColorBrewer)) {install.packages("RColorBrewer")}
if(!require(knitr)) {install.packages("knitr")}
if(!require(kableExtra)) {install.packages("kableExtra")}
if(!require(tinytex)) {install.packages("tinytex")}

# Needed for several ML models from the caret package

if(!require(plyr)) {install.packages("plyr")}
if(!require(penalized)) {install.packages("penalized")}
if(!require(kernlab)) {install.packages("kernlab")}
if(!require(mboost)) {install.packages("mboost")}
if(!require(gam)) {install.packages("gam")}
if(!require(kknn)) {install.packages("kknn")}
if(!require(gbm)) {install.packages("gbm")}
if(!require(bst)) {install.packages("bst")}
if(!require(xgboost)) {install.packages("xgboost")}
if(!require(randomForest)) {install.packages("randomForest")}

# Clear the environment and any plots

rm(list = ls())

if(!is.null(dev.list())) { dev.off() }

# Set working directory

# setwd(file.path(Sys.getenv("HOME"), "Desktop/data_science_HarvardX/house_price_prediction/"))

################################################################################
################################################################################

# FUNCTIONS

# Define essential functions

RMSE <- function(true_ratings, predicted_ratings) {
  
  sqrt(mean((true_ratings - predicted_ratings)^2))
  
}

# Data transformation function

# This combines all the data wrangling and modification methods/techniques
# used on the training set throughout the project i.e. replacing NAs,
# modifying columns etc.
# It is used to convert the test set in the same format as the
# training set in order to be able to make predictions using different ML models.
# These ML models were fit on the training set after it had been modified and hence
# they could not be used with the original test set to make predictions
# as the test set would
# be in a different format to the modified training set that the ML models were fit onto.

# NOTE: This function should be run only after machine learning models have been fitted.
# This is because at some points, the function compares variables in num_train
# to variables in test and uses the differences to properly rename/modify feature entries.
# Hence, for this comparison and modifications to work,
# num_train must be in its final format (which will be after ML model fitting).

modify_data <- function(to_convert, original, final) {
  
  # to_convert: the data frame that is to be converted
  # original: the original working template
  # final: the final/modified working template
  # original and final will be used to make appropriate modification to to_convert
  
  # Remove GarageYrBlt column
  
  to_convert <- to_convert %>% select(-GarageYrBlt)
  
  # Detect columns with NAs
  
  na_values <- map_df(.x = colnames(to_convert), .f = function(c) {
    
    num_na <- sum(is.na(to_convert[[c]]))
    
    if (num_na != 0) {
      
      data.frame(col = c,
                 na_n = num_na,
                 na_pct = mean(is.na(to_convert[[c]])),
                 stringsAsFactors = FALSE)
      
    } # if
    
  }) # map_df
  
  # Replace NAs as in the training set
  
  for (na_col in na_values$col) {
    
    if (na_col == "PoolQC") {
      
      to_convert[which(is.na(to_convert$PoolQC)), "PoolQC"] <- "None"
      
    } else if (na_col == "MiscFeature") {
      
      to_convert[which(is.na(to_convert$MiscFeature)), "MiscFeature"] <- "None"
      
    } else if (na_col == "Alley") {
      
      to_convert[which(is.na(to_convert$Alley)), "Alley"] <- "None"
      
    } else if (na_col == "Fence") {
      
      to_convert[which(is.na(to_convert$Fence)), "Fence"] <- "None"
      
    } else if (na_col == "FireplaceQu") {
      
      to_convert[which(is.na(to_convert$FireplaceQu)), "FireplaceQu"] <- "None"
      
    } else if (na_col == "LotFrontage") {
      
      to_convert[which(is.na(to_convert$LotFrontage)), "LotFrontage"] <-
        mean(to_convert$LotFrontage[!is.na(to_convert$LotFrontage)])
      
    } else if (na_col == "MasVnrType") {
      
      to_convert[which(is.na(to_convert$MasVnrType)), "MasVnrType"] <-
        names(which.max(table(to_convert$MasVnrType)))
      
    } else if (na_col == "MasVnrArea") {
      
      to_convert[which(is.na(to_convert$MasVnrArea)), "MasVnrArea"] <- 0
      
    } else if (na_col == "Electrical") {
      
      to_convert[which(is.na(to_convert$Electrical)), "Electrical"] <-
        names(which.max(table(to_convert$Electrical)))
      
    } else if (na_col %in%
               na_values$col[str_detect(string = na_values$col,
                                        pattern = "^Garage(?!Y|A|.*Cars$)")]) {
      
      to_convert[which(is.na(to_convert[[na_col]])), na_col] <- "None"
      
    } else if (na_col %in%
               na_values$col[str_detect(string = na_values$col,
                                        pattern = "^Bsmt(?!FinSF\\d|.*SF$|.*Bath$)")]) {
      
      to_convert[which(is.na(to_convert[[na_col]])), na_col] <- "None"
      
    } else{
      
      if (class(to_convert[[na_col]]) == "character") {
        
        to_convert[which(is.na(to_convert[[na_col]])), na_col] <-
          names(which.max(table(to_convert[[na_col]])))
        
      } else if (class(to_convert[[na_col]]) == "numeric") {
        
        to_convert[which(is.na(to_convert[[na_col]])), na_col] <-
          as.numeric(which.max(table(to_convert[[na_col]])))
        
      } # nested else if
      
    } # else
    
  } # loop
  
  # Rename colnames to avoid any that start with a digit
  
  colnames(to_convert)[str_which(string = colnames(to_convert),
                                 pattern = "^\\d")][1] <- "FirstFlrSF"
  
  colnames(to_convert)[str_which(string = colnames(to_convert),
                                 pattern = "^\\d")][1] <- "SecondFlrSF"
  
  colnames(to_convert)[str_which(string = colnames(to_convert),
                                 pattern = "^\\d")][1] <- "ThreeSsnPorch"
  
  if (any(grepl(pattern = "^\\d", colnames(to_convert)))) {
    warning("ERROR! Check that there are no column names that start with a digit!")
  }
  
  # Get columns with numeric values
  
  numeric_cols <- sapply(X = colnames(to_convert), FUN = function(c) {
    
    ifelse(test = class(to_convert[[c]]) == "numeric", yes = TRUE, no = FALSE)
    
  })
  
  # Isolate columns with numeric values
  
  num_test <- to_convert[, numeric_cols]
  
  # Isolate columns with character values
  
  chr_test <- to_convert[, !numeric_cols]
  
  # Get columns with quality/condition information
  
  qu_cols <- colnames(chr_test)[str_detect(string = colnames(chr_test),
                                           pattern = ".Q.|.Cond$")]
  
  # Convert character ratings to numeric ones - these will still be categorical features
  
  for (c in qu_cols) {
    
    chr_test[[c]] <- replace(x = chr_test[[c]], list = chr_test[[c]] == "Ex", values = 5)
    
    chr_test[[c]] <- replace(x = chr_test[[c]], list = chr_test[[c]] == "Gd", values = 4)
    
    chr_test[[c]] <- replace(x = chr_test[[c]],  list = chr_test[[c]] == "TA", values = 3)
    
    chr_test[[c]] <- replace(x = chr_test[[c]], list = chr_test[[c]] == "Fa", values = 2)
    
    chr_test[[c]] <- replace(x = chr_test[[c]], list = chr_test[[c]] == "Po", values = 1)
    
    chr_test[[c]] <- replace(x = chr_test[[c]],
                             list = !chr_test[[c]] %in% c(1, 2, 3, 4, 5), values = 0)
    
    chr_test[[c]] <- factor(x = as.numeric(chr_test[[c]]))
    
  } # loop
  
  # Add the (now) numeric columns of chr_test to num_test
  
  num_test <- to_convert[, numeric_cols] %>%
    bind_cols(chr_test[, qu_cols])
  
  # Modify some of the character value columns as they were modified in the training set
  
  updated_cols <- c("MSZoning", "Neighborhood", "MasVnrType", "Foundation",
                    "BsmtExposure", "Electrical", "GarageType", "GarageFinish")
  
  for (col in updated_cols) {
    
    replacement <- data.frame(filter(.data = original, GarageCars < 4) %>% select(col),
                              new = final[[col]],
                              stringsAsFactors = FALSE) %>%
      distinct() %>%
      mutate_at(col, .funs = factor)
    
    for (i in 1:nrow(replacement)) {
      
      chr_test[[col]] <- replace(x = chr_test[[col]],
                                 list = chr_test[[col]] == replacement[i, col],
                                 values = replacement[i, "new"])
      
    } # nested loop
    
  } # loop
  
  # Add the new numeric columns of chr_test to num_test
  
  num_test <- to_convert[, numeric_cols] %>%
    bind_cols(chr_test[, qu_cols]) %>%
    bind_cols(chr_test[, updated_cols]) %>%
    mutate_all(.funs = as.numeric) %>%
    select(-c(Id))
  
  # Filtering and feature engineering based on the correlogram
  
  # Combine columns to create new features
  
  # Then remove the old columns
  
  num_test <- num_test %>%
    mutate(AgeSold = YrSold - YearBuilt) %>%
    select(-c(YrSold, YearBuilt, MoSold)) %>%
    mutate(Pool = PoolArea * PoolQC) %>%
    select(-c(PoolArea, PoolQC))
  
  # Filter out some data
  
  num_test <- num_test %>%
    # filter(GarageCars < 4) %>% # no need to filter out these in the test set
    select(-c(ThreeSsnPorch, MiscVal, BsmtHalfBath,
              OverallCond, ExterCond, BsmtFinSF2,
              LowQualFinSF))
  
  if (colnames(final)[which(!colnames(final) %in% colnames(num_test))] == "SalePrice") {
    
    return(num_test)
    
  } else {
    
    warning("ERROR! Data modification was not successful!")
    
  }
  
} # function

################################################################################
################################################################################

# DATA WRANGLING

# Get data from Kaggle
# URL: https://www.kaggle.com/c/house-prices-advanced-regression-techniques

train <- read_csv(file = "data/train.csv")

test <- read_csv(file = "data/test.csv")

# Check for NA values

na_values <- map_df(.x = colnames(train), .f = function(c) {
  
  num_na <- sum(is.na(train[[c]]))
  
  if (num_na != 0) {
    
    data.frame(col = c,
               na_n = num_na,
               na_pct = mean(is.na(train[[c]])),
               stringsAsFactors = FALSE)
    
  } # if
  
}) # map_df

# Replace NA values

# The PoolQC column lists the pool quality

train[which(is.na(train$PoolQC)), "PoolQC"] <- "None"

# MiscFeature lists any additional miscellaneous features

train[which(is.na(train$MiscFeature)), "MiscFeature"] <- "None"

# The Alley column lists the type of alley access to property

train[which(is.na(train$Alley)), "Alley"] <- "None"

# The Fence column lists the fence quality

train[which(is.na(train$Fence)), "Fence"] <- "None"

# The FireplaceQu column lists the fireplace quality

train[which(is.na(train$FireplaceQu)), "FireplaceQu"] <- "None"

# The LotFrontage column lists the linear feet of street connected to property

train[which(is.na(train$LotFrontage)), "LotFrontage"] <-
  mean(train$LotFrontage[!is.na(train$LotFrontage)])

# Fix any columns referring to garage features

for (g in na_values$col[str_detect(string = na_values$col, pattern = "^Garage[^Y]")]) {
  train[which(is.na(train[[g]])), g] <- "None"
}

# Fix any columns referring to basement features

for (b in na_values$col[str_detect(string = na_values$col, pattern = "^Bsmt")]) {
  train[which(is.na(train[[b]])), b] <- "None"
}

# MasVnrType lists the masonry veneer type - assume it takes the most common value

train[which(is.na(train$MasVnrType)), "MasVnrType"] <- names(which.max(table(train$MasVnrType)))

# MasVnrArea lists the masonry veneer area in square feet - replace NAs with 0
# since above the same exact indices for MasVnrType were replaced with "None"

train[which(is.na(train$MasVnrArea)), "MasVnrArea"] <- 0

# Electrical column has 1 NA value - assume it takes the most common value

train[which(is.na(train$Electrical)), "Electrical"] <- names(which.max(table(train$Electrical)))

# Check again for any NAs

na_values <- map_df(.x = colnames(train), .f = function(c) {
  
  num_na <- sum(is.na(train[[c]]))
  
  if (num_na != 0) {
    
    data.frame(col = c,
               na_n = num_na,
               na_pct = mean(is.na(train[[c]])),
               stringsAsFactors = FALSE)
    
  } # if
  
}) # map_df

# Remove any columns with NAs (for now)

train <- train %>%
  select(-c(na_values$col))

# Rename colnames to avoid any that start with a digit

colnames(train)[str_which(string = colnames(train), pattern = "^\\d")]

colnames(train)[str_which(string = colnames(train), pattern = "^\\d")][1] <- "FirstFlrSF"

colnames(train)[str_which(string = colnames(train), pattern = "^\\d")][1] <- "SecondFlrSF"

colnames(train)[str_which(string = colnames(train), pattern = "^\\d")][1] <- "ThreeSsnPorch"

if (any(grepl(pattern = "^\\d", colnames(train)))) {
  warning("ERROR! Check that there are no column names that start with a digit!")
}

################################################################################
################################################################################

# DUMMY CODING, FEATURE MODIFICATION AND FEATURE ENGINEERING

# Transform the SalePrice column to follow a normal distribution

train$SalePrice <- log1p(train$SalePrice)

# Get columns with numeric values

numeric_cols <- sapply(X = colnames(train), FUN = function(c) {
  
  ifelse(test = class(train[[c]]) == "numeric", yes = TRUE, no = FALSE)
  
})

# Isolate columns with numeric values

num_train <- train[, numeric_cols]

# Some features, although of class numeric, can be considered categorical

cols_ignore <- c("MSSubClass", "OverallQual", "OverallCond",
                 "BsmtFullBath", "BsmtHalfBath", "FullBath",
                 "HalfBath", "BedroomAbvGr", "KitchenAbvGr",
                 "TotRmsAbvGrd", "Fireplaces", "GarageCars",
                 "MiscVal", "YearRemodAdd", "MoSold",
                 "YrSold", "YearBuilt")

# Isolate columns with character values but also include the SalePrice column

chr_train <- bind_cols(train[, !numeric_cols], train[, "SalePrice"]) %>% data.frame()

# Get columns with quality/condition information

qu_cols <- colnames(chr_train)[str_detect(string = colnames(chr_train), pattern = ".Q.|.Cond$")]

# Convert character ratings to numeric ones - these will still be categorical features

for (c in qu_cols) {
  
  chr_train[[c]] <- replace(x = chr_train[[c]], list = chr_train[[c]] == "Ex", values = 5)
  
  chr_train[[c]] <- replace(x = chr_train[[c]], list = chr_train[[c]] == "Gd", values = 4)
  
  chr_train[[c]] <- replace(x = chr_train[[c]],  list = chr_train[[c]] == "TA", values = 3)
  
  chr_train[[c]] <- replace(x = chr_train[[c]], list = chr_train[[c]] == "Fa", values = 2)
  
  chr_train[[c]] <- replace(x = chr_train[[c]], list = chr_train[[c]] == "Po", values = 1)
  
  chr_train[[c]] <- replace(x = chr_train[[c]],
                            list = !chr_train[[c]] %in% c(1, 2, 3, 4, 5), values = 0)
  
  chr_train[[c]] <- factor(x = as.numeric(chr_train[[c]]))
  
} # loop

# Add the (now) numeric columns of chr_train to num_train

num_train <- train[, numeric_cols] %>%
  bind_cols(chr_train[, qu_cols])

# Modify some character containing columns - dummy coding

chr_train <- chr_train %>%
  mutate(MSZoning = reorder(x = factor(x = chr_train$MSZoning),
                            X = SalePrice, FUN = median)) %>%
  mutate(Neighborhood = reorder(x = factor(x = chr_train$Neighborhood),
                                X = SalePrice, FUN = median)) %>%
  mutate(MasVnrType = reorder(x = factor(x = chr_train$MasVnrType),
                              X = SalePrice, FUN = median)) %>%
  mutate(Foundation = reorder(x = factor(x = chr_train$Foundation),
                              X = SalePrice, FUN = median)) %>%
  mutate(BsmtExposure = reorder(x = factor(x = chr_train$BsmtExposure),
                                X = SalePrice, FUN = median)) %>%
  mutate(Electrical = reorder(x = factor(x = chr_train$Electrical),
                              X = SalePrice, FUN = median)) %>%
  mutate(GarageType = reorder(x = factor(x = chr_train$GarageType),
                              X = SalePrice, FUN = median)) %>%
  mutate(GarageFinish = reorder(x = factor(x = chr_train$GarageFinish),
                                X = SalePrice, FUN = median))

chr_train$MSZoning <- plyr::mapvalues(x = chr_train$MSZoning,
                                      from = levels(chr_train$MSZoning),
                                      to = seq(1, length(levels(chr_train$MSZoning)), 1))

chr_train$Neighborhood <- plyr::mapvalues(x = chr_train$Neighborhood,
                                          from = levels(chr_train$Neighborhood),
                                          to = seq(1, length(levels(chr_train$Neighborhood)), 1))

chr_train$MasVnrType <- plyr::mapvalues(x = chr_train$MasVnrType,
                                        from = levels(chr_train$MasVnrType),
                                        to = seq(1, length(levels(chr_train$MasVnrType)), 1))

chr_train$Foundation <- plyr::mapvalues(x = chr_train$Foundation,
                                        from = levels(chr_train$Foundation),
                                        to = seq(1, length(levels(chr_train$Foundation)), 1))

chr_train$BsmtExposure <- plyr::mapvalues(x = chr_train$BsmtExposure,
                                          from = levels(chr_train$BsmtExposure),
                                          to = seq(1, length(levels(chr_train$BsmtExposure)), 1))

chr_train$Electrical <- plyr::mapvalues(x = chr_train$Electrical,
                                        from = levels(chr_train$Electrical),
                                        to = seq(1, length(levels(chr_train$Electrical)), 1))

chr_train$GarageType <- plyr::mapvalues(x = chr_train$GarageType,
                                        from = levels(chr_train$GarageType),
                                        to = seq(1, length(levels(chr_train$GarageType)), 1))

chr_train$GarageFinish <- plyr::mapvalues(x = chr_train$GarageFinish,
                                          from = levels(chr_train$GarageFinish),
                                          to = seq(1, length(levels(chr_train$GarageFinish)), 1))

updated_cols <- c("MSZoning", "Neighborhood", "MasVnrType",
                  "Foundation", "BsmtExposure", "Electrical",
                  "GarageType", "GarageFinish")

# Add the new numeric columns of chr_train to num_train and remove the Id column

num_train <- train[, numeric_cols] %>%
  bind_cols(chr_train[, qu_cols]) %>%
  bind_cols(chr_train[, updated_cols]) %>%
  mutate_all(.funs = as.numeric) %>%
  select(-c(Id))

# Filtering and feature engineering based on the correlogram

# Combine columns to create new features

# Then remove the old columns

num_train <- num_train %>%
  mutate(AgeSold = YrSold - YearBuilt) %>%
  select(-c(YrSold, YearBuilt, MoSold)) %>%
  mutate(Pool = PoolArea * PoolQC) %>%
  select(-c(PoolArea, PoolQC))

# Filter out some data

num_train <- num_train %>%
  filter(GarageCars < 4) %>%
  select(-c(ThreeSsnPorch, MiscVal, BsmtHalfBath,
            OverallCond, ExterCond, BsmtFinSF2,
            LowQualFinSF))

################################################################################
################################################################################

# MACHINE LEARNING

# Initialize several lists to store useful ML outputs

fits <- list()

preds <- list()

rmses <- list()

durations <- list()

# 10-fold cross-validation to be used with the train() function

set.seed(1)

control <- trainControl(method = "cv", number = 10, p = 0.9)

# Define ML models

models <- c("svmLinear", "glmboost", "lm", "penalized", "gamLoess",
            "gbm", "kknn", "xgbLinear", "rf", "xgbTree", "bstTree")

# Fit models

for (m in models) {
  
  # modelLookup(m)
  
  print(m)
  
  before <- Sys.time()
  
  set.seed(1)
  
  if (m == "kknn") {
    
    kknn_params <- expand.grid(kmax = seq(15, 25, 2),
                               distance = 2,
                               kernel = "optimal")
    
    fits[[m]] <- train(SalePrice ~ ., data = num_train, method = m,
                       trControl = control, tuneGrid = kknn_params)
    
  } else if (m == "gamLoess") {
    
    gamLoess_params <- expand.grid(span = seq(0.15, 0.75, 0.05), degree = 1)
    
    fits[[m]] <- train(SalePrice ~ ., data = num_train, method = m,
                       trControl = control, tuneGrid = gamLoess_params)
    
  } else if (m == "rf") {
    
    rf_params <- data.frame(mtry = seq(11, 19, 2), stringsAsFactors = FALSE)
    
    fits[[m]] <- train(SalePrice ~ ., data = num_train, method = m,
                       trControl = control, tuneGrid = rf_params)
    
  } else if (m == "bstTree") {
    
    bstTree_params <- expand.grid(maxdepth = c(2, 3, 4),
                                  nu = 0.1,
                                  mstop = seq(100, 200, 50))
    
    fits[[m]] <- train(SalePrice ~ ., data = num_train, method = m,
                       trControl = control, tuneGrid = bstTree_params)
    
  } else {
    
    fits[[m]] <- train(SalePrice ~ ., data = num_train, method = m, trControl = control)
    
  } # else
  
  preds[[m]] <- predict(object = fits[[m]], newdata = num_train)
  
  rmses[[m]] <- RMSE(preds[[m]], num_train$SalePrice)
  
  rmses[[m]]
  
  after <- Sys.time()
  
  durations[[m]] <- difftime(after, before, units = 'mins')
  
  durations[[m]]
  
} # loop

# Ensemble model

# Ensemble predictions using models with an RMSE <= max_rmse

max_rmse <- 0.1

best_models <- which(rmses <= max_rmse)

best_preds <- preds[best_models]

model <- "ensemble"

all_preds <- do.call(what = cbind, args = best_preds)

preds[[model]] <- rowMeans(all_preds)

rmses[[model]] <- RMSE(preds[[model]], num_train$SalePrice)

durations[[model]] <- do.call(what = sum, args = durations[best_models])

# ML model fitting results

rmse_results <- data.frame(model = c(models, "ensemble"),
                           RMSE = do.call(what = rbind, args = rmses),
                           time = do.call(what = rbind, args = durations),
                           stringsAsFactors = FALSE)

################################################################################
################################################################################

# MAKE PREDICTIONS

# Make predictions of SalePrice on the test set using the ML models fitted above

# Transform the test set into an appropriate format (similar to that of the training set)

new_test <- modify_data(to_convert = test, original = train, final = num_train)

# Make predictions of SalePrice on the test set using all the fitted ML models

submissions <- list()

for (m in models) {
  
  log_trans_preds <- predict(object = fits[[m]], newdata = new_test)
  
  submissions[[m]] <- expm1(log_trans_preds) # expm1 to reverse the SalePrice log1p transformation
  
} # loop

# Ensemble predictions using models with RMSE <= max_rmse on the test set

ensemble_preds <- submissions[best_models]

all_ensemble_preds <- do.call(what = cbind, args = ensemble_preds)

submissions[["ensemble"]] <- rowMeans(all_ensemble_preds)

################################################################################
################################################################################

# CREATE A TABLE WITH THE BEST PREDICTIONS TO SUBMIT TO KAGGLE

final_submission <- data.frame(Id = test$Id,
                               SalePrice = submissions[[m]],
                               stringsAsFactors = FALSE)

################################################################################
################################################################################
