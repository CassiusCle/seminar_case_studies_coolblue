library(dplyr)
library(caret)
library(fastDummies)
library(doParallel)
library(RSNNS)
library(xgboost)
library(Metrics)
library(MASS)
library(glmnet)

# Script that performs nested 10-fold Cross validation to compare the performance of three different models
# NB:   - This script might take a while to run
#       - This script requires the workspace resulting from running (5) to run properly

# Function that converts categorical variables in a data frame to a set of dummy variables
transform_dummy <- function (original, leave_one_out = FALSE){
  n <- ncol(original)
  dummy_df <- original %>% dummy_cols(., remove_first_dummy = leave_one_out)
  dummy_df <- dummy_df[,-(1:(n-2))]
  return(dummy_df)
}

# Function that normalises the values of a given vector
normalise <- function(vec){
  return( (vec-min(vec))/(max(vec)-min(vec)) )
}

# Function that Pre-Processes data
data_pp <- function(data){
  data_dummies <- data[, !names(data) %in% c('program_category_before', 'program_category_after')] %>% transform_dummy
  
  # Combine categories before and after in dummy encoding
  if(sum(names(data) %in% c('program_category_before', 'program_category_after')) != 0){
    program_category_dummies <- dummy_cols((data['program_category_before']))[,-1] + dummy_cols((data['program_category_after']))[,-1]
    colnames(program_category_dummies) <- c("program_category_Film",                     "program_category_Food",                    
                                            "program_category_Game",                     "program_category_Health / Lifestyle",      
                                            "program_category_Intellectual",             "program_category_Living / Home Decorating",
                                            "program_category_Music",                    "program_category_News",                    
                                            "program_category_Other Amusement",          "program_category_Other non-Fiction",       
                                            "program_category_Show",                     "program_category_Sport",                   
                                            "program_category_TV Series",                "program_category_Unknown",                 
                                            "program_category_Vehicles")
    program_category_dummies[program_category_dummies == 2] <- 1
    data_dummies <- cbind(data_dummies, program_category_dummies)
  }
  names(data_dummies) <- gsub(" ", "_", names(data_dummies))
  names(data_dummies) <- gsub("+", "", names(data_dummies))
  
  data_dummies$absolute_effect <- normalise(data_dummies$absolute_effect)
  data_dummies$relative_effect <- normalise(data_dummies$relative_effect)
  return(data_dummies)
}

# Linear CV Control and hyperparameter Grid
trcontrollin = trainControl(method = "cv",
                            number = 10,  
                            allowParallel = TRUE,
                            verboseIter = FALSE,
                            returnData = FALSE)

linGrid <- expand.grid(alpha = 1, 
                       lambda = (10^seq(-5, 0, length = 1000)))

# XGBoost CV Control and hyperparameter Grid
trcontrolXgb = trainControl(method = "cv",
                            number = 10,  
                            allowParallel = FALSE,
                            verboseIter = FALSE,
                            returnData = FALSE)

xgbGrid <- expand.grid(nrounds = c(50, 100, 200),
                       max_depth = c(3, 5, 10),
                       colsample_bytree = c(0.3, 0.5, 0.7),
                       eta = c(0.1),
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1)

# Neural Net CV Control and hyperparameter Grid
trcontrolNeural <-  trainControl(method = "cv",
                               number = 10,  
                               allowParallel = TRUE,
                               verboseIter = FALSE,
                               returnData = FALSE)

neuralGrid <- expand.grid(layer1 = c(3, 5, 10),
                               layer2 = c(3, 5, 10),
                               layer3 = c(3, 5, 10)
)

# Function to train XGBoost model
train_xgb <- function(y_train, X_train, control, grid, seed = -1){
  X_train <- X_train %>% xgb.DMatrix
  
  if (seed != -1){
    set.seed(seed) 
  }
  
  xgb_model = caret::train(
    X_train, y_train,  
    trControl = control,
    tuneGrid = grid,
    method = "xgbTree",
    metric = "RMSE",
    objective = "reg:squarederror"
  )
  return(xgb_model)
}

# Function to predict using trained XGBoost model
predict_xgb <- function(X_test, xgb_model){
  X_test <- X_test %>% xgb.DMatrix
  predicted <-  predict(xgb_model, X_test)
  return(predicted)
}

# Function to train Neural Network
train_neural <- function(y_train, X_train, control, grid, seed = -1){
  
  if (seed != -1){
    set.seed(seed) 
  }
  
  neural_modelRSNNS = caret::train(
    X_train, y_train,  
    trControl = control,
    tuneGrid = grid,
    method = "mlpML",
    metric = "RMSE",
    objective = "reg:squarederror"
  )
}

# Function to predict using trained Neural Network
predict_neural <- function(X_test, neural_model){
  return(predict(neural_model$finalModel, X_test))
}

# Function to fit Linear Model and perform variable selection
train_lin <- function(y_train, X_train, control, grid, seed = -1){
  if (seed != -1){
    set.seed(seed) 
  }
  
  lin_model <- caret::train(
    X_train, y_train,  
    trControl = control,
    method = "glmnet",
    metric = "RMSE",
    family="gaussian",
    objective = "reg:squarederror",
    trace = FALSE, # Supress trace
    tuneGrid = grid
  )
  return(lin_model)
}

# Function to predict using fitted Linear Model
predict_lin <- function(X_test, lin_model){
  return(predict(lin_model, X_test))
}

# Setup Parallel Backend to use many processors
cores <- detectCores()
cl <- makePSOCKcluster(cores[1]-1, outfile = "")
registerDoParallel(cl)

clusterCall(cl, function() {
  library(caret) 
  library(xgboost)
  library(RSNNS)
  library(MASS)
  library(glmnet)
})

# Function to perform nested k-fold cross-validation
nested_cv_prediction <- function(data, k = 10, absolute = FALSE){
  
  # Pre-process data
  data <- data_pp(data)
  
  # Split data into k folds
  folds <- split(sample(nrow(data)), nrow(data)*(1:k)/k )
  
  results <- foreach(i = 1:k,
                     .packages = c('caret', 'xgboost', 'RSNNS', 'dplyr'),
                     .export = c('train_lin','predict_lin','trcontrollin', 'linGrid',
                                 'train_xgb','predict_xgb','trcontrolXgb','xgbGrid',
                                 'train_neural','predict_neural','trcontrolNeural','neuralGrid'
                     ) ) %dopar% {
                       in_test <- folds[[i]]
                       train <- data[-in_test,]
                       test <- data[in_test,]
                       
                       X_train <- train[,-c(1:2)] %>% as.matrix 
                       X_test <- test[,-c(1:2)] %>% as.matrix
                       
                       if (absolute){
                         y_train <- train$absolute_effect
                         y_test <- test$absolute_effect
                       } else {
                         y_train <- train$relative_effect
                         y_test <- test$relative_effect
                       }
                       
                       # Linear Model
                       linmodel <- train_lin(y_train, X_train, trcontrollin, linGrid)
                       lin_pred <- predict_lin(X_test, linmodel)
                       print(paste('Fold',i,'linear complete.'))
                       
                       # XGBoost model
                       xgbmodel <- train_xgb(y_train, X_train, trcontrolXgb, xgbGrid)
                       xgb_pred <- predict_xgb(X_test, xgbmodel)
                       print(paste('Fold',i,'XGBoost complete.'))
                       
                       # Neural network
                       neuralmodel <- train_neural(y_train, X_train, trcontrolNeural, neuralGrid)
                       neural_pred <- predict_neural(X_test, neuralmodel)
                       print(paste('Fold',i,'neural network complete.'))
                       
                       return(list(actual = y_test,
                                   linear_pred = lin_pred, 
                                   linear_model = linmodel,
                                   xgb_pred = xgb_pred, 
                                   xgb_model = xgbmodel,
                                   neural_pred = neural_pred, 
                                   neural_model = neuralmodel
                       ))
                     }
  return(results)
}

# Function that calculates RMSE for given results
get_RMSE <- function(i, results){
  out <- (data.frame(results[[i]][["linear_pred"]],
              results[[i]][["xgb_pred"]],
              results[[i]][["neural_pred"]]
  ) - results[[i]][["actual"]]) %>% {.**2} %>% colMeans %>% sqrt
  names(out) <- c('RMSE_lin', 'RMSE_xgb', 'RMSE_neural')
  return(out)
}

# Function that return output for results
get_metrics_output <- function(k, results){
  metrics_output <- foreach(i = 1:k, .combine = rbind) %do% {
    c(get_RMSE(i, results))
  }
  return(metrics_output)
}

# Specify Cross Validation parameters
data_frames <- c('df_base','df_full')
y_var <- c('absolute', 'relative')
folds <- 10

results_agg <- data.frame(matrix(nrow = length(data_frames)*length(y_var), ncol = 5)) 
colnames(results_agg) <- c('variable_set','type','RMSE_LIN','RMSE_XGB','RMSE_NN')
l <- 1

# Set seed 
set.seed(1913)

# Run Cross-Validation
for(i in data_frames){
  data <- eval(parse(text = i)) 
  for(j in y_var){
    if (j == 'absolute'){
      absol <-  TRUE
    } else {
      absol <-  FALSE
    }
    print(paste('Starting with',i,j))
    results <- nested_cv_prediction(data, k = folds, absolute = absol)
    print(paste('Results',i,j,'computed'))
    assign(paste0('results_',i,'_',j), results)
    
    metrics <- get_metrics_output(folds, results)
    assign(paste0('metrics_',i,'_',j), metrics)
    
    results_agg[l,] <- c(i,j,metrics %>% colMeans)
    l <- l + 1
  }
}

# Only keep the following set from workspace, remove the rest
keep <- c(data_frames,'k', 'results_agg',
          "metrics_df_base_absolute", "metrics_df_base_relative", 
          "metrics_df_full_absolute", "metrics_df_full_relative",
          "results_df_base_absolute", "results_df_base_relative", 
          "results_df_full_absolute", "results_df_full_relative",
          'data_pp', 'train_lin', 'linGrid','trcontrollin', 
          'transform_dummy','normalise'
          )
rm(list=setdiff(ls(), keep))

