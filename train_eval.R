# --------------------------------------------------
# Train and Evaluate XGBoost Model
# https://www.rdocumentation.org/packages/xgboost/versions/1.2.0.1/topics/xgb.train
# 
# R version: 4.0.2
# Date: Nov 14, 2020
# Author: Jongmin Park (jijupax@gmail.com)
# --------------------------------------------------

library("xgboost")
library("caret")


model_train <- function(xgb_train,
                        lr,
                        depth,
                        iter) {
  # Train XGBoost model
  # Arguments
  #   xgb_train: (xgb.DMatrix) train data
  #   lr: (float) learning rate
  #   depth: (int) max depth of decision tree
  #   iter: (int) number of iteration of training
  # Return
  #   model: trained XGBoost model
  params <- list(objective="reg:squarederror", eval_metric="rmse")
  model <- xgb.train(params,
                     data=xgb_train,
                     eta=lr,
                     max.depth=depth,
                     nrounds=iter)
  print(model)

  return(model)
}


model_eval <- function(model,
                       xgb_test,
                       test_y) {
  # Evaluate XGBoost model and plot the predicted value
  # Arguments
  #   model: trained XGBoost model
  #   xgb_test: (xgb.DMatrix) test data
  #   test_y: (int) label of test data
  # Return
  #   None
  pred_y <- predict(model, xgb_test)

  # mse <- mean((test_y - pred_y)^2)
  # mae <- caret::MAE(test_y, pred_y)
  # rmse <- caret::RMSE(test_y, pred_y)
  # cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
  
  mse <- mean((exp(test_y) - exp(pred_y))^2)
  mae <- caret::MAE(exp(test_y), exp(pred_y))
  rmse <- caret::RMSE(exp(test_y), exp(pred_y))
  cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)
  
  x <- 1:length(test_y)
  title <- paste("eta = ", model$params$eta, ", depth = ", model$params$max_depth)
  plot(x, exp(test_y), col = "red", type = "l", main=title)
  lines(x, exp(pred_y), col = "blue", type = "l")
}
