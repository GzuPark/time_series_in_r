# --------------------------------------------------
# Time Series Analysis - XGBoost
# used to UCI Diabetes Data Set
# https://archive.ics.uci.edu/ml/datasets/diabetes
# 
# R version: 4.0.2
# Date: Nov 14, 2020
# Author: Jongmin Park (jijupax@gmail.com)
# --------------------------------------------------


# --------------------------------------------------
# Prepare
# --------------------------------------------------

# clear variables
rm(list=ls())

# required packages
if (!require("xgboost")) install.packages("xgboost")  # v1.2.0.1
if (!require("caret")) install.packages("caret")  # v6.0-86
if (!require("dplyr")) install.packages("dplyr")  # v1.0.2
source("utils.R")
source("eda.R")
source("train_eval.R")

# load Diabetes dataset
# reference: UCI Diabetes Data Set
# https://archive.ics.uci.edu/ml/datasets/diabetes
df <- preprocess()



# --------------------------------------------------
# EDA
# --------------------------------------------------

description(df)

# plot manually
diabetes_plot(df, 1)
diabetes_plot(df, 30)
diabetes_plot(df, 70)




# --------------------------------------------------
# Convert data
# --------------------------------------------------

# split train & test data
train <- df[df$ID != 70, ]
test <- df[df$ID == 70, ]

# reference: select
# https://www.rdocumentation.org/packages/dplyr/versions/0.7.8/topics/select
train_x <- data.matrix(dplyr::select(train, ID, DATE, TIME, CODE, NO))
train_y <- train$VALUE
# train_y <- normalize(train$VALUE, train$VALUE)
# tmp <- denormalize(train_y, train$VALUE)
# summary(tmp)
# summary(train$VALUE)

test_x <- data.matrix(dplyr::select(test, ID, DATE, TIME, CODE, NO))
test_y <- test$VALUE
# test_y <- normalize(test$VALUE, train$VALUE)
# tmp <- denormalize(test_y, train$VALUE)
# summary(tmp)
# summary(test$VALUE)



# --------------------------------------------------
# XGBoost
# https://www.rdocumentation.org/packages/xgboost/versions/1.2.0.1
# --------------------------------------------------

# reference: xgb.DMatrix
# https://www.rdocumentation.org/packages/xgboost/versions/1.2.0.1/topics/xgb.DMatrix
xgb_train <- xgb.DMatrix(data=train_x, label=train_y)
xgb_test <- xgb.DMatrix(data=test_x, label=test_y)

# train model

set.seed(42)  # reproducibility

m1 <- model_train(xgb_train, lr=0.025, depth=4, iter=1000)
model_eval(m1, xgb_test, test_y)

m2 <- model_train(xgb_train, lr=0.025, depth=3, iter=1000)
model_eval(m2, xgb_test, test_y)

m3 <- model_train(xgb_train, lr=0.04, depth=3, iter=1000)
model_eval(m3, xgb_test, test_y)

m4 <- model_train(xgb_train, lr=0.05, depth=3, iter=1000)
model_eval(m4, xgb_test, test_y)
