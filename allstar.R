install.packages("data.table")
install.packages("tidyverse")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("ranger")
install.packages("caret")
install.packages("vip")
library(data.table)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ranger)
library(caret)
library(vip)
regular_season <- fread("https://raw.githubusercontent.com/liamshelley09-droid/DATA-SCIENCE-CAMP/refs/heads/main/Regular_Season.csv")
regular_season$All_Star <- trimws(regular_season$All_Star)

regular_season$All_Star <- factor(
  regular_season$All_Star,
  levels = c("no", "yes")
)
model_data <- regular_season %>%
  select(All_Star, PTS, REB, AST, MIN, League_Avg_PPG) %>%
  #select(All_Star, PTS, REB, AST, MIN) %>%
  drop_na()
  
set.seed(777)

n <- nrow(model_data)

train_id <- sample(1:n, size = round(0.8 * n))

train <- model_data[train_id, ]
test  <- model_data[-train_id, ]



tree_model <- rpart(
  All_Star ~ PTS + REB + AST + MIN + League_Avg_PPG,
  #All_Star ~ PTS + REB + AST + MIN,
  data = train,
  method = "class"
)
tree_pred <- predict(
  tree_model,
  newdata = test,
  type = "class"
)
tree_accuracy <- mean(tree_pred == test$All_Star)

true_acc <- c()
i <- 1
for (i in 1:10)
{
  set.seed(i)
  train_id <- sample(1:n, size = round(0.8 * n))
  
  train <- model_data[train_id, ]
  test  <- model_data[-train_id, ]
  
  rf_model <- randomForest(
  All_Star ~ PTS + REB + AST + MIN + League_Avg_PPG,
  #All_Star ~ PTS + REB + AST + MIN,
  data = train,
  ntree = 500,
  importance = TRUE
)

rf_pred <- predict(
  rf_model,
  newdata = test
)

rf_accuracy <- mean(rf_pred == test$All_Star)
true_acc <- c(true_acc, rf_accuracy)
}
error <- round((1 - mean(true_acc)) * 100, digits = 2)
print(paste("Error Rate:", error, "%"))

rpart.plot(
  tree_model,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Decision Tree for NBA All Star"
)

vip(rf_model, num_features = 10, geom = "col", horizontal = FALSE)
