# Black friday

set.seed(23)

#loading data
path_train <- "train.csv"
path_test <- "test.csv"

train <- read.csv(path_train , header = T)
test <- read.csv(path_test , header = T)

train[10][is.na(train[10])] <- 20
train[11][is.na(train[11])] <- 20

test[10][is.na(test[10])] <- 20
test[11][is.na(test[11])] <- 20

train$data <- 1
test$Purchase <- 0
test$data <- 0

total <- rbind(train,test)

for (i in 1:11)
{
  total[,i] <- as.factor(total[,i])
}

train.notmar <- total[total$Marital_Status == 0 ,] 
train.notmar <- train.notmar[train.notmar$data == 1,]
test.notmar <- total[total$data == 0 ,]

test.notmar$Purchase <- NULL
test.notmar$data <- NULL
train.notmar$data <- NULL

#Decision Tree
library(rpart)

model <- rpart(Purchase ~ .,data = train.notmar)
pred_tree <- predict(model, test.notmar)

submit <- data.frame(User_ID = test$User_ID,
                     Product_ID = test$Product_ID,
                     Purchase = pred_tree)

# XGboost
library(xgboost)

for (i in 1:12)
{
  train.notmar[,i] <-  as.numeric(train.notmar[,i])
}

for (i in 1:11)
{
  test.notmar[,i] <-  as.numeric(test.notmar[,i])
}

#Features 
X_features <- c( "User_ID" , "Product_ID" , "Gender" ,                   
                 "Age" , "Occupation" ,  "City_Category" ,           
                 "Stay_In_Current_City_Years" , "Product_Category_1" ,       
                 "Product_Category_2" , "Product_Category_3")
X_target <- train.notmar$Purchase

xgtrain <- xgb.DMatrix(data <- as.matrix(train.notmar[, X_features]), label = X_target, missing = NA)
xgtest <- xgb.DMatrix(data <- as.matrix(test.notmar[, X_features]), missing = NA)

#Setting Parameters
params <- list()
params$objective <- "reg:linear"
params$eta <- 0.23
params$max_depth <- 10
params$subsample <- 1
params$colsample_bytree <- 1
params$min_child_weight <- 2
params$eval_metric <- "rmse"

#Model building 
model_xgb <- xgb.train(params <- params, xgtrain, nrounds <- 100)

#checking important Features
vimp <- xgb.importance(model <- model_xgb, feature_names = X_features)

# Predicting 
pred_boost <- predict(model_xgb, xgtest)

# Submission
submit$Purchase_boosted <- pred_boost 
Final_submit <- submit

# Weighted Average of Decision tree and Boosting
Final_submit<-Final_submit[,-c(4)]
Final_submit$Purchase_1 <- (submit$Purchase + 2*submit$Purchase_boosted)/3
write.csv(Final_submit[,-c(3,4)], "result.csv", row.names = FALSE)