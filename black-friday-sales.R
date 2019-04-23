train.df <- read.csv("train.csv", header = TRUE)
test.df <- read.csv("test.csv", header = TRUE) 

dim(train.df)
dim(test.df)

train.df

any(is.na(train.df))

names(which(sapply(train.df, anyNA)))

summary(train.df)
summary(test.df)

train.df[10][is.na(train.df[10])] <- 20
train.df[11][is.na(train.df[11])] <- 20

test.df[10][is.na(test.df[10])] <- 20
test.df[11][is.na(test.df[11])] <- 20

sum(is.na(train.df))
sum(is.na(test.df))

X_train = train.df[3:11]
Y_train = train.df[12]

X_test = test.df[3:11]  

library(data.table)
library(mltools)

X_train <- one_hot(as.data.table(X_train))
X_test <- one_hot(as.data.table(X_test))

X_train$Product_Category_1 = scale(X_train$Product_Category_1)
X_train$Product_Category_2 = scale(X_train$Product_Category_2)
X_train$Product_Category_3 = scale(X_train$Product_Category_3)

X_test$Product_Category_1 = scale(X_test$Product_Category_1)
X_test$Product_Category_2 = scale(X_test$Product_Category_2)
X_test$Product_Category_3 = scale(X_test$Product_Category_3)

X_train$Purchases <- Y_train

library(rpart)

model = rpart(Purchases~.,data = X_train)

Y_pred = predict(model, X_test)

df <- data.frame(User_ID = test.df$User_ID, Product_ID = test.df$Product_ID, Purchase = Y_pred)

head(df)

write.csv(df,"result.csv",row.names = FALSE)
