library(e1071)
library(performanceEstimation)

train <- read.csv("training.csv")
test <- read.csv("sorted_test.csv")
soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")

train$Depth <- as.numeric(train$Depth)
test$Depth <- as.numeric(test$Depth)

X_train <- cbind(train[,2:3595])
X_test <- cbind(test[,2:3595])

# take the first derivatives to smoothe out the measurement noise
# training data
MIR_measurements <- train[, 2400:3579]
MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
X_train <- cbind(train[, 3580:3595], MIR_DER[, -1])

# testing data
MIR_measurements <- test[, 2400:3579]
MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
X_test <- cbind(test[, 3580:3595], MIR_DER[, -1])




predictions <- rep(NA, dim(test)[1])
for(soil_property in soil_properties){
  
  print(soil_property)
  sp.col <- which(names(train)==soil_property)
  
  train.d1 <- cbind(X_train,train[ ,sp.col])
  
  model.svm <- svm(train.d1[,-ncol(train.d1)], train.d1[,ncol(train.d1)], cost=100, gamma=0.0001)
  pred1 <- predict(model.svm, X_test)
  
  model.svm <- svm(train.d1[,-ncol(train.d1)], train.d1[,ncol(train.d1)], cost=10, gamma=0.001)
  pred2 <- predict(model.svm, X_test)
  
  model.svm <- svm(train.d1[,-ncol(train.d1)], train.d1[,ncol(train.d1)], cost=1000, gamma=0.0001)
  pred3 <- predict(model.svm, X_test)
  
  model.svm <- svm(train.d1[,-ncol(train.d1)], train.d1[,ncol(train.d1)], cost=100, gamma=0.001)
  pred4 <- predict(model.svm, X_test)
  
  all_pred <-cbind(pred1,pred2,pred3,pred4)
  pred.svm <- rowMeans(all_pred)
  
  predictions <- cbind(predictions, pred.svm)  
  
}

predictions <- predictions[,-1]
colnames(predictions) <-  soil_properties
write.csv(cbind(PIDN= as.character(test[,1]), predictions), "predictions.csv", row.names=FALSE)


predictions <- rep(NA, dim(test)[1])
for(soil_property in soil_properties){
  
  print(soil_property)
  sp.col <- which(names(train)==soil_property)
    
  coef <- switch(soil_property,
                 "Ca" = 0,
                 "P" = 5,
                 "pH" = 1,
                 "SOC" = 1,
                 "Sand" = 1)
  
  nunum <- switch(soil_property,
                 "Ca" = 0.5,
                 "P" = 0.2,
                 "pH" = 0.6,
                 "SOC" = 0.2,
                 "Sand" = 0.4)
  deg <- switch(soil_property,
                "Ca" = 1,
                "P" = 4,
                "pH" = 4,
                "SOC" = 2,
                "Sand" = 3)
  
  model.svm <- svm(X_train,train[ ,sp.col], cost=100, gamma=0.0001,type="nu-regression", nu=nunum, kernel="polynomial", degree=deg, cachesize=100, coef0=coef)
  pred1 <- predict(model.svm, X_test)
  predictions <- cbind(predictions, pred1)  
  
}

predictions <- predictions[,-1]
colnames(predictions) <-  soil_properties
write.csv(cbind(PIDN= as.character(test[,1]), predictions), "predictions.csv", row.names=FALSE)


predictions <- rep(NA, dim(test)[1])
for(soil_property in soil_properties){
  
  print(soil_property)
  sp.col <- which(names(train)==soil_property)
  
  
  model.svm <- svm(X_train,train[ ,sp.col], cost=100, gamma=0.0001,type="nu-regression", nu=0.2, kernel="polynomial", degree=2, cachesize=100, coef0=1)
  pred1 <- predict(model.svm, X_test)
  predictions <- cbind(predictions, pred1)  
  
}

predictions <- predictions[,-1]
colnames(predictions) <-  soil_properties
write.csv(cbind(PIDN= as.character(test[,1]), predictions), "predictions.csv", row.names=FALSE)
