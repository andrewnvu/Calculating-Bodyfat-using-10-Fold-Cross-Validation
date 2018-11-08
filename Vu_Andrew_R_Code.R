# Andrew Vu
# Prof. Panangadan
# CPSC 375 Project 1
# R code listing

#class obtains predict function 
library(class)

#Metrics obtains mean squared error function (mse)
library(Metrics)

#dropped density variable, unused
mod1 <- Bodyfatproject[,2:15]

#created 15 vectors to store the mse's for 10 folds
mseFolds1 <- vector("numeric", 10)
mseFolds2 <- vector("numeric", 10)
mseFolds3 <- vector("numeric", 10)
mseFolds4 <- vector("numeric", 10)
mseFolds5 <- vector("numeric", 10)
mseFolds6 <- vector("numeric", 10)
mseFolds7 <- vector("numeric", 10)
mseFolds8 <- vector("numeric", 10)
mseFolds9 <- vector("numeric", 10)
mseFolds10 <- vector("numeric", 10)
mseFolds11 <- vector("numeric", 10)
mseFolds12 <- vector("numeric", 10)
mseFolds13 <- vector("numeric", 10)
mseFolds14 <- vector("numeric", 10)
mseFolds15 <- vector("numeric", 10)

#cuts my dataset into almost evenly folds
folds <- cut(seq(1,nrow(mod)) ,breaks = 10, labels = FALSE)


###############################################MODEL_1##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm(bodyfat ~ Age + Weight + Height + Neck, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds1[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds1, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model1: Mean Squared Error Plot using
     Age, Weight, Height, Neck")

#obtain the mean of the 10 mse
mean(mseFolds1)
#[1] 51.77905


###############################################MODEL_2##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ Age +Weight + Height + Neck + Chest + Abdomen, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds2[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds2, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 2: Mean Squared Error Plot using
     Age, Weight, Height, Neck, Chest, and Abdomen")

mean(mseFolds2)
#[1] 22.55222



###############################################MODEL_3##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ Hip +Thigh + Knee + Ankle + Biceps + Forearm + Wrist, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds3[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds3, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 3: Mean Squared Error Plot using
     Hip, Thigh, Knee, Ankle, Biceps, Forearm, and Wrist")

mean(mseFolds3)
#[1] 48.89543


###############################################MODEL_4##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ Age  + Height  + Chest  + Hip +  Knee  + Biceps  + Wrist, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds4[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds4, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 4: Mean Squared Error Plot using
      Age, Height, Chest, Hip, Knee, Biceps and Wrist")

mean(mseFolds4)
#[1] 35.63102



###############################################MODEL_5##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ Age +Weight + Chest + Abdomen + Knee 
                + Ankle + Biceps + Forearm + Wrist, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds5[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds5, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 5: Mean Squared Error Plot using
     Age, Weight, Chest, Abdomen, Knee, Ankle, Biceps, Forearm, and Wrist")

mean(mseFolds5)
#[1] 35.63102



###############################################MODEL_6##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ Age + Weight + Height, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds6[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds6, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 6: Mean Squared Error Plot using
     Age, Weight, Height")

mean(mseFolds6)
#[1] 47.40973

###############################################MODEL_7##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ Age +Weight + Height + Neck + Chest + Abdomen + 
                  Hip + Thigh + Ankle + Biceps + Forearm, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds7[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds7, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 7: Mean Squared Error Plot using
     Age, Weight, Height, Neck, Chest, Abdomen, Hip, Thigh,
     Ankle, Biceps, and Forearms")

mean(mseFolds7)
#[1] 22.30133


###############################################MODEL_8##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ Weight + Height + Neck + Chest + Abdomen + 
                  Hip + Thigh + Knee + Ankle + Biceps + Forearm + Wrist, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds8[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds8, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 8: Mean Squared Error Plot using
     Weight, Height, Neck, Chest, Abdomen, Hip, Thigh,
     Knee, Ankle, Biceps, Forearms, and Wrist")

mean(mseFolds8)
#[1] 21.65944


###############################################MODEL_9##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ Weight + Height + Neck + Chest + Abdomen + 
                  Hip + Thigh + Knee + Ankle + Biceps,data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds9[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds9, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 9: Mean Squared Error Plot using
     Weight, Height, Neck, Chest, Abdomen, Hip, Thigh,
     Knee, Ankle, and Biceps")

mean(mseFolds9)
#[1] 22.12311




###############################################MODEL_10##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~  Height + Neck + Chest + Abdomen + 
                  Hip + Thigh + Knee + Forearm + Wrist, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds10[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds10, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 10: Mean Squared Error Plot using
     Height, Neck, Chest, Abdomen, Hip, Thigh,
     Knee, Ankle, Forearm and Wrist")

mean(mseFolds10)
#[1] 21.88004



###############################################MODEL_11##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~  Height + Neck + Knee + Ankle + Biceps + Forearm + Wrist, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds11[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds11, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 11: Mean Squared Error Plot using
     Height, Neck, Knee, Ankle, Biceps, Forearm and Wrist")

mean(mseFolds11)
#[1] 52.06536



###############################################MODEL_12##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ Age + Weight + Height + Biceps + Forearm + Wrist, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds12[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds12, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 12: Mean Squared Error Plot using
     Age, Weight, Height, Biceps, Forearm, and Wrist")

mean(mseFolds12)
#[1] 45.26175



###############################################MODEL_13##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ Forearm + Wrist, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds13[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds13, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 13: Mean Squared Error Plot using
     Forearm, and Wrist")

mean(mseFolds13)
#[1] 63.85075


###############################################MODEL_14##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm( bodyfat ~ log(Weight) + Height + Neck + log(Chest) + Abdomen + 
                  log(Hip) + Thigh + Knee + log(Forearm) + Wrist, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds14[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds14, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 14: Mean Squared Error Plot using
     log(Weight), Height, Neck, log(Chest), Abdomen, log(Hip), Thigh,
     Knee, log(Forearm) and Wrist")

mean(mseFolds14)
#[1] 23.51331



###############################################MODEL_15##################################################
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- mod[testIndexes, ]
  trainData <- mod[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  
  #obtain the the lm model using my train data for the first fold and so on
  #1st instance it'll be rows:24-225
  models <- lm(bodyfat ~  log(Height) + Neck + Chest + log(Abdomen) + 
                  Hip + Thigh + Knee + log(Forearm) + Wrist, data = trainData)
  #use my models or training data to predict values for my rows: 1-23
  predictions <- predict(models, testData)
  #use my predictions that i obtain and compare it to the actual values in rows 1-23
  mseFolds15[i] <- mse(testData$bodyfat, predictions)
}

plot(mseFolds15, xlab = "Folds", ylab = "Mean Squared Errors", 
     main = "Model 15: Mean Squared Error Plot using
     log(Height), Neck, Chest, log(Abdomen), Hip, Thigh,
     Knee,log(Forearm) and Wrist")

mean(mseFolds15)
#[1] 21.64384


