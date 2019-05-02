
# Packages ----------------------------------------------------------------


#install packages
install.packages('corrplot')
install.packages('neuralnet')
install.packages("mlr")
install.packages("mlbench")
install.packages("tidyverse")
install.packages("GGally")
install.packages("dummies")
# Libraries --------------------------------------------------

#libraries
library(caret)
library(mlbench)
library("corrplot")
library(neuralnet)
library(C50)
library(gmodels)
library(rpart)
library(rpart.plot)
library("mlr")
library(GGally)
library(dummies)



# Data cleaning -----------------------------------------------------------

cars <- read.csv("C:\\Users\\Fran\\Documents\\Bsc\\3BSC\\3bsc_pendrive\\FirstSem\\ACI\\Assignment\\ACI-A02\\Q01\\cars.csv", sep=",")

str(cars)

names(cars) <- c("BuyingPrice", "PriceOfMaintenance", "Doors", "Capacity", 
                     "SizeOfBoot", "EstimatedSafety", "class")

summary(cars)


#checks for na's
apply(cars,2,function(x) sum(is.na(x)))


#Convert all to numeric for corrplot usage.
cars$BuyingPrice <- as.numeric(cars$BuyingPrice)
cars$PriceOfMaintenance <- as.numeric(cars$PriceOfMaintenance)
cars$SizeOfBoot <- as.numeric(cars$SizeOfBoot)
cars$EstimatedSafety <- as.numeric(cars$EstimatedSafety)
cars$class <- as.numeric(cars$class)
cars$Capacity <- as.numeric(cars$Capacity)
cars$Doors <- as.numeric(cars$Doors)

#Corrplot
corrplot(cor(cars), method="circle")

#Change back to factors
cars$class <- factor(cars$class)
levels(cars$class)

# Decision tree - Sampling ------------------------------------------------

#Sampling
set.seed(123)
cars.rows <- nrow(cars)
cars.sample70 <- sample(cars.rows, cars.rows*0.7)
cars.sample75 <- sample(cars.rows, cars.rows*0.75)
cars.sample80 <- sample(cars.rows, cars.rows*0.8)

#Train and test
cars.train_dt70 <- cars[cars.sample70,]
cars.test_dt70 <- cars[-cars.sample70,]
cars.train_dt75 <- cars[cars.sample75,]
cars.test_dt75 <- cars[-cars.sample75,]
cars.train_dt80 <- cars[cars.sample80,]
cars.test_dt80 <- cars[-cars.sample80,]


# Decision tree - Prediction ----------------------------------------------

cars.model_dt70 <- C5.0(cars.train_dt70[-7],cars.train_dt70$class, trials=10)
summary(cars.model_dt70)

cars.model_dt75 <- C5.0(cars.train_dt75[-7],cars.train_dt75$class, trials=10)
summary(cars.model_dt75)

cars.model_dt80 <- C5.0(cars.train_dt80[-7],cars.train_dt80$class, trials=10)
summary(cars.model_dt80)

cars.predict_dt70 <- predict(cars.model_dt70, cars.test_dt70[-7])
cars.predict_dt75 <- predict(cars.model_dt75, cars.test_dt75[-7])
cars.predict_dt80 <- predict(cars.model_dt80, cars.test_dt80[-7])

#Best 
cars.best_model_dt <- C5.0(cars.train_dt75[-7],cars.train_dt75$class, trials=20, winnow=FALSE, model="rules")
cars.predict_best_model_dt <- predict(cars.best_model_dt, cars.test_dt75)




#¬

#Summary of each of the above models

summary(cars.model_dt70)
C5imp(cars.model_dt70,metric='usage')

summary(cars.model_dt75)
C5imp(cars.model_dt75,metric='usage')

summary(cars.model_dt80)
C5imp(cars.model_dt80,metric='usage')

summary(cars.best_model_dt)
C5imp(cars.model_dt80,metric='usage')

#¬


#Prediction and accuracy for different test and train
#Predict 70
table_mat_dt70 <- table(cars.test_dt70$class, cars.predict_dt70)
table_mat_dt70

#Accuracy 70
accuracy_Test_dt70 <- sum(diag(table_mat_dt70)) / sum(table_mat_dt70) #sum of diagonal/sum of matrix
print(paste('Accuracy for test', accuracy_Test_dt70)) #98%

#¬

#Predict 75
table_mat_dt75 <- table(cars.test_dt75$class, cars.predict_dt75)
table_mat_dt75

#Accuracy 75
accuracy_Test_dt75 <- sum(diag(table_mat_dt75)) / sum(table_mat_dt75) #sum of diagonal/sum of matrix
print(paste('Accuracy for test', accuracy_Test_dt75)) #99%

#¬

#Predict 80
table_mat_dt80 <- table(cars.test_dt80$class, cars.predict_dt80)
table_mat_dt80

#Accuracy 80
accuracy_Test_dt80 <- sum(diag(table_mat_dt80)) / sum(table_mat_dt80) #sum of diagonal/sum of matrix
print(paste('Accuracy for test', accuracy_Test_dt80)) #97%



#¬


# Decision tree -Visualize ------------------------------------------------

fit <- rpart(class~., data = cars.train_dt70, method = 'class')
rpart.plot(fit, extra = 106)

fit <- rpart(class~., data = cars.train_dt75, method = 'class')
rpart.plot(fit, extra = 106)

fit <- rpart(class~., data = cars.train_dt80, method = 'class')
rpart.plot(fit, extra = 106)




# Decision tree - CrossTables and conf matrix ------------------------------------------------

#70
CrossTable(cars.test_dt70$class, cars.predict_dt70,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
1-(2 + 1 + 3 +2 + 2)/519
#98%
cfMatrix_dt_70 <- confusionMatrix(cars.test_dt70$class, cars.predict_dt70)
cfMatrix_dt_70$byClass


#¬

#75
CrossTable(cars.test_dt75$class, cars.predict_dt75,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
1-(1 + 3)/432
#99% ¬¬(Best one)¬¬
cfMatrix_dt_75 <- confusionMatrix(cars.test_dt75$class, cars.predict_dt75)
cfMatrix_dt_75


#¬

#80
CrossTable(cars.test_dt80$class, cars.predict_dt80,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
1-(5+1+1)/346
#97%

cfMatrix_dt_80 <- confusionMatrix(cars.test_dt80$class, cars.predict_dt80)
cfMatrix_dt_80$overall

#¬


# Decision tree - Metrics And Tuning -------------------------------------------------------------

#Finding best configuration of model using accuracy metric
control <- trainControl(method="repeatedcv", number=5, repeats=5)
set.seed(123)
fit.c50 <- caret::train(class~., data=cars, method="C5.0", metric='Accuracy', trControl=control)
fit.c50 #The final values used for the model were trials = 20, model = rules and winnow = FALSE which obtained accuracy of 99%.
trellis.par.set(caretTheme())
plot(fit.c50, metric='Accuracy') 

#Inbuilt (in caret): KAPPA Metric
control_ROC <- trainControl(method="cv", number=5, classProbs=TRUE)
set.seed(123)
fit.c50_Kappa <- caret::train(class~., data=cars, method="C5.0", metric='Kappa', trControl=control)
fit.c50_Kappa #The final values used for the model were trials = 20, model = rules and winnow = FALSE which obtained kappa of 97%, accuracy of 99%.
trellis.par.set(caretTheme())
plot(fit.c50_Kappa, metric='Kappa') 

#Function to create metrics
CreateMetrics <- function(confMat) {
  #accuracy
  accuracyClass1 <- confMat$byClass[1,11] 
  accuracyClass2 <- confMat$byClass[2,11] 
  accuracyClass3 <- confMat$byClass[3,11] 
  accuracyClass4 <- confMat$byClass[4,11]
  
  
  #FAR: FP/(FP + TN) = 1 - specificity
  #so im getting the specifity from conf matrix and doing 1-specifiy for each class
  farClass1 <- 1 - confMat$byClass[1,2] 
  farClass2 <- 1 - confMat$byClass[2,2] 
  farClass3 <- 1 - confMat$byClass[3,2] 
  farClass4 <- 1 - confMat$byClass[4,2]
  
  #FRR: FRR = FNR = FN/(TP+FN) = 1 - recall
  #same as above explanation
  frrClass1 <- 1 - confMat$byClass[1,6] # 
  frrClass2 <- 1 - confMat$byClass[2,6] # 
  frrClass3 <- 1 - confMat$byClass[3,6] #
  frrClass4 <- 1 - confMat$byClass[4,6] # 
  
  #precision
  precisionClass1 <- confMat$byClass[1,5] # 
  precisionClass2 <- confMat$byClass[2,5] # 
  precisionClass3 <- confMat$byClass[3,5] #
  precisionClass4 <- confMat$byClass[4,5] # 
  
  #recal
  recallClass1 <- confMat$byClass[1,6] # 
  recallClass2 <- confMat$byClass[2,6] # 
  recallClass3 <- confMat$byClass[3,6] #
  recallClass4 <- confMat$byClass[4,6] # 
  
  resultClass1 <- data.frame(accuracyClass1,farClass1,frrClass1, precisionClass1, recallClass1)
  resultClass2 <- data.frame(accuracyClass2, farClass2,frrClass2, precisionClass2, recallClass2)
  resultClass3 <- data.frame(accuracyClass3, farClass3,frrClass3, precisionClass3, recallClass3)
  resultClass4 <- data.frame(accuracyClass4,farClass4,frrClass4, precisionClass4, recallClass4)
  my_list <- list("class 1 metrics " = resultClass1,
                  "class 2 metrics " = resultClass2,
                  "class 3 metrics " = resultClass3,
                  "class 4 metrics " = resultClass4)
  return(my_list)
  
}
# Decision tree - Metric usage on each model ----------------------------------------------

#Model 1 with metrics
table_mat_1 <- table(cars.predict_model_dt_1, cars.predict_model_dt_1)
table_mat_1

model1_result <- CreateMetrics(cfMatrix_dt_70)
model1_result

#¬

#Model 2 with metrics
table_mat_2 <- table(cars.test_dt75$class, cars.predict_model_dt_2)
table_mat_2

model2_result <- CreateMetrics(cfMatrix_dt_75)
model2_result

#¬

#Model 3 with metrics
table_mat_3 <- table(cars.test_dt80$class, cars.predict_model_dt_3)
table_mat_3

model3_result <- CreateMetrics(cfMatrix_dt_80)
model3_result


#¬


#Model 4 (best model) with metrics


table_mat_dt_best_model <- table(cars.test_dt75$class, cars.predict_best_model_dt)
table_mat_dt_best_model

summary(cars.best_model_dt)
C5imp(cars.best_model_dt,metric='usage')

best_model_result <- CreateMetrics(cfMatrix_dt_75)
best_model_result

#Accuracy Metric
accuracy_Test_dt_best_model <- sum(diag(table_mat_dt_best_model)) / sum(table_mat_dt_best_model) #sum of diagonal/sum of matrix
print(paste('Accuracy for test', accuracy_Test_dt_best_model)) #99%


#****

# Neural Network - Normalize ---------------------------------------------

normalize <- function(x) {
  return (
    (x-min(x)) / (max(x)-min(x))
  )
}


carsNN <- cars
carsNN$BuyingPrice <- as.numeric(as.factor(carsNN$BuyingPrice))
carsNN$PriceOfMaintenance <- as.numeric(as.factor(carsNN$PriceOfMaintenance))
carsNN$SizeOfBoot <- as.numeric(as.factor(carsNN$SizeOfBoot))
carsNN$EstimatedSafety <- as.numeric(as.factor(carsNN$EstimatedSafety))
carsNN$class <- carsNN$class
carsNN$Capacity <- as.numeric(as.factor(carsNN$Capacity))
carsNN$Doors <- as.numeric(as.factor(carsNN$Doors))


newCols <- cbind(carsNN[,-7], dummy(carsNN$class))
colnames(newCols) <- c("BuyingPrice", "PriceOfMaintenance", "Doors", "Capacity", 
                        "SizeOfBoot", "EstimatedSafety", "acc", "good", "unacc", "vgood")


# Neural Network - Normalize - 70% ---------------------------------------------


set.seed(1)
carsNN_norm.rows <- nrow(newCols)
carsNN_norm.sample <- sample(carsNN_norm.rows, carsNN_norm.rows*0.70)

cars_train_nn <- newCols[carsNN_norm.sample,]
cars_test_nn <- newCols[-carsNN_norm.sample,]

cars_nn_Model <- neuralnet(acc + good + unacc + vgood~BuyingPrice+PriceOfMaintenance+Doors+Capacity+SizeOfBoot+EstimatedSafety
                           ,data=cars_train_nn,
                           hidden=c(2,4), linear.output = FALSE, threshold = 0.1)
plot(cars_nn_Model)

model_results <- compute(cars_nn_Model, cars_test_nn[,1:6])
p <- model_results$net.result

original_values_1 <- max.col(cars_test_nn[, 7:10])

predictedmax <- max.col(p)
mean(predictedmax == original_values_1)
result_comp <- data.frame(cbind(original_values_1, predictedmax))
result_comp$match <- result_comp$predictedmax == result_comp$original_values_1

table(result_comp$original_values_1, result_comp$predictedmax)

CrossTable(result_comp$original_values_1, result_comp$predictedmax,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
actual <- factor(result_comp$original_values_1)
predicted <- factor(result_comp$predictedmax)
nnc <- confusionMatrix(actual,predicted)
nnc$overall #74%

CreateMetrics <- function(confMat) {
  #accuracy
  accuracyClass1 <- confMat$byClass[1,11] 
  accuracyClass2 <- confMat$byClass[2,11] 
  accuracyClass3 <- confMat$byClass[3,11] 
  accuracyClass4 <- confMat$byClass[4,11]
  
  #FAR: FP/(FP + TN) = 1 - specificity
  #so im getting the specifity from conf matrix and doing 1-specifiy for each class
  farClass1 <- 1 - confMat$byClass[1,2] 
  farClass2 <- 1 - confMat$byClass[2,2] 
  farClass3 <- 1 - confMat$byClass[3,2] 
  farClass4 <- 1 - confMat$byClass[4,2]
  
  #FRR: FRR = FNR = FN/(TP+FN) = 1 - recall
  #same as above explanation
  frrClass1 <- 1 - confMat$byClass[1,6] # 
  frrClass2 <- 1 - confMat$byClass[2,6] # 
  frrClass3 <- 1 - confMat$byClass[3,6] #
  frrClass4 <- 1 - confMat$byClass[4,6] # 
  
  #precision
  precisionClass1 <- confMat$byClass[1,5] # 
  precisionClass2 <- confMat$byClass[2,5] # 
  precisionClass3 <- confMat$byClass[3,5] #
  precisionClass4 <- confMat$byClass[4,5] # 
  
  #recal
  recallClass1 <- confMat$byClass[1,6] # 
  recallClass2 <- confMat$byClass[2,6] # 
  recallClass3 <- confMat$byClass[3,6] #
  recallClass4 <- confMat$byClass[4,6] # 
  
  resultClass1 <- data.frame(accuracyClass1,farClass1,frrClass1, precisionClass1, recallClass1)
  resultClass2 <- data.frame(accuracyClass2, farClass2,frrClass2, precisionClass2, recallClass2)
  resultClass3 <- data.frame(accuracyClass3, farClass3,frrClass3, precisionClass3, recallClass3)
  resultClass4 <- data.frame(accuracyClass4,farClass4,frrClass4, precisionClass4, recallClass4)
  my_list <- list("class 1 metrics " = resultClass1,
                  "class 2 metrics " = resultClass2,
                  "class 3 metrics " = resultClass3,
                  "class 4 metrics " = resultClass4)
  return(my_list)
  
}


#Model 1 with metrics
table_mat_1 <- table(result_comp$original_values_1, result_comp$predictedmax)
table_mat_1

model1_result <- CreateMetrics(nnc)
model1_result


# Neural Network - Normalize - 60% ---------------------------------------------
set.seed(1)
carsNN_norm.rows <- nrow(newCols)
carsNN_norm.sample <- sample(carsNN_norm.rows, carsNN_norm.rows*0.60)

cars_train_nn <- newCols[carsNN_norm.sample,]
cars_test_nn <- newCols[-carsNN_norm.sample,]

cars_nn_Model <- neuralnet(acc + good + unacc + vgood~BuyingPrice+PriceOfMaintenance+Doors+Capacity+SizeOfBoot+EstimatedSafety
                           ,data=cars_train_nn,
                           hidden=c(6,6,4), linear.output = FALSE, threshold = 0.1)
plot(cars_nn_Model)

model_results <- compute(cars_nn_Model, cars_test_nn[,1:6])
p <- model_results$net.result

original_values_1 <- max.col(cars_test_nn[, 7:10])

predictedmax <- max.col(p)
mean(predictedmax == original_values_1)
result_comp <- data.frame(cbind(original_values_1, predictedmax))
result_comp$match <- result_comp$predictedmax == result_comp$original_values_1

table(result_comp$original_values_1, result_comp$predictedmax)

CrossTable(result_comp$original_values_1, result_comp$predictedmax,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE)
actual <- factor(result_comp$original_values_1)
predicted <- factor(result_comp$predictedmax)
nnc <- confusionMatrix(actual,predicted)
nnc$overall #95%

#Model 2 with metrics
table_mat_1 <- table(result_comp$original_values_1, result_comp$predictedmax)
table_mat_1

model2_result <- CreateMetrics(nnc)
model2_result

