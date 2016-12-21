## Installing Required Packages
require(caret)
require(rattle)
require(rpart)
require(C50)
require(ggplot2)
require(ggpubr)
require(ROCR)
require(rpart.plot)
require(CHAID)#not installing#
install.packages("caret")
install.packages("rattle")
install.packages("rpart")
install.packages("C50")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ROCR")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("dplyr")
install.packages("mice")
install.packages("randomForest")
install.packages("glmnet")
install.packages("readr")
install.packages('tree')
install.packages('ISLR')
install.packages("gmodels")
library(rpart)
library(caret)
library(rattle)
library(C50)
library(ggplot2)
library(ggpubr)
library(ROCR)
library(rpart.plot)
library(e1071)
library(dplyr)
library(mice)
library(randomForest)
library(ggplot2)
library(glmnet)
library(readr) 
library(ISLR)
library(tree)
library(gmodels)
library(Hmisc)


## Reading in the data file.

diabetes <- read.csv("diabetes.csv",stringsAsFactors = F)
View(diabetes)

##Checking in the summary of the dataset and also ther structure

str(diabetes)
summary(diabetes)

## Checking in the missing values if any.
table(is.na(diabetes))
## No missing FALSE  6912

# Checking number of cols and rows (768 and 9 Columns)

dim(diabetes)

## Checking the proportion of target variable
table(diabetes$Outcome)

## 34.89% target rate 268 1s and 500 0's

## For classification tree converting the integer outcome to a factor.

diabetes$Outcome <- as.factor(diabetes$Outcome)

# Using the create Data Partition in caretpackage to split data into training and testing
set.seed(2)
intrain <- createDataPartition(y = diabetes$Outcome, p=0.67, list = F)
training <- diabetes[intrain,]
testing <- diabetes[-intrain,]
dim(training)
dim(testing)


##Checking the event rate in training and testing sample

table(training$Outcome)
## 34.95% target rate in training 180 1's and 335 0's

table(testing$Outcome)
## 34.78% target rate in training 88 1's and 165 0's


#################################################################################################
### Training the model using the rpart pacakage using the decision tree CART
#################################################################################################

Outcome.fit1 <- rpart(Outcome ~.,data = training,control = c(minsplit = 30,cp=0))
fancyRpartPlot(Outcome.fit1,cex = 0.5)
Outcome.predict <-predict(Outcome.fit1,testing,type="class")
table(testing$Outcome,Outcome.predict)
confusionMatrix(testing$Outcome,Outcome.predict) 
printcp(Outcome.fit1)



Outcome.prune1 <- prune(Outcome.fit1, cp=   Outcome.fit1$cptable[which.min(Outcome.fit1$cptable[,"xerror"]),"CP"])
printcp(Outcome.prune1)
fancyRpartPlot(Outcome.prune1, cex = 0.5)
Outcome.predict <- predict(Outcome.prune1,testing,type="class")
confusionMatrix(testing$Outcome,Outcome.predict)


#or#

tc <- trainControl("cv",10)
train.rpart <- train(Outcome ~., data=training, method="rpart",trControl=tc)
Outcome.predict1 <- predict(train.rpart,testing)
confusionMatrix(testing$Outcome,Outcome.predict1)
fancyRpartPlot(train.rpart$finalModel)

printcp(Outcome.fit1)
plotcp(Outcome.fit1)
printcp(Outcome.prune1)
plotcp(Outcome.prune1)

pred1 <- prediction(predict(train.rpart,training,type="prob")[,2],training$Outcome)
plot(performance(pred1, "tpr", "fpr"))
abline(0, 1, lty = 2)

pred <- prediction(predict(train.rpart,testing,type="prob")[,2],testing$Outcome)
plot(performance(pred, "tpr", "fpr"))
abline(0, 1, lty = 2)

auc_train  <- performance(pred1,"auc")

auc_test <- performance(pred,"auc")

auc_train
auc_test

-------------------------------------------------------------------------------
  

  Outcome.c50 = C5.0(training$Outcome ~.,data=training)
Outcome.c50
plot(Outcome.c50,,cex = .7)
Outcome.predict1 <-predict(Outcome.c50,testing)
confusionMatrix(testing$Outcome,Outcome.predict1) 
or
# cross tabulation of predicted versus actual classes
CrossTable(testing$Outcome, Outcome.predict1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
summary(Outcome.predict1)
summary(Outcome.c50)


Outcome.c50_boost = C5.0(training$Outcome ~.,data=training[,-ncol(training)], trials = 10)
Outcome.c50_boost
summary(Outcome.c50_boost)
plot(Outcome.c50_boost)
Outcome_boost.predict1 <-predict(Outcome.c50_boost,testing)
confusionMatrix(testing$Outcome,Outcome_boost.predict1)
or
CrossTable(testing$Outcome, Outcome_boost.predict1,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


------------------------------------------------------------
  
  
  
  ##read the data
  diabetes1 <- read.csv("diabetes.csv",stringsAsFactors = F) 

##observe the data structure
str(diabetes1)

##observe variables characteristic
## variables-pregnanacy, glucose,blood presure,skinthickness,insulin & bmi got zeros
## except pregnancy & outcome, all other variables zero values as to be treated as missing values
describe(diabetes1)

##convert outcome as factor
diabetes1$Outcome <- as.factor((diabetes1$Outcome))

colnames(diabetes1) <- c("Pregnancies","Glucose","BloodPressure","SkinThickness",
                         "Insulin ","BMI ","DiabetesPedigreeFunction ","Age","Outcome")

# Check if there has NA in the diabetes1
print(all(!is.na(diabetes1)))

# Treat 0 in the biological variables other than number of times pregnant as missing values 
cols_change <- colnames(diabetes1)[!colnames(diabetes1) %in% c("Pregnancies", "Outcome")]
bool_data <- diabetes1[cols_change] == 0
diabetes1[cols_change][bool_data] <- NA

# Show the number of missing values of each column
print(apply(bool_data, 2, sum))

# Set a random seed
set.seed(123)
# Split the dataset: 80% for trainging and 20% for testing
intrain <- createDataPartition(y = diabetes1$Outcome, p=0.55, list = F)
training1 <- diabetes1[intrain,]
testing1 <- diabetes1[-intrain,]

#check dimensions
dim(training1)
dim(testing1)

#verify frequncy of outcome in training data
#34.95% of ones
table(training1$Outcome)

#verify frequncy of outcome in training data
#34.64% of ones
table(testing1$Outcome)

# Show scatterplot matrix on the training data
pairs(~.,data=diabetes1[intrain,], main="Scatterplot Matrix of Training data")

# Median value imputation
diabetes1$Glucose[is.na(diabetes1$Glucose)] <- median(diabetes1$Glucose,na.rm = T)
diabetes1$BloodPressure[is.na(diabetes1$BloodPressure)] <- median(diabetes1$BloodPressure,na.rm = T)
diabetes1$`BMI `[is.na(diabetes1$`BMI `)] <- median(diabetes1$`BMI `,na.rm = T)
diabetes1$SkinThickness[is.na(diabetes1$SkinThickness)] <- median(diabetes1$SkinThickness,na.rm = T)
diabetes1$`Insulin `[is.na(diabetes1$`Insulin `)] <- median(diabetes1$`Insulin `,na.rm = T)


# Multiple imputation-NOT WORKING VERIFY
#mice_mod <- mice(diabetes1[, c("SkinThickness","Insulin")], method='rf') 

# Show distributions 
par(mfrow=c(2,2))
hist(diabetes1$SkinThickness, freq=F, main=' skin fold thickness ',
     col='darkgreen', ylim=c(0,0.04))

hist(diabetes1$`Insulin `, freq=F, main=' insulin',
     col='darkblue', ylim=c(0,0.004))



# Make sure there is no missing data
sum(is.na(diabetes1))
describe(diabetes1)

# Visualize the relationship between age and diabetes on training data
ggplot(data=diabetes1[intrain,], aes(x = age, fill = Outcome)) +
  geom_bar(stat='count', position='dodge') +
  ggtitle("age VS diabetes") +
  labs(x = 'Age')

# Normalize training data
scale_training <- as.data.frame(scale(diabetes1[intrain, -9],  
                                      center = TRUE, scale = TRUE))

scale_training$Outcome <- diabetes1[intrain, "Outcome"]

str(scale_training)
describe(scale_training)

#Find the Optimal Subset for Random Forest
bestmtry <- tuneRF(scale_training[, c(-9)],scale_training$Outcome, ntreeTry=300, 
                   stepFactor=2,improve=0.05, trace=TRUE, plot=TRUE, dobest=FALSE)

names(scale_training) <- make.names(names(scale_training))
head(scale_training,10)

#oob error least at mtry = 2
rf_model3 <- randomForest(scale_training$Outcome ~ . ,
            data = scale_training, ntree=500, importance = TRUE  ,mtry=2)

# Output classfiaction error rate
plot(rf_model3,ylim = c(0, 0.5))
legend('bottomright', colnames(rf_model3$err.rate), col=1:3, fill=1:3)

print(rf_model3$err.rate[nrow(rf_model3$err.rate),])

# Output variables inportance graph
varImpPlot(rf_model3)

#Accuracy for random forest model14:
1 - rf_model3$err.rate[nrow(rf_model3$err.rate), 1]

print(rf_model3$err.rate)
print(rf_model3$confusion)

-------------------------
  
  #validation
  
  # Normalize testing data
  scale_testing <- as.data.frame(scale(diabetes1[-intrain, -9],  
                                        center = TRUE, scale = TRUE))
  
scale_testing$Outcome <- diabetes1[-intrain, "Outcome"]

str(scale_testing)
describe(scale_testing)

#Find the Optimal Subset for Random Forest
bestmtry1 <- tuneRF(scale_testing[, c(-9)],scale_testing$Outcome, ntreeTry=300, 
                   stepFactor=2,improve=0.05, trace=TRUE, plot=TRUE, dobest=FALSE)

names(scale_testing) <- make.names(names(scale_testing))
head(scale_testing,10)

#oob error least at mtry = 2
rf_model4 <- randomForest(scale_testing$Outcome ~ . ,
                          data = scale_testing, ntree=500, importance = TRUE  ,mtry=2)

# Output classfiaction error rate
plot(rf_model4,ylim = c(0, 0.5))
legend('bottomright', colnames(rf_model4$err.rate), col=1:3, fill=1:3)

print(rf_model4$err.rate[nrow(rf_model4$err.rate),])

# Output variables inportance graph
varImpPlot(rf_model4)

#Accuracy for random forest model14-testing:
1 - rf_model4$err.rate[nrow(rf_model4$err.rate), 1]

print(rf_model4$err.rate)
print(rf_model4$confusion)


------------------------------------------

#logistic for training data
cvfit = cv.glmnet(as.matrix(scale_training[, c(-9)]), scale_training$Outcome, 
                  family = "binomial", type.measure = "class")

# Show the cefficients of the best model
coef(cvfit, s = "lambda.min")

lg_p = predict(cvfit, newx = as.matrix(scale_training[, c(-9)]), 
               s = "lambda.min", type = "class")

# Show confusion matrix
(lg_result <- table(lg_p, scale_training$Outcome))

# Overall error rate and accuracy
overall_accuracy <- (lg_result[1] +  lg_result[4]) / sum(lg_result) 
overall_error <- 1 - overall_accuracy

# Error rate in class 0
error_c0 <- lg_result[2] / (lg_result[1] +  lg_result[2])

# Error rate in class 1
error_c1 <- lg_result[3] / (lg_result[3] +  lg_result[4])

#Accuracy for logistic regression:
overall_accuracy


--------
#logistic for testing data
cvfit = cv.glmnet(as.matrix(scale_testing[, c(-9)]), scale_testing$Outcome, 
                  family = "binomial", type.measure = "class")

# Show the cefficients of the best model
coef(cvfit, s = "lambda.min")

lg_p = predict(cvfit, newx = as.matrix(scale_testing[, c(-9)]), 
               s = "lambda.min", type = "class")

# Show confusion matrix
(lg_result <- table(lg_p, scale_testing$Outcome))

# Overall error rate and accuracy
overall_accuracy <- (lg_result[1] +  lg_result[4]) / sum(lg_result) 
overall_error <- 1 - overall_accuracy

# Error rate in class 0
error_c0 <- lg_result[2] / (lg_result[1] +  lg_result[2])

# Error rate in class 1
error_c1 <- lg_result[3] / (lg_result[3] +  lg_result[4])

#Accuracy for logistic regression:
overall_accuracy




