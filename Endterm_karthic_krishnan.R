#running required packages
## Score 36/10-deepankar
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

#q1.a
#reading data using read.csv
lens <- read.csv("C:/Users/Dell/Documents/lenses10.csv")

#viewing structure of data
str(lens)
View(lens)
describe(lens)

#q1.b
colnames (lens) <- c("index","age","spec_pres","astigmatic","tpr","class")

#viewing updated column names
str(lens)

#q1.c
#assigning categorical names for respective subclasses of variables
lens$age[lens$age == "1"]="young"
lens$age[lens$age == "2"]="pre-presbyopic"
lens$age[lens$age == "3"]="presbyopic"
lens$spec_pres[lens$spec_pres=="1"]="myope"
lens$spec_pres[lens$spec_pres=="2"]="hypermetrope"
lens$astigmatic[lens$astigmatic=="1"]="no"
lens$astigmatic[lens$astigmatic=="2"]="yes"
lens$tpr[lens$tpr=="1"]="reduced"
lens$tpr[lens$tpr=="2"]="normal"
lens$class[lens$class=="1"]="patient needs hard contact lens"
lens$class[lens$class=="2"]="patient needs soft contact lens"
lens$class[lens$class=="3"]="patient does not need contact lens"

#q1.d
#now observing the changed data names 

str(lens)

##due to one observation have value as "g". astigmatic has to be converted to character
lens$astigmatic = as.character(lens$astigmatic)
table(lens$astigmatic)
View(lens)

#q.1.e
#now re assigning the names to astigmatic data
lens$astigmatic[lens$astigmatic=="1"]="no"
lens$astigmatic[lens$astigmatic=="2"]="yes"


#q.1.f
#its observed that tpr class now changed to character
lens$tpr[lens$tpr==1]="reduced"
str(lens)

#q.1.g
#its observed that tpr class now changed to character
lens$tpr[lens$tpr==2]="normal"
str(lens)
class(lens$tpr)

#q.1.h
#using table command to see frequency of each data type

table(lens$index)
table(lens$age)
table(lens$spec_pres)
table(lens$astigmatic)
table(lens$tpr)
table(lens$class)

#q.1.i
#its observed that one typo error presence of value "g" in astigmatic variable 
#so removing that observation(row) having value "g"
lens=lens[lens$astigmatic!="g",]
str(lens)
class(lens$astigmatic)
table(lens$astigmatic)

#q.1.j
# removing index variable as model doesnt require it
lens=lens[-1]
str(lens)
View(lens)

## Question1 Correct 10/10


#q.2.a
#converting to factors
lens$age <- as.factor(lens$age)
lens$spec_pres <- as.factor(lens$spec_pres)
lens$astigmatic <- as.factor(lens$tpr)
lens$tpr <- as.factor(lens$tpr)
lens$class <- as.factor(lens$class)
str(lens)

# Using the create Data Partition in caretpackage to split data into training and testing
set.seed(1)
intrain <- createDataPartition(y = lens$class, p=0.70, list = F)
train <- lens[intrain,]
test <- lens[-intrain,]
dim(train)
dim(test)

#q.2.b


#decision using c.50
class.c50 = C5.0(train$class ~.,data=train)
class.c50

#q.2.c
#ploting decision tree
plot(class.c50,cex = .7)

#q.2.d
#predicting the class in test data
pred_class <-predict(class.c50,test)

#q.2.e
#printing confusion matrix
confusionMatrix(test$class,pred_class) 
print(confusionMatrix(test$class,pred_class))
#or
# cross tabulation of predicted versus actual classes
CrossTable(test$class, pred_class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual class', 'predicted class'))

#q.2.f
#using summary to summarize the model
summary(class.c50)

#q.2.g
#using summary to summarize the model
summary(pred_class)
mean(train$class != pred_class)
mean(test$class != pred_class)

#q.2.g
#error rates are more in train. recommendation is to collect bigger sample size to have proper distribution and derive better accuracy.
## Correct 10/10
#q.3
#load the data
foodcon <- read.csv("foodcon10.csv",stringsAsFactors = F )

#view data & structure
View(foodcon)
str(foodcon)

#view row names
countries =row.names(foodcon)
names(foodcon)

#finding missing values
is.na(foodcon)

# missing value imputation by median Median 
foodcon$Sweetener[is.na(foodcon$Sweetener)] <- median(foodcon$Sweetener,na.rm = T)
foodcon$Yoghurt[is.na(foodcon$Yoghurt)] <- median(foodcon$Yoghurt,na.rm = T)


#finding mean and variance  value of data
apply(foodcon , 2, mean)
apply(foodcon , 2, var)


#finding principal component
pr.out =prcomp (foodcon , scale =TRUE)

#summary of PC output
summary(pr.out)

#rotaion process
names(pr.out)

#viewing center,scale,rotaion,and x
pr.out$center
pr.out$scale
pr.out$rotation
pr.out$x

#ploting output of pc visualization
biplot (pr.out , scale =0)

#rotating the axis
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x

#ploting output of pc visualization after rotation
biplot (pr.out , scale =0)
pr.out$sdev
pr.var =pr.out$sdev ^2
pr.var

pve=pr.var/sum(pr.var)
pve

#ploting pc vs variance explained
plot(pve , xlab=" Principal Component ", ylab= "Proportion of
     Variance Explained ", ylim=c(0,1) ,type='b')


plot(cumsum (pve), xlab="Principal Component", ylab ="Cumulative Proportion of Variance Explained", ylim=c(0,1) ,
     type='b')

## PC1 and PC2 are not named 8/10
#q4
# define the filename
irisfile <- iris

names(iris)
View(iris)

head(iris)
dim(iris)
str(iris)

# set the column names in the dataset
colnames(irisfile) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

# create a list of 67% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(irisfile$Species, p=0.65, list=FALSE)

# select 20% of the data for validation
validation <- irisfile[-validation_index,]

# use the remaining 80% of data to training and testing the models
dataset <- irisfile[validation_index,]

# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)



# take a peek at the first 5 rows of the data
head(dataset)



# list the levels for the class
levels(dataset$Species)

# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

# summarize attribute distributions
summary(dataset)

#Univariate Plots
# split input and output
x <- dataset[,1:4]
y <- dataset[,5]

# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# barplot for class breakdown
plot(y)

#Multivariate Plots
# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


# LDA
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda,cart=fit.cart))
summary(results)

# compare accuracy of models
dotplot(results)


# summarize Best Model
print(fit.lda)

#Make Predictions
# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)



## plot of LD1 vs LD2 and Species not done
##8/10


