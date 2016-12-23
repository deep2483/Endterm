#End Term
# Author: Nishanth Rao

#Q.1 
library(dplyr)
# A. Reading the csv file into R
getwd()
setwd("C:/Users/Nishanth/Documents/BA/DataMining2/EndTerm")
lens <- read.csv("lenses.csv",stringsAsFactors = FALSE, header=FALSE)
lens
str(lens)

# B. getting the column nanmes
colnames(lens) <- c('index','age','spec_pres','astigmatic', 'tpr', 'class')

# C. Changing the values of the columns age and spec_pres as per the metadata
# age: 1= young 2=pre-presbyopic 3= presbyopic
# spec_pres: 1= myope 2= hypermetrope

lens$age[lens$age==1] = "young"
lens$age[lens$age==2] = "pre-presbyopic"
lens$age[lens$age==3] = "presbyopic"

# Similarly changing the column spec_pres with their metadata values
lens$spec_pres[lens$spec_pres==1] = "myope"
lens$spec_pres[lens$spec_pres==2] = "hypermetrope"

str(lens)

#D. Converting tpr and class to character
lens$tpr = as.character(lens$tpr)
lens$class = as.character(lens$class)

str(lens)

#E. Changing the astigmatic column values to its corresponding metadata values: 1= no 2 = yes
lens$astigmatic[lens$astigmatic=="1"] = "no"
lens$astigmatic[lens$astigmatic=="2"] = "yes"

str(lens)

#F and G Changing the tpr column values: 1= reduced 2=normal
lens$tpr[lens$tpr=="1"] = "reduced"
lens$tpr[lens$tpr=="2"] = "normal"

#H. 
table(lens)
lens

#I. Removing the record with the value g and storing it in another data frame lens1
lens <- filter(lens, astigmatic != "g")
lens1

#J. Removing the index column from lens1
lens1 <- lens1[, c("age", "spec_pres", "astigmatic", "tpr", "class")]
lens1


#Q2
#A. 
library(tree)
library(rattle)

# Using the final modified dataset, lens1 as lens going forward 
lens=lens1

# Changing the variables to factor
lens$age = as.factor(lens$age)
lens$spec_pres = as.factor(lens$spec_pres)
lens$astigmatic = as.factor(lens$astigmatic)
lens$tpr = as.factor(lens$tpr)
lens$class = as.factor(lens$class)

lens

lens = lens[,c("age", "spec_pres", "astigmatic", "tpr", "class")]
#Creating a train and test sample with a split of 0.7 and 0.3 respectively
Training = sample (1: nrow(lens ), nrow(lens )*0.7)
Train
Train = lens[Training,]
Test = lens[-Training,]

#B. Running the tree command

tree.lens =tree(class~.,data=Train)
model1 = tree.lens


#C. plotting
plot(model1)
text(model1 ,pretty =0)

#D. predicting
pred_class <- predict(model1, Test, type='class')
pred_class

summary(pred_class)

#E. Creating the confusion matric
table(Test$class, pred_class)

#F Sumamry of the model

summary(model1)

#########################Output###############################
# Classification tree:
# tree(formula = class ~ ., data = Train)
# Variables actually used in tree construction:
#  [1] "tpr"
# Number of terminal nodes:  2 
# Residual mean deviance:  1.029 = 14.4 / 14 
#Misclassification error rate: 0.1875 = 3 / 16 
##################################################################

#G. Misclassification error rate for Test

test_Error = mean(Test$class != pred_class)
test_Error

##########Output #######
# test_Error
# [1] 0.5714286
#######################
#H.
# Observation 
# The error rate for Test dataset is higher as the model is being run on unknown data set. 
# This means that the model is overfit. 
# We can use bagging , boostng and randomforest to increase the accuracy.

#Question 3

# Loading the data from csv

foods <- read.csv("food.csv",stringsAsFactors = FALSE, header=TRUE)

foods
# Running PCA
#Checking for missing values 
is.na(foods)
foods[is.na(foods)]

# There are 3 misisng values For column Sweetner , country Spain, Biscuits for Sweden and Yogurt for Finland.
# Updating the average values 

avg_sweet = mean(foods[,"Sweetener"], na.rm=T)
avg_bisc = mean(foods[,"Biscuits"], na.rm=T)
avg_yog = mean(foods[,"Yoghurt"], na.rm=T)


str(foods)

foods1 = foods[,-1]
country =row.names(foods)

pr.foods =prcomp(foods , scale =TRUE)

#4

library(MASS)
library(ROCR)
library(pROC)
library(ISLR)

#Running LDA
iris
train = sample (1: nrow(iris ), nrow(iris )*0.7)
lda.fit=lda(Species~. ,data=iris ,subset =train)

lda.fit
############################################
#Call:
#  lda(Species ~ ., data = iris, subset = train)

#Prior probabilities of groups:
#  setosa versicolor  virginica 
#0.3333333  0.3333333  0.3333333 

#Group means:
#  Sepal.Length Sepal.Width Petal.Length Petal.Width
#setosa            5.006       3.428        1.462       0.246
#versicolor        5.936       2.770        4.260       1.326
#virginica         6.588       2.974        5.552       2.026
#
#Coefficients of linear discriminants:
#  LD1         LD2
#Sepal.Length  0.8293776  0.02410215
#Sepal.Width   1.5344731  2.16452123
#Petal.Length -2.2012117 -0.93192121
#Petal.Width  -2.8104603  2.83918785
#
#Proportion of trace:
#  LD1    LD2 
#0.9912 0.0088
##############################################

plot(lda.fit)

# Predicting using test

lda.pred=predict(lda.fit , iris[-train,])
names(lda.pred)
test = iris[-train,]
lda.class =lda.pred$class

table(lda.class ,test$Species)
mean(lda.class == test$Species)



