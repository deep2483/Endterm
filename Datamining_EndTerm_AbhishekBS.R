###########################################
#                                         #
# Q1  Loading the Data of Lenses into R   #
#                                         #
###########################################

## TOTAL SCORE 29/40
#A.Loading lense table into R

lens<-read.csv('E:/Datamining and R/DM2/Lenses.csv', stringsAsFactors = T, header = F)
str(lens)

#B.Adding Column names

colnames(lens) <-c("index", "age", "spec_pres", "astigmatic", "tpr", "class")
View(lens)

#C.Giveing the met data names

lens$age[lens$age == "1"]="young"
lens$age[lens$age == "2"]="pre-presbyopic"
lens$age[lens$age == "3"]="presbyopic"
lens$spec_pres[lens$spec_pres=="1"]="myope"
lens$spec_pres[lens$spec_pres=="2"]="hypermetrope"

#D. Convert the coulmn to character

lens$astigmatic=as.character(lens$astigmatic)


#E. change the astigmatic column data

lens$astigmatic[lens$astigmatic=="1"]="no"
lens$astigmatic[lens$astigmatic=="2"]="yes"


#F. TPR column data type convert to character

lens$tpr[lens$tpr==1]="reduced"


#G.replace 2 TPR coulmn with normal

lens$tpr[lens$tpr=="2"]="normal"

#H. Counts of each data type.

table(lens$index)
table(lens$age)
table(lens$spec_pres)
table(lens$astigamatic)
table(lens$trp)
table(lens$class)

#I. Removing G from the column

lens=lens[lens$astigmatic!="g",]

#J. Deleting Index Column

lens$index <- NULL

View(lens)

## Correct 10/10
############################################################################################


###############################
#                             #
# Q2    Decision Tree         #
#                             #
###############################

#Converting all the features into factors

library(tree)
library(caret)

lens$class<-as.factor(lens$class)
lens$age<-as.factor(lens$age)
lens$spec_pres<-as.factor(lens$spec_pres)
lens$astigmatic<-as.factor(lens$astigmatic)
lens$tpr<-as.factor(lens$tpr)

#A. Spliting the data 0.70 Using Caret package

set.seed(2)
lens_data<-createDataPartition(y= lens$class, p=0.70,list = F)
lens_train<-lens[lens_data]
lens_test<-lens[-lens_data]

#B. creating the model and storing it in a variable model1

model1<-tree(Class ~ .,x,tree = train)

#C. plot the model 

plot(model1)
text(model1,pretty=0)

#D. predict on the test data set

pred_class<-predict(model1 ,lens_test,type ="class")

#E. print the confuison matrix

table(pred_class ,lens_test$class)

#F. Misclassification erro rate.

summary(model1)

# Misclassification error rate: 0.1765 = 3 / 17

#G. Using formula mean  

mean(testing$Class != pred_class)

# Misclassification error rate: 0

#H. Train Data set has an error rate of 0.176 which is bad compared to testing data as it got 0

## Since the sample we need to bootstrap rather than relying on test sample 9/10
############################################################################################

###############################
#                             #
# Q3          PCA             #
#                             #
###############################

#Lodaing the data into R

food<-read.csv('E:/Datamining and R/DM2/Foodconsumption.csv', stringsAsFactors = F, header = T)
str(food)
View(food)
dim(food)
summary(food)

names(food)

#Replacing missing values with mean

for(i in 1:ncol(food))
  {
   if (is.numeric(food[,i]))
     {
      data[is.na(food[,i]), i] <- mean(food[,i], na.rm = TRUE)
     }
  }

View(food)

## PCA questions not answered correctly 0/10
############################################################################################

###############################
#                             #
# Q4          LDA             #
#                             #
###############################

#Loading IRIS Data

data(iris)
head(iris)
str(iris)

iris1<-iris
attach(iris1)
#splitting the data 

set.seed(1) 
train=sample(1:nrow(iris1),110)  

#LDA
library(MASS)
library(ggplot2)

# Group Mean, Coefficent Model
iri_train<-lda(Species~.,data=iris1,subset=train) 
iri_train 

#test set

iri_test<-iris1[-train,]

#plotting LD1 and LD2 
data_Pred<-data.frame(Species=predict(iri_train)$class,predict(iri_train)$x)
ggplot(data_Pred, aes(x=LD1, y=LD2, col=Species) ) + 
  geom_point(size = 3, aes(pch = Species))

#prediction for
iri_pred=predict(iri_train,iris$Species) 
iri_class=iri_pred$class 

#confusion matrix
table(iri_class,iris$Species) 
mean(iri_class==iris$Species)

summary(tree.iris1)
## Correct 10/10


############################################################################################
