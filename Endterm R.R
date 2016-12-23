
#Question1
#Importing lens file
setwd("E:\PRAXISKT\Deepanker 'R'\DM2")

#Reading the csv file and storing in lens
lens <- read.csv("lenses.csv", header=T, sep=",")

#to see the lens
str(lens)

#column names
colnames(lens)<- c("index","spec_pres","astigmatic","tpr")

#c).
lens$age[lens$age == "1"]="young"
lens$age[lens$age == "2"]="pre-presbyopic"
lens$age[lens$age == "3"]="presbyopic"
lens$spec_pres[lens$spec_pres=="1"]="myope"
lens$spec_pres[lens$spec_pres=="2"]="hypermetrope"

#D. 
str(lens$astigmatic)
#Using the code as.factor
lens$astigmatic=as.character(lens$astigmatic)

# changing the data  1 to no and 2 to yes
lens$astigmatic[lens$astigmatic=="1"]="no"
lens$astigmatic[lens$astigmatic=="2"]="yes"
 
#f.replacing with reduced
lens$tpr[lens$tpr==1]="reduced"

#g.Replacing 2 with tpr column with normal
lens$tpr[lens$tpr=="2"]="normal"

#h).using table columns
table(lens$astigmatic)

#I) 
lens=lens[lens$astigmatic!="g",]

#j).index column is the 1st column so removing index column
lens=lens[-1]


#Question:2
library(ISLR)
library(rattle)
library(rpart)
library(caret)
library(tree)
lens$Class<-as.factor(lens$Class)
lens$Age<-as.factor(lens$Age)
lens$Spec_pres<-as.factor(lens$Spec_pres)
lens$Astigamatic<-as.factor(lens$Astigamatic)
lens$TPR<-as.factor(lens$TPR)
str(lens)


#A).split ratio 0.7
smp_size <- floor(0.70 * nrow(lens))

set.seed(3)
train_ind <- sample(seq_len(nrow(lens)), size =smp_size)
train <- lens[train_ind, ]
test <- lens[-train_ind, ]
View(lens)


#B)using tree command
model1<-tree(Class~.,data = train)

#C)plotting the model1
plot(model1)
text(model1,pretty=0)

#D)predicting the classes and storing in pred_class
pred_class<-predict(model1 ,test,type ="class")

#).Using table command
table(pred_class ,test$Class)


#F)using summary command to print the summry
summary(model1)

#G)missclassification error rate is 0
mean(test$Class != pred_class)

#H)Test data set has zero misclassification error rate
#Train data is having more error rate which is not worth hence we can improve the model by other techniques like bagg


#Q3PCA
foods<-read.csv("foods.csv",header = F,stringsAsFactors = F)
str(foods)
dim(foods)

##1)
pr.foods =prcomp (foods , scale =TRUE)

summary=pr.food
names(pr.food)
pr.food$scale
pr.food$rotation


#Question:4
library(MASS)
library(ROCR)read.csv()
library(pROC)
library(ISLR)
library(ggplot2)
names(iris)
View(iris)

dim(iris)
summary (iris)

attach(iris)
data<-iris

#Splitting the data into training and test

smp_size <- floor(0.80 * nrow(iris))

set.seed(1)
train_ind <- sample(seq_len(nrow(iris)), size =smp_size)

train <- iris[train_ind, ]
test <- iris[-train_ind, ]

#Group Means and coefficients of the discriminant model-Sepal.Length  0.8293776  0.02410215
#Sepal.Width   1.5344731  2.16452123
#Petal.Length -2.2012117 -0.93192121
#Petal.Width  -2.8104603  2.83918785
#prior class probabilities setosa-0.3333 versicolor-0.3333 virginica-0.3333
lda.fit=lda(data$Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=data)
lda.fit

#plotting the data
plot(lda.fit)

#predicing using predict function
lda.pred=predict(lda.fit ,train)
names(lda.pred)

#Posterior probabilities
lda.class =lda.pred$class

#Accuracy of classification 0.98
table(lda.class)
mean(lda.class == train$Species)

#Misclassification error
mean(lda.class!=train$Species)

#checking the accuracy on test data
lda.pred=predict(lda.fit ,test)
names(lda.pred)


lda.class =lda.pred$class

#test data accuarcy=0.96
table(lda.class)
mean(lda.class == test$Species)

#Posterior probabilities
View(lda.pred)

#plotting the LD1x LD2X
plot(lda.pred$x)



