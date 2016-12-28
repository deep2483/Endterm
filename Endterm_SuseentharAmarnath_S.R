############################################################
#                   Question 1                             #
############################################################

## Score 36/40
# A. importing csv file

lens <- read.csv("lense1.csv", header = F)
str(lens)


# B. Adding coloumn names

colnames(lens) <- c("index","age","spec_pres","astigmatic","tpr","class")

#C. changing data

lens$age[lens$age == "1"]= "young"
lens$age[lens$age == "2"]= "pre-presbyopic"
lens$age[lens$age == "3"]= "presbyopic"

lens$spec_pres[lens$spec_pres == 1] = "myope"
lens$spec_pres[lens$spec_pres == 2] = "hypermetrope"


#D. Converting factor to char

str(lens)
lens$astigmatic=as.character(lens$astigmatic)

#E. replacing with names

lens$astigmatic[lens$astigmatic=="1"]="no"
lens$astigmatic[lens$astigmatic=="2"]="yes"

#F. 

lens$tpr[lens$tpr==1]="reduced"
str(lens)

#G. replacing 2 in tpr column with "normal"

lens$tpr[lens$tpr=="2"]="normal"
str(lens)

#H. Using table comd to see the variable counts

table(lens$age)
table(lens$spec_pres)
table(lens$astigmatic)
table(lens$tpr)
table(lens$class)

#I. removing row which has g in column in astigmatic

lens=lens[lens$astigmatic!="g",]
str(lens)

#J. Removing index column

lens=lens[-1]

 #or#

lens$index <- NULL

## Correct 10/10
############################################################
#                   Question 2                             #
############################################################

require(caret)
require(rpart)
library(ISLR)
library(tree)
library(rattle)
library(rpart)
library(rpart.plot)
library(tree)

# Converting all columns into factors

lens$age        <- as.factor(lens$age)
lens$spec_pres  <- as.factor(lens$spec_pres)
lens$astigmatic <- as.factor(lens$astigmatic)
lens$tpr        <- as.factor(lens$tpr)
lens$class      <- as.factor(lens$class)

# A. Using the createDataPartition in caret package to split data into train and test

set.seed(4)
intrain  <- createDataPartition(y = lens$class,p=0.7,list=F)
train <-lens[intrain,]
test  <-lens[-intrain,]

#B building model

tree.basic <- tree(class~ .,data = train)

summary(tree.basic)

#C. Plotting the tree

plot(tree.basic)

text(tree.basic ,pretty =0)

#D. predicting class using test dataset

pred_class <- predict(tree.basic,test,type="class")

#E.confusion matrix

table(test$class,pred_class)

#F. Summary of model

summary(tree.basic)

#G.Misclassification error rate

mean(test$class!=pred_class)

## No Explanation given for misclassification rate no values printed and how the solution can be improved 6/10
############################################################
#                   Question 3                             #
############################################################


# importing csv file
food <-read.csv("food.csv",header = T, stringsAsFactors =  F)
str(food)

#checking for Missing values in food data

sapply(food,function(x) sum(is.na(x)))

#Replacing Missing values with Mean

food$Sweetener[is.na(food$Sweetener)] <- mean(food$Sweetener,na.rm=TRUE)
food$Biscuits[is.na(food$Biscuits)] <- mean(food$Biscuits,na.rm=TRUE)
food$Yoghurt[is.na(food$Yoghurt)] <- mean(food$Yoghurt,na.rm=TRUE)

#1. PCSmodel - Leaving the first column(char) and scaling the dataframe

food.out =prcomp (food[-1] , scale =TRUE)
summary(food.out)
names(food.out)
food.out$center

#2.Plotting PC1 against PC2

biplot (food.out , scale =0)

#3.Variance

food.out$sdev
pr.var =food.out$sdev ^2
pr.var

pve=pr.var/sum(pr.var)
pve
cumsum(pve)


plot(pve , xlab=" Principal Component ", ylab= "Proportion of
     Variance Explained ", ylim=c(0,1) ,type='b')

#4. Naming the fist two groups
food.out$rotation

# PC1 talks more about Tin.soup,Tinned.fruit,Frozen.veggies,Sweetener
# so they can be tagged as "preserved food items"
# PC2 talks more about Instant.coffee,Powder.soup,Crisp.bread
# so they can be tagged "Coffe soup"

## 10/10
############################################################
#                   Question 4                             #
############################################################

require(MASS)
require(ggplot2)
require(scales)
require(gridExtra)

# Loading data
data(iris)
str(iris)

# Spliting data

train <- sample(1:150, 75)
prior_prob <- lda(formula = Species ~ .,data = iris,prior = c(1,1,1)/3,subset = train)

#1. Prior class probability

prior_prob$prior

# setosa versicolor  virginica
# 0.3333333  0.3333333  0.3333333

#2. Group Means

prior_prob$means

# Sepal.Length Sepal.Width Petal.Length Petal.Width
# setosa         5.008000    3.488000     1.436000    0.240000
# versicolor     5.979310    2.810345     4.262069    1.320690
# virginica      6.361905    2.885714     5.442857    1.947619


#3.Coefficient of discriminant model generated

prior_prob$scaling

# LD1        LD2
# Sepal.Length  0.7549198 -0.9854035
# Sepal.Width   1.4900793  2.4056869
# Petal.Length -2.1701328 -0.1995235
# Petal.Width  -2.9087113  2.2349124


#4. Posterior probability

# CV = TRUE it uses a leave-one-out cross-validation
# and returns a named list with components

post_prob <- lda(formula = Species ~ .,data = iris, prior = c(1,1,1)/3,CV = TRUE)
head(post_prob$posterior, 3)

# setosa   versicolor    virginica
# 1      1 5.087494e-22 4.385241e-42
# 2      1 9.588256e-18 8.888069e-37
# 3      1 1.983745e-19 8.606982e-39


#5.Plotting LD1 against LD2


plot_data  <-data.frame(Species=predict(prior_prob)$class,predict(prior_prob)$x)

final_plot <- ggplot(data_Pred, aes(x=LD1, y=LD2, col=Species) ) + 
              geom_point(size = 3, aes(pch = Species))

#6. Calculating Missclassification error

lda.pred=predict(prior_prob,iris)

# Confusion matrix
lda.class=lda.pred$class
table(lda.class,iris$Species)

# Misclassification error rate
mean(iris$Species!=lda.class)

 ## Correct 10/10
