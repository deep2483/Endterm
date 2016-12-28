
############ DM2 ENDTERM ##############

####Q1

##A)## Correct
setwd("C:/Users/Avinashh/Desktop/") 
lens<-read.csv("lens.csv",header = F,stringsAsFactors = T)
str(lens) 

##B) ## Correct
colnames(lens)=c('Index','Age','Spec_pres','Astigamatic','TPR','Class')
str(lens)

##C)## Correct
lens$Age[lens$Age=="1"]="Young"
lens$Age[lens$Age=="2"]="pre_pres"
lens$Age[lens$Age=="3"]="pres"

lens$Spec_pres[lens$Spec_pres=="1"]="myope"
lens$Spec_pres[lens$Spec_pres=="2"]="normal"

##D) ## Correct
str(lens)
lens$Astigamatic=as.character(lens$Astigamatic)

##E) ## Correct
lens$Astigamatic[lens$Astigamatic=="1"]="no"
lens$Astigamatic[lens$Astigamatic=="2"]="yes"

##F and G) ## Correct
lens$TPR[lens$TPR==1]="reduced"
lens$TPR[lens$TPR==2]="normal"
str(lens)

##H) ## Correct
table(lens$Index)
table(lens$Age)
table(lens$Spec_pres)
table(lens$Astigamatic)
table(lens$TPR)
table(lens$Class)

##I) ## Correct
lens<-lens[c(-11),]
View(lens)

##J) ## Correct
lens<-lens[-1]

#################################################################

###Q2 ## Correct
library(tree)
lens$Class<-as.factor(lens$Class)
lens$Age<-as.factor(lens$Age)
lens$Spec_pres<-as.factor(lens$Spec_pres)
lens$Astigamatic<-as.factor(lens$Astigamatic)
lens$TPR<-as.factor(lens$TPR)
str(lens)

##A) ## Correct
library(caret)
set.seed(7)
intrain <- createDataPartition(y = lens$Class,p=0.70,list=F)
training<-lens[intrain,]
testing<-lens[-intrain,]

##B) ## Correct
model1<-tree(Class~.,data = training)

##C) ## Correct
plot(model1)
text(model1,pretty=0)

##D and E) ## Correct
pred_class<-predict(model1 ,testing,type ="class")
table(pred_class ,testing$Class)

##pred_classes 
#  1 2 3
#1 1 0 0
#2 0 1 0
#3 0 0 4

##F) ## Correct
summary(model1)
## Misclassification error rate: 0.1765 = 3 / 17

##G) ## Correct
mean(testing$Class != pred_class)
## Misclassification error rate: 0

##H) ## Techniques such as random forest can better be used as the test sample here is quite small
#Since all actual are predicting well in test data we have error rate as 0
#But in train data it seemed to be not that worse

################################################################################

## Ques1 - 10/10 ## Ques2- 9/10
##Q3 ## Correct 10/10
food<-lens<-read.csv("food1.csv",header = T, stringsAsFactors =  F)
str(food)
dim(food)

##1)
##Missing values treatment. Replacing by mean of the variable

for(i in 1:ncol(food)){
  if (is.numeric(food[,i])){
      food[is.na(food[,i]), i] <- mean(food[,i], na.rm = TRUE)
    }
  }
View(food)

sum(is.na(food))
##No missimg values now

pri_food =prcomp (food[-1] , scale =TRUE)
summary(pri_food)

##2)
biplot (pri_food, scale =0)

##3)
summary(pri_food)
##                       PC1    PC2    PC3     PC4     PC5     PC6     PC7
#Standard deviation     2.5026 1.9590 1.6567 1.24971 1.12912 1.08016 0.97012
#Proportion of Variance 0.3131 0.1919 0.1372 0.07809 0.06375 0.05834 0.04706
#Cumulative Proportion  0.3131 0.5050 0.6423 0.72035 0.78409 0.84243 0.88949
#PC8     PC9    PC10    PC11    PC12    PC13    PC14
#Standard deviation     0.82047 0.73384 0.61847 0.48927 0.45430 0.32945 0.22769
#Proportion of Variance 0.03366 0.02693 0.01912 0.01197 0.01032 0.00543 0.00259
#Cumulative Proportion  0.92314 0.95007 0.96920 0.98116 0.99148 0.99691 0.99950
#PC15      PC16
#Standard deviation     0.09972 1.803e-16
#Proportion of Variance 0.00050 0.000e+00
#Cumulative Proportion  1.00000 1.000e+00

##4)
## PC1 and PC2 explaines 50.50% of variations.

pri_food$rotation

## PC1 mostly talks about fruits, fruits jam and tin consumption 
## so we can name it as "Fruits consumption"

## PC2 mostly talks about coffee, bread , youghart
## hence we can name it as "confectionary Consumption"

#################################################################################
##Q4)

View(iris)
iris_new<-iris

#Peeking at the data
head(iris_new)
dim(iris_new)
str(iris_new)

##Checking the missing values
sum(is.na(iris_new))

attach(iris_new)

#LDA
library(MASS)
##Checking levels in Dependent variable
levels(iris_new$Species)

##Computing LDA in train dataset
lda.fit=lda(Species~.,data=iris_new,subset=iris_train)

##Q2
lda.fit

# Prior probabilities of groups:
#  setosa versicolor  virginica 
# 0.32       0.36       0.32 

##Q3

#Group means:
#  Sepal.Length Sepal.Width Petal.Length Petal.Width
# setosa         4.943750    3.340625     1.450000    0.240625
# versicolor     5.936111    2.780556     4.277778    1.330556
# virginica      6.600000    3.000000     5.556250    2.081250

##Q4

# Coefficients of linear discriminants:
# LD1        LD2
# Sepal.Length  0.6258009  0.4071223
# Sepal.Width   1.4366869  1.5178632
# Petal.Length -1.8109354 -1.6912109
# Petal.Width  -3.1163349  3.8620989

##Q5

##Plotting LDA1 and LDA2
#create data.frame
data_Pred<-data.frame(Species=predict(lda.fit)$class,predict(lda.fit)$x)
##Now Plotting LDA1 and LDA2
#Eqivalent plot with ggplot2 but without decision boundaries
library(ggplot2)
ggplot(data_Pred, aes(x=LD1, y=LD2, col=Species) ) + 
  geom_point(size = 3, aes(pch = Species))

##Q6)

##predicitng for iris_new dataset
lda.pred=predict(lda.fit,iris_new)

##Confusion matrix
lda.class=lda.pred$class
table(lda.class,iris_new$Species)

##Misclassification error rate
mean(iris_new$Species!=lda.class)
## Misclassification error rate=0.033333

##Posterior probabilities for levels of spicies
lda.pred$posterior 
summary(lda.pred$posterior)

## Correct Great Work Total Score 39/40
## The plots were good
