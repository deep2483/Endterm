
## Score 40/40
setwd("C:/Users/User/Desktop/DM2-EndTerm/Question")

##### Q.1 #########
# reading file
lens<-read.csv("lenses.csv",header=F)
str(lens)

#coloumnnames
colnames(lens)<-c('index','age','spec_pres','astigmatic','tpr','class')
head(lens)

# format change in age and spec_pres variable
lens$age[lens$age==1]<-"young"
lens$age[lens$age==2]<-"pre-presbyopic"
lens$age[lens$age==3]<-"presbyopic"
lens$spec_pres[lens$spec_pres==1]<-"myope"
lens$spec_pres[lens$spec_pres==2]<-"hypermetrope"

# changing factor to character
str(lens)
lens$astigmatic<-as.character(lens$astigmatic)

# changing names in astigmatic cloumn
lens$astigmatic[lens$astigmatic==1]="no"
lens$astigmatic[lens$astigmatic==2]="yes"

# replacing reduced in the tpr coloumn
lens$tpr[lens$tpr==1]<-"reduced"
str(lens)

#replacing '2'in tpr
lens$tpr[lens$tpr==2]<-"normal"

#using table comment to see the count in each data type
table(lens$age)
table(lens$spec_pres)
table(lens$astigmatic)
table(lens$tpr)
table(lens$class)

# deleting the row with typo
lens<-lens[-(lens$astigmati=='g'),]

#removing index coloumn
lens<-lens[,-1]
str(lens)

## Correct 10/10

######## Q.2 ########

#creating dependent var class as catoegrical for 1=hardlens,2=softlens and 3=nolens
lens$class[which(lens$class==1)]<-"hardlens"
lens$class[which(lens$class==2)]<-"softlens"
lens$class[which(lens$class==3)]<-"nolens"
str(lens)

#conerting all type to factor to avoid coercion error
lens$class<-as.factor(lens$class)
lens$age<-as.factor(lens$age)
lens$spec_pres<-as.factor(lens$spec_pres)
lens$astigmatic<-as.factor(lens$astigmatic)
lens$tpr<-as.factor(lens$tpr)


# splitting datasetset
set.seed(4)
library(caret)
sample <- createDataPartition(y = lens$class,p=0.7,list=F)
train<-lens[sample,]
test<-lens[-sample,]
str(train)
str(test)

#BUILDING TREE
model1<-tree(class~.,train)
summary(model1)

#plotting tree and adding text
plot(model1)
text(model1,pretty=0)
model1

#predicting in test data
pred_class<-predict(model1,test,type="class")
pred_class

#printing confusion matrix
table(pred_class,test$class)

#printing summary fo r model
summary(model1) #### Misclassification error rate: 0.1765 = 3 / 17####

#findnig Misclassification error rate in test data
MER<-mean(test$class!=pred_class)
MER 


# ###############comparion of Misclassification error rate ##############################
# MER is zero in test data and 0.17 in train data. model is very good in test data      #
# but small error  in train data. Possibilty of classifying a not required lens category#
# as a hard/softlens category.But model looks very good in test data. By checking ROC   # 
# and by using Bagging or boosting models will give more accuracy                       #
######################################################################################## 



### Correct 10/10

######## Q.3 ########

# importing and analyisng data
food<-read.csv("food.csv",header=T, stringsAsFactors = F)
str(food)
head(food)
names(food)

#finding missing value
which(is.na(food))
sapply(food,function(x) sum(is.na(x)))

#3 missing values found.replacing with col means

food$Sweetener[is.na(food$Sweetener)]<-mean(food$Sweetener,na.rm=TRUE)
food$Biscuits[is.na(food$Biscuits)]<-mean(food$Biscuits,na.rm=TRUE)
food$Yoghurt[is.na(food$Yoghurt)]<-mean(food$Yoghurt,na.rm=TRUE)

sapply(food,function(x) sum(is.na(x))) ## no missig values now ## 

#fitting a PCA model
pr.food =prcomp (food[-1] , scale =TRUE)
summary(pr.food)

#plotting loading of P1 and P2
biplot (pr.food , scale =0)

# finding the variance explained -- availablle in summary
summary(pr.food)
##############################################################################################################
#                          PC1    PC2    PC3     PC4     PC5     PC6     PC7     PC8     PC9    PC10    PC11 #
#Standard deviation     2.5026 1.9590 1.6567 1.24971 1.12912 1.08016 0.97012 0.82047 0.73384 0.61847 0.48927 #
#Proportion of Variance 0.3131 0.1919 0.1372 0.07809 0.06375 0.05834 0.04706 0.03366 0.02693 0.01912 0.01197 #
#Cumulative Proportion  0.3131 0.5050 0.6423 0.72035 0.78409 0.84243 0.88949 0.92314 0.95007 0.96920 0.98116 #
#PC12    PC13    PC14    PC15      PC16                                                                      #
#Standard deviation     0.45430 0.32945 0.22769 0.09972 1.803e-16                                            #
#Proportion of Variance 0.01032 0.00543 0.00259 0.00050 0.000e+00                                            #
#Cumulative Proportion  0.99148 0.99691 0.99950 1.00000 1.000e+00                                            #
##############################################################################################################


# interpreting and naming PCAs
pr.food$scale
pr.food$x
pr.food$rotation

# Most of the variation is explained by PC1 PC2 and PC 3 - 64 % variance.
# PC1 talks about ygurt,tin soup,coffee - liquid food lovers 
# PC2 talks more abt butter,jam - bread lovers
# PC3 talks abtmargarine butter, oranges- food lovers

## Correct 10/10


###### Q.4 #######
# loading and splitting data #

library(MASS)

data(iris)
iris_ds<-iris
head(iris_ds)

set.seed(2) 
train<-sample(1:nrow(iris_ds),105)  
iris.test<-iris_ds[-train,]

## Running LDA
lda.model<-lda(Species~.,data=iris_ds,subset=train)
lda.model

# 1.,2.& 3. prior cals probailities and group means
plot(lda.model)

###################################
#  Prior probabilities of groups: #
#  setosa versicolor  virginica   #
# 0.3523810  0.3238095  0.3238095 #
###################################

##################################################################
# Group means:                                                   #
#               Sepal.Length Sepal.Width Petal.Length Petal.Width#
# setosa         4.975676    3.410811     1.475676   0.2432432   #
# versicolor     5.970588    2.764706     4.323529   1.3294118   #
# virginica      6.602941    3.038235     5.567647   2.0529412   #
##################################################################

########################################
# Coefficients of linear discriminants:#
#               LD1        LD2         #
# Sepal.Length  0.7984948 -0.1548242   #
# Sepal.Width   1.8453627  2.3168483   #
# Petal.Length -2.3166005 -0.9851038   #
# Petal.Width  -2.9726728  3.0692946   #
########################################

# 5. Plottting LD1 and LD2
ld.plot<-data.frame(Species=train.pred$class,train.pred$x)
ld.plot

library(ggplot2)
ggplot(ld.plot, aes(x=LD1, y=LD2, col=Species) ) +  geom_point(size = 3, aes(pch = Species))

## 6. prediction function & model fit in test datset & confusion matrix
test.pred<-predict(lda.model,iris.test)
irislda.class<-iris.pred$class

table(irislda.class,iris.test$Species)

#############Confusion Matrix ###############
# irislda.class setosa versicolor virginica #
# setosa         14          3         0    #
# versicolor      0         10         4    #
# virginica       0          0        14    #
#############################################

#  Misclassification Error
mean(irislda.class==iris.test$Species)
# Misclassification Error  =  0.8444444

## Correct 10/10  
                  ###########################################################
