## CHECKING THE WORKING DIRECTORY TO STORE FILES IN DESIRED PATH##
getwd()



###############################################################################################
## QUESTION.1##                                                                               #
###############################################################################################
## READING FILE FROM SOURCE AND STORING IN A DATA FRAME LENS##
lens=read.csv(file = 'lenses.csv', header = FALSE, sep = ',', dec = '.')
str(lens)




## QUESTION 1.B##
## ASSINGNING THE COLUMN NAMES##
colnames(lens) <- c("index","age","spec_pres","astigmatic","tpr","class")

## CHECKING FOR THE CHANGES##
str(lens)




## QUESTION 1.C##
## CONVERTING INTEGER VARIABLES INTO CHARACTOR##
## ASSIGNING THE NAME TO LEVEL IN THE VARIABLE##
lens$age[lens$age == "1"]="young"
lens$age[lens$age == "2"]="pre-presbyopic"
lens$age[lens$age == "3"]="presbyopic"
lens$spec_pres[lens$spec_pres=="1"]="myope"
lens$spec_pres[lens$spec_pres=="2"]="hypermetrope"





## QUESTION 1.D##
## CHECKING FOR CHANGES IN THE TYPE OF VARIABLE AND VARIABLE NAMES##
str(lens)

## AS ASTIGMATIC COLUMN IS A FACTORE WHICH IS STORING NUMBERS AS CHARACTOR, NOW CONVERTING THIS COLUMN AS CHARACTOR##
lens$astigmatic=as.character(lens$astigmatic)

## CHECKING FOR THE CHANGES##
str(lens)




## QUESTION 1.E##
## NOW CHANGING THE LEVELS OF ASTIGMATIC INTO RIGHT NAMES AS IT IS IN 1 AND 0##
lens$astigmatic[lens$astigmatic=="1"]="no"
lens$astigmatic[lens$astigmatic=="2"]="yes"

## CHECKING FOR THE CHANGES##
str(lens)




## QUESTION 1.F##
## CONVERTING LEVEL 1 OF TPR VARIABLE TO "REDUCED"##
lens$tpr[lens$tpr==1]="reduced"

## CHECKING FOR THE CHANGES##
## DATA TYPE OF THE VARIABLE CHANGED TO CHARACTOR##
str(lens)




## QUESTION 1.G##
## CONVERTING LEVEL 2 OF TPR VARIABLE TO "NORMAL"##
lens$tpr[lens$tpr=="2"]="normal"

## CHECKING FOR THE CHANGES##
str(lens)




## QUESTION 1.H##
## CHECKING THE COUNT FOR EACH DATA TYPE##
table(lens$astigmatic)




## QUESTION 1.I##
## STORING ALL THE ROWS INTO DATAFRAME "LENS" WHICH DOES NOT CONTAINS TYPO##
lens=lens[lens$astigmatic!="g",]

## CHECKING THE COUNT FOR EACH DATA TYPE##
table(lens$astigmatic)




## QUESTION 1.H##
## REMOVING COLUMN INDEX COLUMN FROM DATA FRAME##
lens=lens[-1]

## CHECKING FOR THE CHANGES##
str(lens)

## Correct 10/10

###############################################################################################
## QUESTION.2##                                                                               #
###############################################################################################

library(tree)
lens$class<-as.factor(lens$class)
lens$age<-as.factor(lens$age)
lens$spec_pres<-as.factor(lens$spec_pres)
lens$astigmatic<-as.factor(lens$astigmatic)
lens$tpr<-as.factor(lens$tpr)
str(lens)

## QUESTION-2.A##
## SPLITING THE DATA IN TRAINING AND TESTING##
library(caret)
set.seed(7)
intrain <- createDataPartition(y = lens$class,p=0.70,list=F)
training<-lens[intrain,]
testing<-lens[-intrain,]


## QUESTION-2.B##  
model1<-tree(class~.,data = training)


## QUESTION-2.C##  
plot(model1)
text(model1,pretty=0)


## QUESTION-2.D##  
pred_class<-predict(model1 ,testing,type ="class")


## QUESTION-2.E## 
table(pred_class ,testing$class)


## QUESTION-2.F##  
## CHECKING FOR MISCLASIFICATION ERROR RATE 
## MISCLASIFICATION ERROR RATE: 0.1765 = 3 / 17##
summary(model1)



## QUESTION-2.G##  
mean(testing$class != pred_class)
## Misclassification error rate: 0


## QUESTION-2.H##  
#Since all actual are predicting well in test data we have error rate as 0
#But in train data it seemed to be worse and can be improved by bagging

## Since the sample here is small bootstrap should have provided better results..
## 9/10



###############################################################################################
## QUESTION.3##                                                                               #
###############################################################################################
## IMPORTING DATA SET##
## CHECKING FOR DATA TYPES AND DIMENSIONS##
food<-lens<-read.csv("food consumptions.csv",header = T, stringsAsFactors =  F)
str(food)
dim(food)

## QUESTION-3.1##
## CHECKING FOR MISSING VALUES##
sum(is.na(food))

## MISSING VALUE TREATMENT##
## REPLACING MISSING VALUES WITH MEAN OF THE VARIABLES##

for(i in 1:ncol(food)){
  if (is.numeric(food[,i])){
    food[is.na(food[,i]), i] <- mean(food[,i], na.rm = TRUE)
  }
}
View(food)

## AGAIN CHECKING FOR MISSING VALUES##
sum(is.na(food))

## RUNNING PCA ON FOOD DATASET##
pca_food =prcomp (food[-1] , scale =TRUE)
summary(pca_food)



## QUESTION-3.2##
## PLOTTING A LOADING PLOT OF P1 AGAINST P2##
biplot (pca_food, scale =0)



## QUESTION-3.3##
## CHECKING THE SUMMARY OF PCA(PRINCIPAL COMPONENETS)##
summary(pca_food)


## QUESTION3.4##
## CHECKING THE VARIATION OF PRINCIPAL COMPONENTS ACOSS THER VARIABLES##
pri_food$rotation

## PC1 TALKS ABOUT Tinned.fruit, Sweetener AND Frozen.veggies Jam SO WE CAN NAME IT AS "FRUITS AND VEGGIE".##

## PC2 TALKS ABOUT Crisp.bread, Instant.coffee , Powder.soup, Frozen.fish SO WE CAN NAME IT AS "INSTANT DRINKS AND FOOD".##

## Correct 10/10




###############################################################################################
## QUESTION.4##                                                                               #
###############################################################################################
## FETCHING THE IRIS DATASET##
## CHECKING FOR THE VARIABLE DATA TYPES##
data(iris)
head(iris)
str(iris)

## PUTTING DATA SET IN A NEW DATAFRAME##
ir<-iris

## SPLITING THE DATASET INTO TRAIN AND TEST##
set.seed(1)
## TRAIN
train=sample(1:nrow(ir),110)  
## TEST
test<-ir[-train,]



## QUESTION-4.1,4.2,4.3,4.4##
## CHECKING LINEAR DISCRIMENTAL ANALYSIS##
library(MASS)
library(ggplot2)

## PRIOR PROBALITIY OF CLASS, GROUP MEANS, COEFFICIENTS OF LINEAR DISCRIMINANTS##
iri_train<-lda(Species~.,data=ir,subset=train) 
iri_train 




## QUESTION-4.5##
## PLOTTING LD1 ANF LD2##
## PUTTING PREDICTIONS IN A DATA FRAME PRED## 
Pred<-data.frame(Species=predict(iri_train)$class,predict(iri_train)$x)

## PLOTTING PREDICTIONS ON SCATTER PLOT LD1 VS LD2##
ggplot(Pred, aes(x=LD1, y=LD2, col=Species) ) + 
  geom_point(size = 3, aes(pch = Species))



## QUESTION-4.6##
## PREDICTIONS##
iri_pred=predict(iri_train,iris$Species) 

## CREATING CONFUSION MATRIX
iri_class=iri_pred$class 
table(iri_class,iris$Species) 

## MISCLASIFICATION ERROR RATE##
mean(iri_class==iris$Species)

summary(Pred)

#Correct 10/10




