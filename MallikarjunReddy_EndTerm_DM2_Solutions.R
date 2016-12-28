

####   End Term DM2 _ Solution Script __ MallikarjunReddy __RollNo D1600_23rd Dec,2016  ######
Score 33/40

getwd()

#Prerequsite Pacakages
install.packages('MASS')
library(MASS)
library(caret)


#Question 1:-
#Solution for A:-
#Importing Csv file form machine to Rstudio  through read.csv command

lens=read.csv("C:/Users/MALLIKARJUN REDDY/Desktop/lenses.csv")
#Head listing top 6 oulmuns & rows of dataset
head(lens)
#str function gives the type of variables either numeric or character ect 
str(lens)

#Solution for Questio 1 B:-
#Function colnames lables the names for columuns for the dataset

colnames(lens) <- c("index","age","spec_pres","astigmatic","tpr","class")


#Solution for Questio 1 c:-
##we can introduce new variable with $ symbol along with dataset to create variables 
lens$age[lens$age == "1"]="young"
lens$age[lens$age == "2"]="pre-presbyopic"
lens$age[lens$age == "3"]="presbyopic"
lens$spec_pres[lens$spec_pres=="1"]="myope"
lens$spec_pres[lens$spec_pres=="2"]="hypermetrope"


#Solution for Questio 1 D :-
##converting to charcter for the variable astigmatic using function as.character
lens$astigmatic=as.character(lens$astigmatic)

#Solution for Questio 1 E :-
##changing to response in varible to 1 or 2 to yes  or no response 
lens$astigmatic[lens$astigmatic=="1"]="no"
lens$astigmatic[lens$astigmatic=="2"]="yes"


#Solution for Questio 1 F :-
## Relacing 1 with reduced with given function in columun tpr###

lens$tpr[lens$tpr==1]="reduced"

#Solution for Questio 1 G :-
## Relacing 2 with reduced with given function in column tpr ###
lens$tpr[lens$tpr=="2"]="normal"


#Solution for Questio 1 H:-
##using table  function listing yes and no in variable astigmatic##

table(lens$astigmatic)


#Solution for Questio 1 I:-
## removing g in the observations in the variable astigmatic##
lens=lens[lens$astigmatic!="g",]

#Solution for Questio 1 J:-
##with the '[]' these subset brackets we can drop varibles by using - sign 
##____indicating coulmn number 

lens=lens[-1]


## Correct 10/10

###  Question 2:-  ####
##loading tree package from library 
library(tree)
#Solution for Questio 2 A:-
#Spliting the dataset in specified ratio as in question and labeling it as Train & Test ##
spl=sample.split(lens$clas,SplitRatio=0.7)

Train=lens[spl==TRUE,]

Test=lens[spl==FALSE,]


#Solution for Questio 2 B:-
###Buinding tree model labeling it as model1###

model1=tree(class~.,data=Train)

#Solution for Questio2 C :-
## using plot function ploting the model1 and also using text comment to lable the plot ##
plot(model1)

text(model1)


#Solution for Questio2 D :-

##using predit() function predict the test dataset ##
pred_class=predict(model1,newdata = Test, type="class")


#Solution for Questio2 E :-
## Using table() function to summarise the predclass##
table(Test$class,pred_class)



#Solution for Questio2 F :-
## usingsummary() function to  summirise  model ##
summary(model1)



#Solution for Questio2 G :-

mean(Test$class != pred_class)

#Solution for Questio2 H :-

##The mer is worse on the test set because the model is not good. 


## How will you improve no answered 8/10

##Question 3:-

##Loadging datset foo-Consumptionfrom local drive using function read.csv##
food=read.csv("C:/Users/MALLIKARJUN REDDY/Desktop/food.csv")
str(food)
dim(food)


##checking for missing values and if any  Replacing by mean of the variable

for(j in 1:ncol(food)){
  if (is.numeric(food[,j])){
      food[is.na(food[,j]), j] <- mean(food[,j], na.rm = TRUE)
    }
  }
View(food)

sum(is.na(food))
##Checking missing value count ###

pca_food =prcomp (food[-1] , scale =TRUE)
summary(pca_food)


##Solution for Question 3-2:-
biplot (pca_food, scale =0)

##Solution for Question 3-3:-
summary(pca_food)
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

##Solution for Question 3-4:-
## Principal component 1  and Principal component 2 explins 50.50% of variations.

pca_food$rotation

## Principal component 1:- is more of  fruits:- fruits jam and tin iteams  
##It can be named  as "Fruits iteams "

## Principa; component 2 :-is more of   coffee, bread , youghart-Breakfast iteams 
##It can be  named  as "Breakfast iteams "

## Correct 10/10

##Question 4:-

# Load data
data(iris)

#Solution for Questio 4-1:-


head(iris, 3)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

r <- lda(formula = Species ~ ., 
         data = iris, 
         prior = c(1,1,1)/3)
r
#Solution for Questio 4-1:-
r$prior

#Solution for Questio 4-2:-
r$means
r$scaling
r$svd
prop = r$svd^2/sum(r$svd^2)

#Solution for Questio 4-3:-

r2 <- lda(formula = Species ~ ., 
          data = iris, 
          prior = c(1,1,1)/3,
          CV = TRUE)
 
head(r2$class)
#Solution for Questio 4-4:-
head(r2$posterior, 3)
  
train <- sample(1:150, 75)
 
r3 <- lda(Species ~ ., # training model
         iris, 
         prior = c(1,1,1)/3, 
         subset = train)
#Solution for Questio 4-4:- 
plda = predict(object = r, # predictions
               newdata = iris[-train, ])
 
head(plda$class) # classification result

 
head(plda$posterior, 3) # posterior prob.


head(plda$x, 3) # LD projections


#Solution for Question 4-5:-
##Plotting LDA1 and LDA2
#create data.frame
data_Pred<-data.frame(Species=predict(lda.fit)$class,predict(lda.fit)$x)
##Now Plotting LDA1 and LDA2
#Eqivalent plot with ggplot2 but without decision boundaries
library(ggplot2)
ggplot(data_Pred, aes(x=LD1, y=LD2, col=Species) ) + 
  geom_point(size = 3, aes(pch = Species))



#Solution for Question 4-6:-

predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
dataset=iris
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
 
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
 

# summarize accuracy of models
results <- resamples(list(lda=fit.lda))
summary(results)



# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
####output of confussion rate ####
#Statistics by Class:

#                     Class: setosa Class: versicolor Class: virginica
#Sensitivity                 1.0000            1.0000           1.0000
#Specificity                 1.0000            1.0000           1.0000
#Pos Pred Value              1.0000            1.0000           1.0000
#Neg Pred Value              1.0000            1.0000           1.0000
#Prevalence                  0.3333            0.3333           0.3333
#Detection Rate              0.3333            0.3333           0.3333
#Detection Prevalence        0.3333            0.3333           0.3333
#Balanced Accuracy           1.0000            1.0000           1.0000


## Correct solution but this is copied from interent 5/10
