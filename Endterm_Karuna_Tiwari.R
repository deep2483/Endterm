# Question 1
# A.Use read.csv() command to load the lenses.csv lens and store it in lens. 
# Use the str() command to see lens. 

getwd()       #File is stored in current working directory
lens <- read.csv("Lenses.csv",stringsAsFactors = F,header=F)
str(lens)

# B.	Notice there are no column names. The column names are as follows
# index, age, spec_pres, astigmatic, tpr. Use one line code to change the column
# names to the aforementioned names.

colnames(lens) = c('index', 'age', 'spec_pres', 'astigmatic', 'tpr', 'contact_lenses')
# Used 'Contact_lenses' for the last column as it will useful for question 2
head(lens)

# C. Type the code lens$age[lens$age == "1"]="young" 
#Use the same format to change all the data to its names for the age and spec_pres variables.

lens$age[lens$age == "1"]="young"
lens$age[lens$age == "2"]="pre-presbyopic"
lens$age[lens$age == "3"]="presbyopic"
lens$spec_pres[lens$spec_pres=="1"]="myope"
lens$spec_pres[lens$spec_pres=="2"]="hypermetrope"


# D.	Use the str() command to see the changes. Also notice that the astigmatic column
# is a factor that is also storing numbers as characters. To get all of them in the 
# same format, let's convert it to character. Use the code as.character () to convert
# this column data type to character.
str(lens)
lens$astigmatic=as.character(lens$astigmatic)

# E.	Now change the astigmatic column data to the right names
lens$astigmatic[lens$astigmatic=="1"]="no"
lens$astigmatic[lens$astigmatic=="2"]="yes"

# F.	Use the following code to replace the 1 with "reduced" in the tpr column 
# lens$tpr[lens$tpr==1]="reduced". Now type str(lens) to see the dataframe. 
lens$tpr[lens$tpr==1]="reduced"
str(lens)

## G.	Go ahead and replace 2 in the tpr column with "normal"
lens$tpr[lens$tpr=="2"]="normal"

# H.	use the table() command to see the counts of each data type
table(lens$age)
table(lens$spec_pres)
table(lens$astigmatic)
table(lens$tpr)
table(lens$contact_lenses)

# I.	Notice that there is a g in the count. That could possibly be a typo. We can 
# go ahead and remove that row since there is only one row with that typo. Hint: You
# can select all rows that does not have that typo and store it back in the lens data
# frame.
typo = lens[, 4]!="g" 
typo
lens =lens[typo,]
lens
s
# OR
lens=lens[lens$astigmatic!="g",]

# J.We realized that the index column is not necessary for our modelling purposes. So
#lets remove the index column
lens=lens[-1]

str(lens) #check the structure
View(lens) 

## Correct 10/10
##################################################################################
#QUESTION 2
#Load the tree library and Convert all the features (columns) into factors, including 
#the class column and do following:
library(tree)

# Loading the data
getwd()       #File is stored in current working directory
data <- read.csv("Lenses.csv",stringsAsFactors = F,header=F)

# Column names
colnames(data) = c('index', 'age', 'spec_pres', 'astigmatic', 'tpr', 'contact_lens')

# Checking the data
head(data)
str(data)

# Dropping the Index
data=data[-1]

# Encoding variables as factors 
data$age = as.factor(data$age)
data$spec_pres = as.factor(data$spec_pres)
data$astigmatic = as.factor(data$astigmatic)
data$tpr = as.factor(data$tpr)
data$contact_lens = as.factor(data$contact_lens)

# Checking the level of each variable
levels(data[,1])
levels(data[,2])
levels(data[,3])
levels(data[,4])
levels(data[,5])

#Structure of the data
str(data)

# A.	Use the sample methods that you learnt in the class to split the data into two sets 
# with a Split Ratio of 0.7. Hint: Use any package of your choice. Store the results into
# Train and Test.

library(caret)
training_index <- createDataPartition(data$contact_lens, p=0.7, list=FALSE)

# select 70% of the data for training
Train <- data[training_index,]
table(Train$contact_lens)

# use the remaining 30% of data validation
Test <- data[-training_index,]
table(Test$contact_lens)

# B.	Use the tree() command to build the model. Use class as the target variable and
# everything else as the predictor variable. Also, use the Train variable as the data 
# source. Store the model in a variable called model1
model1 <- tree(contact_lens~.,data = Train)
summary(model1)

# C.	Use the plot() command to plot the model and use the text() command to add the text.
plot(model1)
text(model1, pretty =0)

# D.	Use the predict() command to predict the classes using the Test dataset. We want
# to predict the classes. Store this in the variable pred_class
pred_class <-predict(model1,Test,type="class")

# E.	Use the table() command to print the confusion matrix. Hint: You are comparing 
# the class from the Test set and the predicted vector. This tells you whether the 
# model is answering anything right or wrong
table(Test$contact_lens,pred_class)
confusionMatrix(Test$contact_lens,pred_class) # Accuracy 67%

# F.	Use the summary() to print the summary of the model and note the misclassification
# error rate.
summary(model1)
# misclassification error rate = 0.2778

# G.	Now find the misclassification error rate of the model on the Test data. Use the
# formula. mean(Test$class != pred_class)
mean(Test$class != pred_class)

# H.	Compare the two misclassification error rates and determine which is worse and why.
# How can we improve the model?

## Not answered Total 9/10
#####################################################################3
# Question 3

# Loading the data
getwd()       #File is stored in current working directory
Food <- read.csv("Food.csv",stringsAsFactors = F,header=T)
str(Food)
head(Food)

# 1.	Fit a PCA model to the data
# Since it will take only numeric , remove the first column
Food1 = Food[-1]

# Removing rows 11, 14, 15 as missing
remove = c(11, 14, 15)
Food2 = Food1[-remove,]
View(Food2)

pr.out =prcomp (Food2 , scale = TRUE)

#Eigen values of Correlation matrix
summary(pr.out)
names(pr.out)
pr.out$center

# Eigen vectors
pr.out$scale
pr.out$rotation

#2.	Plot a loadings plot of p1 against p2
pr.out$x
biplot (pr.out , scale =0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot (pr.out , scale =0)

# 3.	Using the output given by R calculate
# the variance explained by generated principal components
pr.out$sdev
pr.var =pr.out$sdev ^2
pr.var

pve=pr.var/sum(pr.var)
pve
cumsum(pve)

# Variaance explained
# 0.3359 0.1671 0.1493 0.08575 0.06619 0.05657 0.05003 0.03807 
# 0.02409 0.01479 0.01093 0.00129 0.000e+00
# Cum Variance explained 
# 0.3359 0.5030 0.6523 0.73803 0.80421 0.86078 0.91082 0.94889 
# 0.97298 0.98777 0.99871 1.00000 1.000e+00

plot(pve , xlab=" Principal Component ", ylab= "Proportion of
     Variance Explained ", ylim=c(0,1) ,type='b')

plot(cumsum (pve), xlab="Principal Component", ylab ="Cumulative Proportion of Variance Explained", ylim=c(0,1) ,
     type='b')

# 4.	How will you interpret the first two principal components? Can you
# name them if yes what will be the name
#PC1 -> Tinned Food
#PC2 -> Frozen Food

## Correct 10/10
########################################################333
# Question 4
library(MASS)
library(ggplot2)
irisdata = iris

# splitting the data for training and validation purpose
set.seed(1)  # So that every time we get the same split
train=sample(1:nrow(irisdata),100)  # (2/3rds Observations for train and 1/3rd for test)
table(irisdata$Species[train])
table(irisdata$Species[-train])

# Validation data
validation1 = irisdata[-train,]
str(validation1)

#LDA
lda.fit=lda(Species~.,data=irisdata,subset=train)

# 1)	Display the prior class probabilities.
print(lda.fit$prior)
# 2) . Display Group Means
print(lda.fit$ means)

#3)	Display the coefficients of the discriminant model generated
print(lda.fit$ scaling)

# 4)	What are the posterior probabilities
lda.pred=predict(lda.fit,validation1)
lda.class=lda.pred$class
lda.pred$posterior

# 5)Plot LD1 against LD2 and colour the points using species.
plot(lda.fit)
ggplot(lda.fit$ scalin, aes(LD1, LD2)) + 
  geom_point() + 
  stat_chull(fill = NA, colour = "Species")

# 6)	Predict using the predict function and create a confusion
# matrix. What is the misclassification error rate(Compute this in R)
table(lda.class,validation1$Species) # Actual vs Predicted
mean(lda.class==validation1$Species) # no of times predicted correctly- accuracy

## Correct 10/10
           
           
