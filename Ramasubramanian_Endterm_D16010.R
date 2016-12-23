##Question one
#1(a)Read the Csv file lenses.csv
data = read.csv(file = 'C:/Users/ramasubramaniam/Desktop/DM2_Endterm/lenses.csv', 
                stringsAsFactors = T,header=FALSE)
colnames(data) = c('Age_patient', 'specs_pres', 'astigmatic','tears_production','class')
##1(D)
str(data)
View(data)
summary(data$Age_patient)
class(data$Age_patient)

data2$Age_patient=as.character(data2$Age_patient)
data2$Age_patient=as.numeric(data2$Age_patient)
##1(I)
#Replace G values with the Maximum values

apply(data, 2, function(x){ 
  
  x[is.na(x)] <- names(which.max(table(x)))
  
  return(x) })

data2[is.na(data2)]<-1


##Question 2

data2=data

##Sample Split based on the data
##Converting all the variables to categorical variable

View(data2)
dim(data2) 
data2$Age_patient=as.factor(data2$Age_patient)
data2$class=as.factor(data2$class)
data2$specs_pres=as.factor(data2$specs_pres)
data2$astigmatic=as.factor(data2$astigmatic)
data2$tears_production=as.factor(data2$tears_production)

##1(B)and C AND E,F,G
cols <- c("Age_patient", "class","specs_pres","astigmatic","tears_production")
data2[,cols] <- data.frame(apply(data2[cols], 2, as.factor))

levels(data2$Age_patient) <- c("Young", "pre-presbyopic","presbyopic")
levels(data2$class) <- c("hard", "soft", "notfit")
levels(data2$specs_pres) <- c("myope", "hypermetrope")
levels(data2$astigmatic) <- c("no", "yes")
levels(data2$tears_production) <- c("reduced", "normal")

#creating Sample with train=70% and test=30%
indexes = sample(1:nrow(data2), size=0.30*nrow(data2))
##2(A)
# Split data
set.seed(1)
test = data2[indexes,]
dim(test) 
train = data2[-indexes,]
dim(train)
View(data2)
##2(F)
summary(data2)

library(tree)
library(caret)
library(ISLR)
##2(B)
basictree=tree(class~.,data = train)
##2(C)
plot(basictree)
text(basictree ,pretty =0)



##2(D)Predict
yhat=predict(basictree ,test,type="class")
lenses_test=test$class
lenses_test
plot(lenses_test)
plot(yhat)
abline (0,1)

##Mispecification/Confusion Matrix
##1(H) AND 2(E) AND 2(G) and 2(H)
table(lenses_test,yhat)



##Question3
library(ISLR)

##Reading the csv File
data_pca = read.csv(file = 'C:/Users/ramasubramaniam/Desktop/DM2_Endterm/foodconsumption.csv',header=TRUE)
states =row.names(data_pca)
head(data_pca)

class(data_pca$Apples)
##Replacing NA with mean of the particular column

data_pca$Powder.soup[is.na(data_pca$Powder.soup)] <- round(mean(data_pca$Powder.soup, na.rm = TRUE))
data_pca$Instant.coffee[is.na(data_pca$Instant.coffee)] <- round(mean(data_pca$Instant.coffee, na.rm = TRUE))
data_pca$Tea[is.na(data_pca$Tea)] <- round(mean(data_pca$Tea, na.rm = TRUE))
data_pca$Sweetener[is.na(data_pca$Sweetener)] <- round(mean(data_pca$Sweetener, na.rm = TRUE))
summary(data_pca)
str(data_pca)
for(i in 1:ncol(data_pca)){
  class(data_pca)=as.numeric(data_pca)
  data_pca[is.na(data_pca[,i]), i] <- mean(data_pca[,i], na.rm = TRUE)
}

View(data_pca)
names(data_pca)
##3(3)Variance Explained by the Model
apply(data_pca , 1, mean)
apply(data_pca , 2, var)

##3(1)Fitting the PCA Model
##Using PrComp we did PCA Analysis and looked at the summary of the data
pr.out =prcomp (data_pca[-1] , scale =TRUE)
##Totally 16 Pc were found
summary(pr.out)
names(pr.out)
pr.out$center


pr.out$scale
pr.out$rotation

pr.out$x
biplot (pr.out , scale =0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot (pr.out , scale =0)

pr.out$sdev
pr.var =pr.out$sdev ^2
sum=as.numeric()
sum=sum(pr.var)
pve=as.numeric
pve=pr.var/sum
pve

##Based on the Graph we have selected the Principle Components(L-Bend)
plot(pve , xlab=" Principal Component ", ylab= "Proportion of
     Variance Explained ", ylim=c(0,1) ,type='b')

##3(2)Loading Plot

plot(cumsum (pve), xlab="Principal Component", ylab ="Cumulative Proportion of Variance Explained", ylim=c(0,1) ,
     type='b')
##3(4)Name the PCA
##PCA-1(Frozen_foods)
##PCA-2(beverages)




##Question4
library(caret)
library(rpart)

#Load the Data#

View(iris)
data_iris=iris

# create a traindataset of 65%
validation_index <- createDataPartition(data_iris$Species, p=0.65, list=FALSE)
# select 35% of the data for testing
validation <- data_iris[-validation_index,]
data_iris <- data_iris[validation_index,]
##Finding the dimensions class and Levels in Species Column
dim(data_iris)
sapply(data_iris, class)
levels(data_iris$Species)

##4(A) Prior class distribution

# summarize the class distribution
percentage <- prop.table(table(data_iris$Species)) * 100
cbind(freq=table(data_iris$Species), percentage=percentage)


# split input and output
x <- data_iris[,1:4]
y <- data_iris[,5]


# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


##Accuracy of the Model using 10-fold Cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

##LDA
set.seed(7)
fit.lda <- train(Species~., data=data_iris, method="lda", metric=metric, trControl=control)
##Cart
set.seed(7)
fit.cart <- train(Species~., data=data_iris, method="rpart", metric=metric, trControl=control)



##Comparison between both the models
results <- resamples(list(lda=fit.lda,cart=fit.cart))
summary(results)
print(fit.lda)


# 4(6)Confustion Matrix
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)





