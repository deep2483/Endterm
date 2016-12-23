#End Term For Data Mining II
#***************************

#*** File saved from Word document ***


#*************Q 1 A Start *****************************************************************************************************
# Use read.csv and pass file location details.  Using str() to check 
# structure of data

lens <- read.csv("C:/Users/Vivek/Documents/Praxis - folders/class details/Data mining/Data/Lens.csv")	

str(lens)

table(lens)

#*************END END END Q1 A *****************************************************************************************************
 

#*************Q 2 A Start***********************************************************************************************************

# **************
# TO deepankar - B had one column name missing but Question C had mention of a column called class so using that to rename all columns
# **************


colnames(lens) <- c("index", "age", "spec_pres", "astigmatic","tpr","class") 

# Checking head of lens for colnames change
head(lens)
str(lens)

#*************END END END Q1 B *****************************************************************************************************


#*************Q1 C Start ***********************************************************************************************************

lens$age[lens$age == "1"]="young"
lens$age[lens$age == "2"]="pre-presbyopic"
lens$age[lens$age == "3"]="presbyopic"
lens$spec_pres[lens$spec_pres=="1"]="myope"
lens$spec_pres[lens$spec_pres=="2"]="hypermetrope"

lens
str(lens)
#*************END END END Q1 C *****************************************************************************************************



#*************Q1 D Start ***********************************************************************************************************

str(lens)

# Deepankar - I Initially imported the data with stringAsFactors = FALSE i will
# already have the astigmatic field as chr. But redoing to run the below command

lens$astigmatic <- as.character(lens$astigmatic)
str(lens) # To check structure

#*************END END END Q1 D *****************************************************************************************************




#*************Q1 E Start ***********************************************************************************************************

lens$astigmatic[lens$astigmatic=="1"]="no" 
lens$astigmatic[lens$astigmatic=="2"]="yes"


str(lens) #Check structure

#*************END END END Q1 E *****************************************************************************************************



#*************Q1 F and G Start ***********************************************************************************************************

lens$tpr[lens$tpr==1]="reduced"  #Converting to char string for tpr=1

str(lens) #confirming that tpr changed to char from int


lens$tpr[lens$tpr==2]="normal"

str(lens)  #Rechecking the structure to confirm changes

#*************END END END Q1 F and G *****************************************************************************************************



#*************Q1 H Start ***********************************************************************************************************

# Using a for loop to print the table command details looping through all columns except index

for (i in 2:ncol(lens)) { print(table(lens[i]))}

# We notice two things. Astigmatic has 4 levels where there is 2 missing data and one incorrect data "g"
# We will treat the missing data based on similar records while we will delete the g row as per I(question)

#*************END END END Q1 H *****************************************************************************************************


#*************Q1 I Start ***********************************************************************************************************

#I am using dplyr in this case to simply filter the records 

lens <- lens %>% filter(lens$astigmatic != "g")

str(lens) # confirm if 22 records remain

#*************END END END Q1 I *****************************************************************************************************



#*************Q1 J Start ***********************************************************************************************************

lens1 <- lens[,-which(names(lens)=="index")]  # Select all columns but the first column
str(lens1)  # Using a new data set to preseve original data
str(lens) # comparing with original data set

#*************END END END Q1 J *****************************************************************************************************



#*************Q2   Start ***********************************************************************************************************

library(tree)

lens1$class <- as.factor(lens1$class) # Convert class to factor
lens2 <- lens1 %>% mutate_if(is.character, as.factor) # Use dplyr and mutate_if to convert others to factors

#using lens2 as new data set for keeping in tact lens1 data set as an insurance

str(lens2) #check conversion

# Q2 A

# Before we start and due to the size of the data, the two missing values are computed based on following rules
# Based on final class, tpr, spec_press and age we find for pre-Presbyopic 
# class 3, tpr reduced, spec_pres Hypermetrope the astigmatic is no and similarly for the other missing value
# astigmatic is Yes


lens2$astigmatic[lens2$astigmatic=='' & lens2$age =="pre-presbyopic"] = "no"
lens2$astigmatic[lens2$astigmatic=='' & lens2$age =="presbyopic"] = "yes"
lens2  # CHECK UPDATES
lens1   # VALIDATE UPDATES

#split data for running a model
head(lens2)
lens2



## 70% as split size
smp_size <- floor(0.70 * nrow(lens2))

set.seed(1)  # set seed

train_ind <- sample(seq_len(nrow(lens2)), size = smp_size) #Create the sample

train <- lens2[train_ind, ] # cut train
test <- lens2[-train_ind, ] # cut test

test  #check data
train # Check data

model1 <- tree(class ~., data= train)  #Using tree command to build model
summary(model1)  # display summary


# Plot and text the tree to have a visual
plot(model1)
text(model1, pretty=0)

# Predict test datas class using the model

pred_class <- predict(model1, test, type="class")

# view the predicted numbers
pred_class


# create the confusion matrix to view the model fit 

table(pred_class, test$class)


# check test$ class value to view the misclassification Since the number of data is very less we can even find the misclassification easilly
test$class

# Find misclassification rate
summary(model1)

# misclass rate = 0.2667

mean(test$class != pred_class)  # value = 0.5714

# model rate is lower while the same is not applicable in the test data. The ways to improve
# the model include trying to create cross fold, c5.0 or other methods of sampling the
# data to avoid pattern based model creation.

#try bagging, boosting or random forest based algorithms to ensure we train the model to
#include data from all patterns while creating the model and then apply it on test data


# Question 3

# Read data from file and save as food

food <- read.csv("C:/Users/Vivek/Documents/Praxis - folders/class details/Data mining/Data/food.csv", stringsAsFactors=F)	

food1

# there is one column of Yoghurt that is missing for a country. We will drop this record

food1<- food %>% filter(food$Yoghurt != "NA")
food1 <- food1 %>% filter(food1$Sweetener != "NA")
food1 <- food1 %>% filter(food1$Biscuits != "NA")
food2 <- food1[-1]

str(food2)
summary(food2)


pr.out =prcomp(food2, scale=T)

str(food2)


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
pr.var

pve=pr.var/sum(pr.var)
pve
cumsum(pve)


plot(pve , xlab=" Principal Component ", ylab= "Proportion of
Variance Explained ", ylim=c(0,1) ,type='b')



plot(cumsum (pve), xlab="Principal Component", ylab ="Cumulative Proportion of Variance Explained", ylim=c(0,1) ,
     type='b')

new <- cbind(food1[1],pr.out$x)
new
head(new)


#Ideally the first two components shows kind of food eating pattern. First component primary shows fluid consumption pattern while 
# second component kind of provides information about frozen food etc. 


# First PCA - Around Soups, Coffee, Jam, Biscuits and mostly around break fast items
# Second PCA - Probably shows Frozen and preserved items. 


#PCA also shows countries that consume such products for example Ireland shows Frozen items, Margaine etc, while 
# Holland and england show garlic and olive oil....
#Aimilarly Denmark, Norway, Luxemburg and Germany form a similar cluster while france and Belgium form a similar cluster based on 
#Principal compoinents.
