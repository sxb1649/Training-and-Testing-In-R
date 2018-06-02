# Training-and-Testing-In-R

BUAN 6356.003  Spring 2017 (Johnston) Project 3: Cross Validation and Ensemble Estimation Due by:   

26 Mar 2017 Description: 

This project is set up in 3 segments.  Deliverables for each segment are cumulative.  Use alpha = 0.05 wherever applicable.  Do NOT save any information to disk from your submitted R code.  Use a random number seed value of 385766359.  Calculate any needed residual values as original value – fitted value. Code for each segment is to be submitted through eLearning.  Multiple code submissions are allowed. Each instance of submitted code will be run and the deliverables compared to a reference result. Segment 1. Download the file “Boston.csv” from the BUAN_6356>projects area.  Use this data to generate a “training” sample (90%) and a “testing” sample (10%).  Define and extract the “testing” sample first.  Keep all variables originally in each sample.  Use the “training” set to build a final regression model explaining the median home value (“medv”) using all numeric variables in the data set. Calculate fitted values for the “testing” sample. Update both “training” and “testing” data frames to include fitted values (named “olsFit”) and residuals (named “olsResid”). Segment 1 Deliverables: 1. Updated “training” data frame   (name: housingTrain) 2. Updated “testing” data frame   (name: housingTest) 3. Final regression model results   (name: modelTrain) 
Segment 2. After successfully performing the actions specified in Segment 1, create regression trees to explain the median home value (“medv”) from the original variables in the “training” sample using both rpart() and tree().  Calculate fitted values for the “testing” sample. Update both “training” and “testing” data frames to include fitted values for each regression tree procedure (named “rpartFit” and “treeFit”) as well as residuals (named “rpartResid” and “treeResid”). Segment 2 Deliverables: 1. Updated “training” data frame   (name: housingTrain) 2. Updated “testing” data frame   (name: housingTest) 3. Final rpart() model results    (name: rpartTrain) 4. Final tree() model results    (name: treeTrain Segment 3. After successfully performing the actions specified in Segment 2, use the “training” set to build a final regression model explaining the median home value (“medv”) using only the fitted data values now present in data frame “housingTrain”. Calculate fitted values from this new model for the “testing” sample. Update both “training” and “testing” data frames to include fitted values for the final regression procedure (named “eFit”) as well as residuals (named “eResid”). Segment 3 Deliverables: 1. Updated “training” data frame   (name: housingTrain) 2. Updated “testing” data frame   (name: housingTest) 3. Final regression model results   (name: eTrain) 




#Segment 1
myDir<-("C:/data/BUAN6356")
#Set working directory and load the data in the work environment
setwd(myDir)

myFile<-"Boston.csv"
regData<-read.csv(file=myFile,header = TRUE,sep=",")

#Checking the Contents of the file
head(regData)

set.seed(385766359)							# from random.org,gives reproduceability
u01			<-	runif(length(regData$medv))		# random seq of U(0,1)
index			<-	1:length(u01)				# any variable we know the name of
shuffled		<-	index[order(u01)]
n_10pc			<-	floor(0.10 * length(u01))		# how many is 10%?
n_10pc
sample_10pc		<-	shuffled[1:n_10pc]			# indexes for 10% sample
sample_10pc
reg_test		<-	regData[sample_10pc, ]			# testing set (10%)
length(reg_test$medv)
sample_90pc		<-	shuffled[(n_10pc+1):length(u01)]	# indexes for remaining 90%
reg_train		<-	regData[sample_90pc, ]			# training set (90%)
length(reg_train$medv)



#Keep the results of the model and generate detailed reports
#Generating the linear model including only numeric values and comparing with alpha=0.05, we subtract id,indus and age

modelTrain<-lm(medv~.-id-indus-age,data=reg_train)
summary(modelTrain)

#Getting the fitted valaues and residual values

housingTest<-reg_test
housingTrain<-reg_train

#Updating the test and train final models with fitted values
housingTrain$olsFit<-predict(modelTrain,housingTrain)
housingTest$olsFit<-predict(modelTrain,housingTest)

#Updating the test and train final models with residual values
housingTest$olsResid <- housingTest$medv-housingTest$olsFit

housingTrain$olsResid <- housingTest$medv-housingTrain$olsFit


str(housingTrain)
str(housingTest)

head(housingTrain)
head(housingTest)



#Segment2
myDir<-("C:/data/BUAN6356")
#Set working directory and load the data in the work environment
setwd(myDir)

myFile<-"Boston.csv"
regData<-read.csv(file=myFile,header = TRUE,sep=",")

#Checking the Contents of the file
head(regData)

set.seed(385766359)							# from random.org,gives reproduceability
u01			<-	runif(length(regData$medv))		# random seq of U(0,1)
index			<-	1:length(u01)				# any variable we know the name of
shuffled		<-	index[order(u01)]
n_10pc			<-	floor(0.10 * length(u01))		# how many is 10%?
n_10pc
sample_10pc		<-	shuffled[1:n_10pc]			# indexes for 10% sample
sample_10pc
reg_test		<-	regData[sample_10pc, ]			# testing set (10%)
length(reg_test$medv)
sample_90pc		<-	shuffled[(n_10pc+1):length(u01)]	# indexes for remaining 90%
reg_train		<-	regData[sample_90pc, ]			# training set (90%)
length(reg_train$medv)



#Keep the results of the model and generate detailed reports
#Creating regression trees to explain the median home value (“medv”) from the original variables in the “training” sample using both rpart() and tree()

require(rpart)
require(tree)

rpartTrain<-rpart(medv~.-id,data=reg_train)
summary(rpartTrain)

treeTrain<-tree(medv~.-id,data=reg_train)
summary(treeTrain)

#Getting the fitted valaues and residual values

housingTest<-reg_test
housingTrain<-reg_train

#Updating the test and train final models with fitted values of rpart function

housingTrain$rpartFit<-predict(rpartTrain,housingTrain)
housingTest$rpartFit<-predict(rpartTrain,housingTest)

#Updating the test and train final models with residual values
housingTest$rpartResid<- housingTest$medv-housingTest$rpartFit 
housingTrain$rpartResid<-housingTrain$medv-housingTrain$rpartFit 

#Updating the test and train final models with fitted values of tree function

housingTrain$treeFit<-predict(treeTrain,housingTrain)
housingTest$treeFit<-predict(treeTrain,housingTest)

#Updating the test and train final models with residual values
housingTest$treeResid<- housingTest$medv-housingTest$treeFit 
housingTrain$treeResid<-housingTrain$medv-housingTrain$treeFit 

str(housingTrain)
str(housingTest)

head(housingTrain)
head(housingTest)

#Segment3
myDir<-"C:/data/BUAN6356"

myData  <-      "Boston.csv"
setwd(myDir) 
getwd()
set.seed(385766359)
train<-0.90
test<-1-train
readData<-read.csv(file=myData,header=TRUE,sep=",")
summary(readData)
randseq<-runif(length(readData$medv))
rank<-order(randseq)
n_test1<-floor(test*length(randseq))
reg1_test<-readData[rank[1:n_test1],]
reg1_train<-readData[ rank[(n_test1+1):length(randseq)],]
str(reg1_train)
modelTrain0<-lm(medv~.-id,data=reg1_train)

summary(modelTrain0)

modelTrain1<-lm(medv~.-id-indus-age,data=reg1_train)

summary(modelTrain1)

anova(modelTrain0,modelTrain1)

modelTrain<-modelTrain1

reg1_test1<-reg1_test
reg1_train1<-reg1_train
reg1_test$olsFit<-predict(modelTrain,reg1_test)
reg1_train$olsFit<-predict(modelTrain,reg1_train)
reg1_train$olsResid<-resid(modelTrain)
reg1_test$olsResid<-reg1_test$medv-predict(modelTrain,reg1_test)

housingTrain<-reg1_train
housingTest<-reg1_test
library("rpart")
library("tree")
rpartTrain<-rpart(medv~.-id,data=reg1_train1,method="anova")
treeTrain<-tree(medv~.-id,data=reg1_train1)
housingTest$rpartFit<-predict(rpartTrain,reg1_test1)
housingTrain$rpartFit<-predict(rpartTrain,reg1_train1)
housingTest$treeFit<-predict(treeTrain,reg1_test1)
housingTrain$treeFit<-predict(treeTrain,reg1_train1)
housingTrain$rpartResid<-resid(rpartTrain)
housingTrain$treeResid<-resid(treeTrain)
housingTest$rpartResid<-reg1_test1$medv-predict(rpartTrain,reg1_test1)
housingTest$treeResid<-reg1_test1$medv-predict(treeTrain,reg1_test1)

summary(housingTest)
summary(housingTrain)
eTrain<-lm(medv~olsFit+rpartFit+treeFit,data=housingTrain)
summary(eTrain)
#Checking the insignifacnat one and removing from model
eTrain <- lm(medv ~ olsFit+treeFit, data=housingTrain)
summary(eTrain)
housingTrain$eFit<-predict(eTrain,housingTrain)
housingTest$eFit<-predict(eTrain,housingTest)
housingTrain$eResid<-resid(eTrain)
housingTest$eResid<-housingTest$medv-predict(eTrain,housingTest)
summary(housingTrain)

summary(housingTest)
