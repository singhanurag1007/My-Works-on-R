##Import Data
data <- read.csv(file.choose())
head(data)

##Converting variables into factors
data$Gender <- as.factor(data$Gender)
data$Education <- as.factor(data$Education)
data$Occupation <- as.factor(data$Occupation)
data$location <- as.factor(data$location)
data$Income <- as.factor(data$Income)
data$medi <- as.factor(data$medi)
data$hi <- as.factor(data$hi)
data$hospital.preference <- factor(data$hospital.preference,levels = c(0,1))


##Creating train and test samples
library(caTools)
set.seed(12345)
split <- sample.split(data$hospital.preference,SplitRatio = 0.8)
train <- subset(data,split == T)
test <- subset(data,split == F)

##DT using rpart algorithm
library(rpart)
tree <- rpart(formula = hospital.preference ~ . ,data = train,method = "class")
library(rpart.plot)
rpart.plot(tree)
summary(tree)
rpart.plot(tree,1,extra = 106)
plotcp(tree)
printcp(tree)
ptree=prune(tree,cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
rpart.plot(ptree)

##Checking Confusion matrix on Test data
library(caret)
pred1 <- predict(object = tree,test[-10],type = "class")
confusionMatrix(pred1,test$hospital.preference)

##random forest model
library(randomForest)
rf <-randomForest(hospital.preference~.,data=train, ntree=500) 
print(rf)
floor(sqrt(ncol(data) - 10))
mtry <- tuneRF(train[-10],train$hospital.preference, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
rf <-randomForest(hospital.preference~.,data=train, mtry=best.m, importance=TRUE,ntree=500)
print(rf)

#Evaluate variable importance
importance(rf)
varImpPlot(rf)

# Prediction and Model Evaluation using Confusion Matrix
predRF <- predict(rf,test[-10])
confusionMatrix(predRF,test$hospital.preference)

##prediction on never visited
data1 <- read.csv(file.choose())
head(data1)
data1$Gender <-as.factor(data1$Gender)
data1$Education <-as.factor(data1$Education)
data1$Occupation <-as.factor(data1$Occupation)
data1$location <-as.factor(data1$location)
data1$Income <-as.factor(data1$Income)
data1$medi <-as.factor(data1$medi)
data1$hi <-as.factor(data1$hi)
data1$hospital.preference <-as.factor(data1$hospital.preference)

pred2<- predict(rf,data1)
table(pred2)
