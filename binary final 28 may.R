#Importing data 
setwd("C:/Users/HP/Desktop/project/zzzz-project final folder/binary logistic by R")
data <- read.csv('binary 4 significant.csv')
head(data)

##Factoring data
data$ hospital.preference =as.factor(data$ hospital.preference);
data$Gender=as.factor(data$Gender);
data$Education=as.factor(data$Education);
data$Occupation=as.factor(data$Occupation);
data$ location =as.factor(data$ location);
data$ Income =as.factor(data$ Income);

##Splitting data
library(caTools)
set.seed(12345)
split=sample.split(data,SplitRatio=0.8)
train=subset(data,split=="TRUE");
test=subset(data,split=="FALSE");

##Model with all variables
mymodel=glm(hospital.preference ~.,family="binomial", data=train)
step=step(mymodel, direction="both")
summary(step)

##Final model with significant variables
mymodel=glm(hospital.preference ~Age+ Occupation+ Number.of.family.members + Income,family="binomial", data=train)
step=step(mymodel, direction="both")
summary(step)

##Multicollinearity
library(car)
vif(step)

##Wald's test
library(survey)
regTermTest(step,"Age")
regTermTest(step,"Occupation")
regTermTest(step,"Number.of.family.members")
regTermTest(step,"Income")

##odds ratio 
exp(coef(step))

##Confusion matrix on testing data 
library(caret)
pred=predict(step,newdata=test[,-8],type="response") 
pred2=ifelse(pred<0.5,0,1)
confusionMatrix(table(test$ hospital.preference, pred2,dnn=list("Actual","Predicted")))

##Goodness of fit test
library(ResourceSelection)
hoslem.test(step$y,fitted(step))

##ROC Curve & ks plot 
library(InformationValue)
plotROC(actuals=train$hospital.preference,predictedScores=as.numeric(fitted(step)))
ks_plot(actuals = train$hospital.preference,predictedScores = as.numeric(fitted(step)))
ks_stat(actuals = train$hospital.preference,predictedScores = as.numeric(fitted(step)))
Concordance(actuals = train$hospital.preference,as.numeric(fitted(step)))

