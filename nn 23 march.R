data <- read.csv(file.choose())
head(data)

data$Gender <-as.factor(data$Gender)
data$Age<-scale(data$Age)
data$Number.of.family.members<-scale(data$Number.of.family.members)
data$Education <-as.factor(data$Education)
data$Occupation <-as.factor(data$Occupation)
data$location <-as.factor(data$location)
data$Income <-as.factor(data$Income)
data$medi <-as.factor(data$medi)
data$hi <-as.factor(data$hi) 
#data$hospital.preference <- as.factor(data$hospital.preference)

head(data)

library(fastDummies)
result <- fastDummies::dummy_cols(data)
head(result)

final <- result[c(-1,-3,-4,-6,-7,-8,-9)]
head(final)


library(caTools)
set.seed(12345)
split=sample.split(final,0.8)
trainset<- subset(final,split==T)
testset<- subset(final,split==F)

#Neural Network
library(neuralnet)
nn<- neuralnet(formula = hospital.preference ~., data=trainset, 
               hidden=c(7,3), act.fct = "logistic", linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)

temp_test<- testset[,-3]
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$hospital.preference, prediction = nn.results$net.result)
results


roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)



#prediction
data1 <- read.csv(file.choose())
head(data1)

data1$Gender <-as.factor(data1$Gender)
data1$Age<-scale(data1$Age)
data1$Number.of.family.members<-scale(data1$Number.of.family.members)
data1$Education <-as.factor(data1$Education)
data1$Occupation <-as.factor(data1$Occupation)
data1$location <-as.factor(data1$location)
data1$Income <-as.factor(data1$Income)
data1$medi <-as.factor(data1$medi)
data1$hi <-as.factor(data1$hi)
#data1$hospital.preference <- as.factor(data1$hospital.preference)

head(data1)

library(fastDummies)
result1 <- fastDummies::dummy_cols(data1)
head(result1)

final1 <- result1[c(-1,-3,-4,-6,-7,-8,-9)]
head(final1)

nn.results <- compute(nn, final1)
results2 <- data.frame(prediction = nn.results$net.result)
results2
roundedresults1<-sapply(results2,round,digits=0)
roundedresults1
table(roundedresults1)
