library("readxl")
library("e1071")
library("caret")

data <- read_excel("~/Churn.xls")

set.seed(1)
index=sample(1:nrow(data),0.8*nrow(data))
train<-data[index,]
test<-data[-index,-8]

fit1<-glm(Churn~1, data = train, family = binomial(link = "logit"))
summary(fit1)

fit2<-glm(Churn~.,family = binomial(link = "logit"), data = train)
summary(fit2)

step(fit1, scope = list(upper=fit2), direction = "both", 
     test = "Chisq",data= train)

fit3<-glm(formula = Churn ~ `Int'l Plan` + `Day Mins` + `CustServ Calls` + 
            `VMail Plan` + `Eve Charge` + `Intl Charge` + `Intl Calls` + 
            `Night Mins` + `Intl Mins` + `VMail Message`, 
              family = binomial(link = "logit"), data = train)

summary(fit3)

p<-predict(fit3, newdata = train[,-8], type = "response")


pred1<-ifelse(p<0.6,0,1) #accuracy = 86.42%

c1<-table(train$Churn,pred1, dnn = list("actual","predicted"))
cm1<-confusionMatrix(c1)
cm1$overall["Accuracy"]


pred2<-ifelse(p<0.5,0,1) #accuracy = 86.38%
c2<-table(train$Churn,pred2, dnn = list("actual","predicted"))
cm2<-confusionMatrix(c2)
cm2$overall["Accuracy"]

pred3<-ifelse(p<0.4,0,1) #accuracy = 86.09%
c3<-table(train$Churn,pred3, dnn = list("actual","predicted"))
cm3<-confusionMatrix(c3)
cm3$overall["Accuracy"]

library("InformationValue")

plotROC(actuals = train$Churn, predictedScores = as.numeric(fitted(fit3)))


library(Rserve)
Rserve()

