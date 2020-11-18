############ Importing Data Set & Installing Required Libraries #########

setwd("C:/Users/Sakshi/Desktop/Great Learning/Sample R Projects/Customer Churn Prediction")
library(readxl)
cell=read_excel(file.choose(),2)
View(cell)
library(caTools)
library(carData)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(DataExplorer)
library(tidyverse)
library(pscl)
library(ROCR)
library(caret)

############## Exploratory Data Analysis #########################

str(cell)
summary(cell)
dim(cell)
names(cell)
sum(is.na(cell))
plot_intro(cell)
cell$Churn=as.factor(cell$Churn)
cell$ContractRenewal=as.factor(cell$ContractRenewal)
cell$DataPlan=as.factor(cell$DataPlan)
numeric.var <- sapply(cell, is.numeric)
corr.matrix <- cor(cell[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method = "number")

#### How many churns in this dataset #####
ggplot(cell, aes(x = Churn))+
  geom_histogram(stat = "count", fill = c("sky blue", "orange"))
attach(cell)

plot_histogram(cell)
plot_density(cell)

############ Splitting the Test & Train Dataset ######################


split = sample.split(cell$Churn, SplitRatio = 0.70)
Train=subset(cell, split==TRUE)
Test=subset(cell, split==FALSE)
dim(Train)
dim(Test)
prop.table(table(Train$Churn))
prop.table(table(Test$Churn))

############Logistics Regression - Model 1 #################

Model1=glm(Churn~., family = binomial(link='logit'), data=Train)
summary(Model1)
#Likelihood ratio test
anova(Model1, test="Chisq")
pR2(Model1)
exp(coef(Model1)) # Odds Ratio
exp(coef(Model1))/(1+exp(coef(Model1))) # Probability


pred<-predict(Model1,newdata=Train,type="response")
y_pred_num <- ifelse(pred>0.5,1,0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- Train$Churn
confusionMatrix(y_pred,y_act,positive="1")



ROCRpred <- prediction(as.numeric(pred), as.numeric(Train$Churn))
ROCRpref <- performance(ROCRpred,"auc")
auc_lr <- as.numeric(ROCRpref@y.values)
auc_lr

perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("Train Data AUC = ",format(auc_lr, digits=5, scientific=FALSE)))



## For Test DataSet ####
pred<-predict(Model1,newdata=Test,type="response")
y_pred_num <- ifelse(pred>0.5,1,0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- Test$Churn
confusionMatrix(y_pred,y_act,positive="1")


ROCRpred <- prediction(as.numeric(pred), as.numeric(Test$Churn))
ROCRpref <- performance(ROCRpred,"auc")
auc_lr <- as.numeric(ROCRpref@y.values)
auc_lr

perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("Test Data AUC = ",format(auc_lr, digits=5, scientific=FALSE)))


############# Model 2 #################

Model2 <- step(glm(Churn ~ ., family = binomial(link='logit'), data=Train))
Model3 = glm(Churn ~ RoamMins+OverageFee+DataPlan+DayMins+ContractRenewal+CustServCalls,family = binomial(link='logit'), data=Train)
summary(Model3)
anova(Model3, test="Chisq")
pR2(Model3)
exp(coef(Model3)) # Odds Ratio
exp(coef(Model3))/(1+exp(coef(Model3))) # Probability


pred<-predict(Model3,newdata=Train,type="response")
y_pred_num <- ifelse(pred>0.5,1,0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- Train$Churn
confusionMatrix(y_pred,y_act,positive="1")

ROCRpred <- prediction(as.numeric(pred), as.numeric(Train$Churn))
ROCRpref <- performance(ROCRpred,"auc")
auc_lr <- as.numeric(ROCRpref@y.values)
auc_lr

perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("Train Data AUC = ",format(auc_lr, digits=5, scientific=FALSE)))


## For Test DataSet ####
pred<-predict(Model3,newdata=Test,type="response")
y_pred_num <- ifelse(pred>0.5,1,0)
y_pred <- factor(y_pred_num, levels=c(0,1))
y_act <- Test$Churn
confusionMatrix(y_pred,y_act,positive="1")

ROCRpred <- prediction(as.numeric(pred), as.numeric(Test$Churn))
ROCRpref <- performance(ROCRpred,"auc")
auc_lr <- as.numeric(ROCRpref@y.values)
auc_lr

perf_ROC <- performance(ROCRpred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot")
text(0.5,0.5,paste("Test Data AUC = ",format(auc_lr, digits=5, scientific=FALSE)))

