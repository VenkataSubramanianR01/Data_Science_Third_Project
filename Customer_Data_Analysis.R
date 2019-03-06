rm(list = ls())
setwd("D:/Data Science/Third_Project")
######LOad Customer Data#######
customer_train_data=read.csv("Train_data.csv")
customer_test_data=read.csv("Test_data.csv")

############Data Preprocessing Techniques##########3
#####Missing Value Analysis##############
library("DMwR")
customer_train_data=knnImputation(customer_train_data,k=5)
customer_test_data=knnImputation(customer_test_data,k=5)
########Outlier Analysis#############
numeric_train_data=sapply(customer_train_data, is.numeric)
numeric_test_data=sapply(customer_test_data, is.numeric)
numeric_train_values=customer_train_data[,numeric_train_data]
numeric_test_values=customer_test_data[,numeric_test_data]
train_colnames=colnames(numeric_train_values)
test_colnames=colnames(numeric_test_values)
#####Outlier Analysis on Train Data###########
library("ggplot2")
for (i in 1:length(train_colnames))
{
  assign(paste0("gn_train",i), ggplot(aes_string(y = (train_colnames[i]), x = "Churn"), data = subset(customer_train_data))+ 
           stat_boxplot(geom = "boxplot", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "yellow" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=train_colnames[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",train_colnames[i])))
}
#plotting plots together
gridExtra::grid.arrange(gn_train1,gn_train2,gn_train3,gn_train4,ncol=4)
gridExtra::grid.arrange(gn_train5,gn_train6,gn_train7,gn_train8,ncol=4)
gridExtra::grid.arrange(gn_train9,gn_train10,gn_train11,gn_train12,ncol=4)
gridExtra::grid.arrange(gn_train13,gn_train14,gn_train15,gn_train16,ncol=4)

plot(customer_train_data$number.vmail.messages,customer_train_data$Churn, pch = 16, cex = 1.3, col = "red", main = "Vmail Messages Code against Churn Rate", xlab = "Churn", ylab = "Voice Mail")
qqplot(customer_train_data$total.eve.calls,customer_train_data$Churn,plot.it = TRUE)
##########Treating Outliers for Train Data##############
col_names=c("number.vmail.messages","area.code")
train_colnames=train_colnames[which(!train_colnames %in% col_names)]
for (i in train_colnames) {
  print(i)
  val=customer_train_data[,i][customer_train_data[,i] %in% boxplot.stats(customer_train_data[,i])$out]
  customer_train_data[,i][customer_train_data[,i] %in% val]=NA
}
customer_train_data=knnImputation(customer_train_data,k=5)
write.csv(customer_train_data,file = "Outliers_Removed_Train_Data.csv")
#########Outlier Treating for Test Data###########
for (i in 1:length(test_colnames))
{
  assign(paste0("gn_test",i), ggplot(aes_string(y = (test_colnames[i]), x = "Churn"), data = subset(customer_test_data))+ 
           stat_boxplot(geom = "boxplot", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "yellow" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=test_colnames[i],x="Churn")+
           ggtitle(paste("Box plot of Churn for",test_colnames[i])))
}
gridExtra::grid.arrange(gn_test1,gn_test2,gn_test3,gn_test4,ncol=4)
gridExtra::grid.arrange(gn_test5,gn_test6,gn_test7,gn_test8,ncol=4)
gridExtra::grid.arrange(gn_test9,gn_test10,gn_test11,gn_test12,ncol=4)
gridExtra::grid.arrange(gn_test13,gn_test14,gn_test15,gn_test16,ncol=4)

test_colnames=test_colnames[which(!test_colnames %in% col_names)]
for (i in test_colnames) {
  print(i)
  val=customer_test_data[,i][customer_test_data[,i] %in% boxplot.stats(customer_test_data[,i])$out]
  customer_test_data[,i][customer_test_data[,i] %in% val]=NA
}
customer_test_data=knnImputation(customer_test_data,k=5)
write.csv(customer_test_data,file = "Outliers_Removed_Test_Data.csv")
#################Feature Selection for Train Variables#####################
train_data_numeric=sapply(customer_train_data, is.numeric)
train_data_values=customer_train_data[,train_data_numeric]
train_data_cols=colnames(train_data_values)
library("corrgram")
corrgram(customer_train_data[train_data_cols],order = F,upper.panel = panel.pie,text.panel = panel.txt,
         main="Correlation plot")
###Dimension Reduction based on Correlation Plot###
customer_train_data=subset(customer_train_data,select=-c(total.day.charge,total.eve.charge,total.night.charge,total.intl.charge))

############Feature Selection for Test Variables##############
test_data_numeric=sapply(customer_test_data, is.numeric)
test_data_values=customer_test_data[,test_data_numeric]
test_data_cols=colnames(test_data_values)
corrgram(customer_test_data[test_data_cols],order = F,upper.panel = panel.pie,text.panel = panel.txt,
         main="Correlation plot")
###Dimension Reduction based on Correlation Plot###
customer_test_data=subset(customer_test_data,select=-c(total.day.charge,total.eve.charge,total.night.charge,total.intl.charge))

##############Chi SQuare Test for Train Data#################
train_data_index=sapply(customer_train_data, is.factor)
train_data_val=customer_train_data[,train_data_index]
train_col_values=colnames(train_data_val)
for (i in train_col_values) {
  print(i)
  print(chisq.test(table(train_data_val$Churn,train_data_val[,i])))
}
###Dimesnion Reduction based on ChiSquare Test##########
customer_train_data=subset(customer_train_data,select=-c(phone.number))

##############Chi SQuare Test for Test Data#################
test_data_index=sapply(customer_test_data, is.factor)
test_data_val=customer_test_data[,test_data_index]
test_col_values=colnames(test_data_val)
for (i in test_col_values) {
  print(i)
  print(chisq.test(table(test_data_val$Churn,test_data_val[,i])))
}
###Dimesnion Reduction based on ChiSquare Test##########
customer_test_data=subset(customer_test_data,select=-c(phone.number))

##############Machine Learning###################
library("randomForest")
RF_Model=randomForest(Churn ~.,data=customer_train_data,importance=TRUE,ntree=500)
importance(RF_Model,type = 1)
###Dimension Reduction after identifying important variables###
customer_train_data=subset(customer_train_data,select=-c(area.code,account.length,total.day.calls,total.eve.calls,total.night.calls,state))
RF_Model_Test=randomForest(Churn ~.,data=customer_test_data,importance=TRUE,ntree=500)
importance(RF_Model_Test,type=1)
####Check for Collinearity####
customer_test_data=subset(customer_test_data,select=-c(area.code,account.length,total.day.calls,total.eve.calls,total.night.calls,state))
train_cor_val=sapply(customer_train_data, is.numeric)
train_cor_data=customer_train_data[,train_cor_val]
library("usdm")
vifcor(train_cor_data,th=0.9)

#############Logistic Regression###############
logit_model = glm(Churn ~ ., data = customer_train_data, family = "binomial")
summary(logit_model)
logit_predictions=predict(logit_model,newdata = customer_test_data,type='response')
logit_predictions=ifelse(logit_predictions>0.5,1,0)
confusion_matrix_LR=table(customer_test_data$Churn,logit_predictions)
#confusion_matrix
library("caret")
#confusionMatrix(confusion_matrix_LR)
False_Negative_Rate_LT=(19/(19+1424))#1.3


############Decision Tree############
library("C50")
decisiontree_model=C5.0(Churn ~.,data = customer_train_data,trails=500,rules=TRUE)
summary(decisiontree_model)
decisiontree_predictions=predict(decisiontree_model,customer_test_data[,-10],type="class")
confMatrix_dt=table(customer_test_data$Churn,decisiontree_predictions)
confMatrix_dt
confusionMatrix(confMatrix_dt)
#accuracy-93.4
False_Negative_Rate_DT=(95/(95+129))#42.4

################Random Forest###################
#transform RFObject to intrees format
RF_Model_Train=randomForest(Churn ~.,data=customer_train_data,importance=TRUE,ntree=1500)
library("inTrees")
treelist=RF2List(RF_Model_Train)
#Extract Rules
exec=extractRules(treelist,customer_train_data[,-10])
#Make Rules Readable
Readable_Rules=presentRules(exec,colnames(customer_train_data))
Readable_Rules[1:2,]
#Get Rule Metric
ruleMetric = getRuleMetric(exec, customer_train_data[,-10], customer_train_data$Churn)
ruleMetric[1:2,]
RF_Predictions=predict(RF_Model_Train,customer_test_data[,-10])
confusion_Matrix_RF=table(customer_test_data$Churn,RF_Predictions)
confusion_Matrix_RF
confusionMatrix(confusion_Matrix_RF)
#Accuracy-94.24#94.1#94.3
False_Negative_Rate_RF=(94/(94+130))#41.6#42.8#41.5#41.9
