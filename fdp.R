loan <- read.csv("C:/Users/yo/Documents/BAPM/SEM1/PDM/Kaggle/cs-training.csv")
View(loan)

#Data cleaning
class(loan)
sapply(loan,class) #datat types of all variables
class(loan$SeriousDlqin2yrs)
#Modelling type of response variable "SeriousDlqin2yrs" to Nominal from   Continuous 
loan$SeriousDlqin2yrs=as.logical(loan$SeriousDlqin2yrs) 

length(loan$RevolvingUtilizationOfUnsecuredLines)
gru=loan$RevolvingUtilizationOfUnsecuredLines[loan$RevolvingUtilizationOfUnsecuredLines>1]
n=length(gru)
#2.	To fix the inconsistencies in variable Revolving Utilization Of Unsecured Lines, we excluded all rows where this variable's value is greater than 1.
loan$RevolvingUtilizationOfUnsecuredLines=replace(loan$RevolvingUtilizationOfUnsecuredLines,gru,1)
#imputing monthly income with median value

loan$MonthlyIncome=replace(loan$MonthlyIncome,is.na(loan$MonthlyIncome)==TRUE,5400)

#limiting outliers in debt ration to 295
loan$DebtRatio=replace(loan$DebtRatio,loan$DebtRatio>295,295)

cor(loan)


attach(loan)
#reducing dimensions

install.packages("caret")

library("caret")

trans = preProcess(loan[,c(5,9,11)], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
trans
PC = predict(trans, loan[,c(5,9,11)])
PC
View(PC)

#View(loan)
NewLoan=loan[-c(5,9,11)]
NewLoan$PC1<-PC$PC1
NewLoan$PC2<-PC$PC2
NewLoan
#NewLoan <-cbind(loan$X,loan$SeriousDlqin2yrs,loan$RevolvingUtilizationOfUnsecuredLines,loan$age,loan$DebtRatio,loan$MonthlyIncome
#           ,loan$NumberOfOpenCreditLinesAndLoans,loan$NumberRealEstateLoansOrLines,loan$NumberOfDependents,PC$PC1,PC$PC2)
#View(NewLoan)
#Modeling techniques
#Nominal logistic

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)


reg1=lm(SeriousDlqin2yrs~.,data=NewLoan)
tablr<-predict(reg1)
ftable(tablr,SeriousDlqin2yrs)
summary(reg1)
View(reg1)

reg1=multinom(SeriousDlqin2yrs~.,data=NewLoan,is.na=TRUE)
ftable(reg1)
logisticprediction=as.data.frame(predict(reg1))

ftable(SeriousDlqin2yrs,logisticprediction)
length(SeriousDlqin2yrs)
nrow(logisticprediction)
(length(which(is.na(SeriousDlqin2yrs)))) 
ftable(SeriousDlqin2yrs,logisticprediction)
ftable(logisticprediction)


