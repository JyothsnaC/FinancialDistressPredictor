#Environmnet Setup 
View(loan)
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

#Read Data
loan <- read.csv("C:/Users/yo/Documents/BAPM/SEM1/PDM/Kaggle/cs-training.csv")

#Data cleaning
class(loan)
sapply(loan,class) #data  types of all variables
class(loan$SeriousDlqin2yrs)

#Modelling type of response variable "SeriousDlqin2yrs" to Nominal from   Continuous 
loan$SeriousDlqin2yrs=as.logical(loan$SeriousDlqin2yrs) 
length(loan$RevolvingUtilizationOfUnsecuredLines)
gru=loan$RevolvingUtilizationOfUnsecuredLines[loan$RevolvingUtilizationOfUnsecuredLines>1]
n=length(gru)

#2.	To fix the inconsistencies in variable Revolving Utilization Of Unsecured Lines, we excluded all rows where this variable's value is greater than 1.
loan$RevolvingUtilizationOfUnsecuredLines<-replace(loan$RevolvingUtilizationOfUnsecuredLines,gru,1)

#imputing monthly income with median value
loan$MonthlyIncome<-replace(loan$MonthlyIncome,is.na(loan$MonthlyIncome)==TRUE,5400)

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


#Modeling techniques
#1.Nominal logistic
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

#2.Bootstrap Forest
library(boot)
# function to obtain R-Squared from the data 
rsq <- function(SeriousDlqin2yrs~., NewLoan, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 
# bootstrapping with 1000 replications 
results <- boot(data=NewLoan, statistic=rsq, 
  	R=1000, formula=SeriousDlqin2yrs~.)

# view results
results 
plot(results)
# view results
results
plot(results,NewLoan[1]) # intercept 
plot(results,NewLoan[2]) # wt 
plot(results,NewLoan[3]) # disp 

#3. Decision Tree
# Regression Tree Example
library(rpart)
# grow tree 
fit <- rpart(SeriousDlqin2yrs~., 
   method="class", data=NewLoan)
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  	

# plot tree 
plot(fit, uniform=TRUE, 
  	main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
  	title = "Classification Tree")
# prune the tree 
pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
  	main="Classification Tree")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree.ps", 
  	title = "Classification Tree")

#4. Boosted Tree
require(gbm)
#separating training and test data
train=sample(1:506,size=374)
Boston.boost=gbm(SeriousDlqin2yrs~. ,data = NewLoan[train,],distribution = "gaussian",n.trees = 10000,
                  shrinkage = 0.01, interaction.depth = 4)
Boston.boost
summary(Boston.boost) #Summary gives a table of Variable Importance and
#Plot of Response variable with lstat variable
plot(Boston.boost,i="lstat") 
#Inverse relation with lstat variable
plot(Boston.boost,i="rm") 
#as the average number of rooms increases the the price increases
n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 
#Generating a Prediction matrix for each Tree
predmatrix<-predict(Boston.boost,Boston[-train,],n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix
#Calculating The Mean squared Test Error
test.error<-with(Boston[-train,],apply( (predmatrix-medv)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#5.Neural Net
set.seed(500)
library(MASS)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(SeriousDlqin2yrs~., data=Newloan)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
maxs <- apply(Newloan, 2, max) 
mins <- apply(Newloan, 2, min)
scaled <- as.data.frame(scale(Newloan, center = mins, scale = maxs - mins))
train_ <- scaled[3,]
test_ <- scaled[-3,]
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("SeriousDlqin2yrs~.", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
plot(nn)
pr.nn <- compute(nn,test_[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(paste(MSE.lm,MSE.nn))

      









