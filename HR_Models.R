library(corrplot)
library(caret)
library(SDMTools)
library(R.oo)
library(randomForest)
library(e1071)
library(miscTools)
library(Metrics)
library(class)

setwd("E:/RPI/Spring2017/Data Analytics/Project/human-resources-analytics")

HRData <- read.csv("HRData.csv", header = TRUE, sep = ",")
str(HRData)
summary(HRData)

### CORRELATION MATRIX ###
corr.mat <- cor(HRData[,1:5])
corrplot(corr.mat)

HRData$sales <- as.numeric(HRData$sales)
HRData$salary <- as.numeric(HRData$salary)
HRData$left <- as.factor(HRData$left)

### DATA PARTITIONING ###
set.seed(1234)
data_index <- createDataPartition(HRData$left, p=0.7, list = FALSE)
train.data <- HRData[data_index, ]
test.data <- HRData[-data_index, ]

############################ "LEFT" AS DEPENDENT VARIABLE#######################################
##### LOGISTIC REGRESSION ######
hrlr <- glm(left~., data = train.data, family = "binomial")
summary(hrlr)
summary(hrlr$coefficients)
lr_pred1 <- predict(hrlr,test.data)
accuracy(test.data$left, lr_pred1, threshold = 0.6)
plot(hrlr)

####### STEP ##########
hrlr.step <- step(hrlr)
summary(hrlr.step)

lr_pred <- predict(hrlr.step,test.data)
accuracy(test.data$left, lr_pred, threshold = 0.6)

###### RANDOM FOREST #######
hrrf=randomForest(left~., data=train.data, ntree=500, importance=TRUE, type="classification")
varImpPlot(hrrf,sort=TRUE, n.var=min(30, nrow(hrrf$importance)))
plot(hrrf)

rf_pred <- predict(hrrf,test.data)
accuracy(test.data$left, rf_pred)
table(rf_pred, test.data$left)

###### NAIVE BAYES ########
hrnb <- naiveBayes(left~.,data = train.data)
hrnb
summary(hrnb)

nb_pred <- predict(hrnb,test.data)
table(nb_pred, test.data$left)

###### SUPPORT VECTOR MACHINES #######
hrsvm <- svm(left~., data = train.data)
summary(hrsvm)

svm_pred <- predict(hrsvm,test.data)
print(predict(hrsvm, test.data))
table(svm_pred, test.data$left)

###### K NEAREST NEIGHBOR ########
labels <- HRData$left 
HRData$left <- NULL

set.seed(1294)
train.pct <- 0.7
N <- nrow(HRData)
train.index <- sample(1:N, train.pct * N)
train.data1 <- HRData[train.index, ]
test.data1 <- HRData[-train.index, ]
train.labels <- as.factor(as.matrix(labels)[train.index, ])
test.labels <- as.factor(as.matrix(labels)[-train.index, ])
k<-10

knn.fit <- knn(train = train.data1, # training set
               test = test.data1, cl = train.labels, k = k
               # test set
               # true labels
               # number of NN to poll
)
summary(knn.fit)
print(table(test.labels, knn.fit))

############ SATISFACTION LEVEL AS DEPENDENT VARIABLE ###################
##### LINEAR REGRESSION ###########

sl.lr <- lm(satisfaction_level~.,data = train.data)
summary(sl.lr)
sl.step <- step(sl.lr)
summary(sl.step)

sllr_pred <- predict(sl.step,test.data)
accuracy(test.data$left, sllr_pred, threshold = 0.6)

sllr.mse <- mean((test.data$satisfaction_level - sllr_pred)^2)
sqrt(mean(sl.step$residuals^2)) ###rmse

sllr.rmse <- rmse(test.data$satisfaction_level, sllr_pred)
lr.r2 <- rSquared(test.data$satisfaction_level, test.data$satisfaction_level - sllr_pred)
sllrp <- ggplot(aes(x=actual, y=sllr_pred),
                data=data.frame(actual=test.data$satisfaction_level, sllr_pred))
sllrp.p <- sllrp + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("Linear Regression in R r^2=", lr.r2, sep=""))

########## RANDOM FOREST FOR SATISFACTION LEVEL ##################
slrf=randomForest(satisfaction_level~., data=train.data, ntree=500, importance=TRUE, type="regression")
varImpPlot(slrf,sort=TRUE, n.var=min(30, nrow(slrf$importance)))
plot(slrf)
summary(slrf)

slrf_pred <- predict(slrf,test.data)
rf.r2 <- rSquared(test.data$satisfaction_level, test.data$satisfaction_level - slrf_pred)
mse <- mean((test.data$satisfaction_level - slrf_pred)^2)
slrf.rmse <- rmse(test.data$satisfaction_level, slrf_pred)

slrfp <- ggplot(aes(x=actual, y=slrf_pred),
              data=data.frame(actual=test.data$satisfaction_level, slrf_pred))
slrfp.p <- slrfp + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", rf.r2, sep=""))

########### SVM FOR SATISFACTION LEVEL #################

sl.svm <- svm(satisfaction_level~., train.data)
summary(sl.svm)
sl.svm_pred <- predict(sl.svm, test.data)

points(test.data$satisfaction_level, sl.svm_pred, col = "red", pch=4)
svm.error <- test.data$satisfaction_level - sl.svm_pred

svrPredictionRMSE <- rmse(test.data$satisfaction_level,sl.svm_pred)
svm.r2 <- rSquared(test.data$satisfaction_level, test.data$satisfaction_level - sl.svm_pred)
slsvmp <- ggplot(aes(x=actual, y=sl.svm_pred),
                data=data.frame(actual=test.data$satisfaction_level, sl.svm_pred))
slsvmp.p <- slsvmp + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("SVM Regression in R r^2=", svm.r2, sep=""))

