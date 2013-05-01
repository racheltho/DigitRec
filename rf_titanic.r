library(randomForest)
library(gbm)
library(ipred)
library(caret)
setwd("C:/Users/rthomas/Desktop/Kaggle/DigitRecognition")

allData <- read.csv("titanictrain.csv", header=TRUE)

sample <- sample(1:nrow(allData),floor(0.4*nrow(allData)), replace=FALSE)
train <- allData[sample,]
holdOut <- allData[-sample,]

labels <- as.factor(train[,1])
labelsHold <- as.factor(holdOut[,1])
x <- train[,-1]
xHold <- holdOut[,-1]

rf <- randomForest(x, labels, xtest=xHold, ntree=500, nodesize=3, na.action = na.roughfix)
rfpredictions <- levels(labels)[rf$test$predicted]

gb <- gbm.fit(y = labels, x = x, distribution = "multinomial", n.trees = 500, n.minobsinnode = 3)
best.iter <- gbm.perf(gb, method="OOB")
gbpredictions <- predict(gb,xHold, n.trees=best.iter, type="response")
gbpred <- apply(gbpredictions, 1, function(x) colnames(gbpredictions)[which.max(x)])
gbincorrect <- sum(gbpred != labelsHold)

bag <- ipredbagg(y=labels, X=x, ns=100, nbagg=75)
bagpredictions <- predict(bag, newdata=xHold)

rfincorrect <- sum(rfpredictions != labelsHold)
gbincorrect <- sum(gbpred != labelsHold)
bagincorrect <- sum(bagpredictions != labelsHold)