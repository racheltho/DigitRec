# makes the random forest submission

library(randomForest)
library(gbm)
library(ipred)
library(caret)
library("ggplot2")

setwd("C:/Users/rthomas/Desktop/Kaggle/DigitRecognition")

allData <- read.csv("train.csv", header=TRUE)

sample <- sample(1:nrow(allData),floor(0.1*nrow(allData)), replace=FALSE)
train <- allData[sample,]
holdOut <- allData[-sample,]

labels <- as.factor(train[,1])
labelsHold <- as.factor(holdOut[,1])
pixels <- train[,-1]
pixelsHold <- holdOut[,-1]

rfincorrect <- array(0, 5)
gbincorrect <- array(0, 5)
bagincorrect <- array(0, 5)

total <- length(labelsHold)

rf <- randomForest(pixels, labels, xtest=pixelsHold, ntree=250, nodesize=3, importance=TRUE)
rfpredictions <- levels(labels)[rf$test$predicted]

gb <- gbm.fit(y = labels, x = pixels, distribution = "multinomial", n.trees = 1000, n.minobsinnode = 3)
best.iter <- gbm.perf(gb, method="OOB")
gbpredictions <- predict(gb,pixelsHold, n.trees=best.iter, type="response")
gbpred <- apply(gbpredictions, 1, function(x) colnames(gbpredictions)[which.max(x)])
gbincorrect <- sum(gbpred != labelsHold)

bag <- ipredbagg(y=labels, X=pixels, ns=100, nbagg=75)
bagpredictions <- predict(bag, newdata=pixelsHold)

rfincorrect[i] <- sum(rfpredictions != labelsHold)/total
bagincorrect[i] <- sum(bagpredictions != labelsHold)/total



#write(predictions, file="rf_benchmark.csv", ncolumns=1) 


rf_function <- function(x){
  rf <- randomForest(pixels, labels, xtest=pixelsHold, ntree=x*250, nodesize=3)
  rfpredictions <- levels(labels)[rf$test$predicted]
  return(sum(rfpredictions != labelsHold)/total)}

for(i in 1:5){ rfincorrect[i] <- rf_function(i)}

gb_function <- function(x){
  gb <- gbm.fit(y = labels, x = pixels, distribution = "multinomial", n.trees = x*250, n.minobsinnode = 5)
  best.iter <- gbm.perf(gb, method="OOB")
  gbpredictions <- predict(gb,pixelsHold, n.trees=best.iter, type="response")
  gbpred <- apply(gbpredictions, 1, function(x) colnames(gbpredictions)[which.max(x)])
  return(sum(gbpred != labelsHold)/total)}

bag_function <- function(x){
  bag <- ipredbagg(y=labels, X=pixels, ns=x*50, nbagg=x*250)
  bagpredictions <- predict(bag, newdata=pixelsHold)  
  return(sum(bagpredictions != labelsHold)/total)}
  


# Given a row of pixels (representing a square image of a digit) and the label of the digit, display
display.digit <- function(row_pixels, title = NULL)
{
  l <- length(row_pixels)
  n <- sqrt(l);
  if (l %% n != 0)
    stop("Length of input should be a perfect square.")  
  mat <- matrix(as.double(row_pixels), nrow=n, ncol=n)
  mat <- apply(mat,1,rev)
  p<-ggfluctuation(as.table(mat), type = "colour") +
    labs(title=title) +
    scale_fill_gradient(low = "white", high = "blue")
  print(p)
  Sys.sleep(0.5)
}


rf_error <- c(rf_error, NA, NA, NA)
plotData <- data.frame(trees, gbm_error, rf_error)
reData <- melt(plotData, id='trees')
qplot(log(trees), value, data=reData, group=variable, color=variable, ylab="Error", xlab="Log(# Trees)", geom="point", size=50)



