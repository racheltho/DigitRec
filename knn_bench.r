# makes the KNN submission

library(FNN)
source("digit.r")

trainRaw <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)

sample <- sample(1:nrow(trainRaw),floor(0.5*nrow(trainRaw)), replace=FALSE)
n <- floor(length(sample)/2)
train_idx <- sample[1:n]
cv_idx <-sample[(n+1):length(sample)]

train_wl <- trainRaw[train_idx,]

cv_wl <- trainRaw[cv_idx,]
#holdOutRaw10 <- trainRaw[-sample10,]

#labels <- as.factor(trainRaw[,1])
labels <- as.factor(train_wl[,1])
labelsCV <- as.factor(cv_wl[,1])
#labelsHold10 <- as.factor(holdOutRaw10[,1])
#train <- trainRaw[,-1]
train <- train_wl[,-1]
#train10 <- trainRaw10[,-1]
dataCV <- cv_wl[,-1]
#holdOut10 <- holdOutRaw10[,-1]

results <- knn(train, dataCV, labels, k = 10, prob = TRUE, algorithm="cover_tree")
prob <- attr(results,"prob")

hist(prob)

lowProb <- which(prob <= .7)

dataCV_low <- dataCV[lowProb,]
labelsCV_low <- labelsCV[lowProb]
prob_low <- prob[lowProb]

dataList <- as.list(data.frame(t(dataCV_low)))
low_len <- length(lowProb)
thetaList <- c(-pi/4,-pi/8,pi/8,pi/4)
indices <- 1:low_len
l <- ncol(train)

listRotate1 <- lapply(dataList, function(x) {rotate.image(row_pixels=x, theta=-pi/4)})
listRotate2 <- lapply(dataList, function(x) {rotate.image(row_pixels=x, theta=-pi/8)})
listRotate3 <- lapply(dataList, function(x) {rotate.image(row_pixels=x, theta=pi/8)})
listRotate4 <- lapply(dataList, function(x) {rotate.image(row_pixels=x, theta=pi/4)})

listRotate <- outer(indices,thetaList,Vectorize(function(x,y){
  temp_pixels <- rotate.image(row_pixels=dataList[[x]],theta=y)
  list_pixels <- as.list(data.frame(temp_pixels))
  return(list_pixels)}))

dataRotate <- matrix(unlist(listRotate),nrow=low_len*length(thetaList),ncol=l,byrow=TRUE)

dataRotate1 <- t(matrix(unlist(listRotate1),nrow=784,ncol=1271))
dataRotate2 <- t(matrix(unlist(listRotate2),nrow=784,ncol=1271))
dataRotate3 <- t(matrix(unlist(listRotate3),nrow=784,ncol=1271))
dataRotate4 <- t(matrix(unlist(listRotate4),nrow=784,ncol=1271))

resultsR1 <- knn(train, dataRotate1, labels, k = 10, prob = TRUE, algorithm="cover_tree")
probR1 <- attr(resultsR1,"prob")
resultsR2 <- knn(train, dataRotate2, labels, k = 10, prob = TRUE, algorithm="cover_tree")
probR2 <- attr(resultsR2,"prob")
resultsR3 <- knn(train, dataRotate3, labels, k = 10, prob = TRUE, algorithm="cover_tree")
probR3 <- attr(resultsR3,"prob")
resultsR4 <- knn(train, dataRotate4, labels, k = 10, prob = TRUE, algorithm="cover_tree")
probR4 <- attr(resultsR4,"prob")

probs <- matrix(c(prob_low,probR1,probR2,probR3,probR4),ncol=low_len,nrow=5,byrow=TRUE)

max <- apply(probs,2,max)
whichmax <- apply(probs,2,which.max)
lablesRot <- matrix(c(results[lowProb],resultsR1,resultsR2,resultsR3,resultsR4),ncol=low_len,nrow=5,byrow=TRUE)
guess <- lapply(indices, function(x){ lablesRot[whichmax[x],x]})

#testing <- outer(indices,thetaList,Vectorize(function(x,y){rotate.image(row_pixels=dataList[[x]],theta=y)}))

results <- knn(train, dataCV_low, labels, k = 10, prob = TRUE, algorithm="cover_tree")
prob <- attr(results,"prob")


wrong <- which(results != labelsCV)
right <- which(results == labelsCV)

mean(prob[wrong])
mean(prob[right])

hist(prob[wrong])
hist(prob[right])

accuracy <- 1 - length(wrong)/length(labelsCV)

dataCV_wrong <- dataCV[wrong,]
dataCV_right <- dataCV[right,]
labelsCV_wrong <- labelsCV[wrong]
labelsCV_right <- labelsCV[right]
pred_wrong <- results[wrong]
prob_wrong <- prob[wrong]

avgDigit <- lapply(0:9,function(x) colMeans(dataCV_right[which(labelsCV_right==x),]))
avgWrong <- lapply(0:9,function(x) colMeans(dataCV_wrong[which(labelsCV_wrong==x),]))

for(i in 0:9){
  display.digit(avgDigit[[i+1]],i)
  display.digit(avgWrong[[i+1]],i)
}

         
for(j in 1:50){
  smooth.digit(dataCV_wrong[j,], labelsCV_wrong[j], pred = pred_wrong[j], prob = prob_wrong[j])
}

  
write(newResults, file="6nn_benchmark.csv", ncolumns=1) 




# Testing multiple k values
# kk = c(2,4,6)
# 
# results <- matrix(ncol=nrow(holdOut10), nrow=length(kk))
# 
# for(i in 1:length(kk)){
#   results[i,] <- (0:9)[knn(train10, holdOut10, labels10, k = kk[i], algorithm="cover_tree")]
# }
# 
# accuracy <- matrix(ncol=1, nrow=length(kk))  
# for(i in 1:length(kk)){ 
#   accuracy[i] <- sum(ifelse(results[i,] == labelsHold10, 1, 0))/length(labelsHold10)
# }

#Looking at my accuracy vector, max accuracy with k=6

newResults <- (0:9)[knn(train, test, labels, k = 6, algorithm="cover_tree")]

write(newResults, file="6nn_benchmark.csv", ncolumns=1) 
