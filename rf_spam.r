library(kernlab)

data(spam)

spam.sample <- sample(1:nrow(spam),floor(0.4*nrow(spam)), replace=FALSE)
spam.train <- spam[spam.sample,]
spam.holdOut <- spam[-spam.sample,]

spam.labels <- as.factor(spam.train[,58])
spam.labelsHold <- as.factor(spam.holdOut[,58])
spam.pixels <- spam.train[,-58]
spam.pixelsHold <- spam.holdOut[,-58]

spam.rf <- randomForest(spam.pixels, spam.labels, xtest=spam.pixelsHold, ntree=500, nodesize=3)
spam.rfpredictions <- levels(spam.labels)[spam.rf$test$predicted]
spam.rfincorrect <- sum(spam.rfpredictions != spam.labelsHold)

spam.gb <- gbm.fit(y = spam.labels, x = spam.pixels, distribution = "multinomial", n.trees = 500, n.minobsinnode = 3)
spam.gbpredictions <- predict(spam.gb,spam.pixelsHold,n.trees=500, type="response")
spam.gbpred <- apply(spam.gbpredictions, 1, function(x) colnames(spam.gbpredictions)[which.max(x)])
spam.gbincorrect <- sum(spam.gbpred != spam.labelsHold)


spam.bag <- ipredbagg(y=spam.labels, X=spam.pixels, ns=100, nbagg=75)
spam.bagpredictions <- predict(spam.bag, newdata=spam.pixelsHold)
spam.bagincorrect <- sum(spam.bagpredictions != spam.labelsHold)


