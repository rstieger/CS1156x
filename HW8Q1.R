library(e1071)
getdata <- function(fname) {
    t <- read.table(fname, col.names=c("digit", "symmetry", "intensity"))
    t$digit <- as.factor(t$digit)
    t
}
train <- getdata("features.train")
test <- getdata("features.test")

getmodel <- function(n) {
    svm(train[2:3], as.factor(train$digit == n), kernel="polynomial", degree=2, gamma=1, coef0=1, cost=0.01, scale=FALSE)
}
Ein <- sapply(0:9, function(n) {
    classifier <- as.factor(train$digit == n)
    model <- getmodel(n)
    pred <- predict(model, train[2:3])
    sum(classifier != pred)/length(pred)
    })

m0 <- getmodel(0)
m1 <- getmodel(1)
m0$tot.nSV - m1$tot.nSV

train1.5 <- train[train$digit %in% c(1,5),]
test1.5 <- test[test$digit %in% c(1,5),]
lapply(c(2,5), function(Q) {
sapply(c(0.0001, 0.001, 0.01, 0.1, 1), function(cost) {
    m <- svm(train1.5[2:3], as.factor(train1.5$digit == 1), kernel="polynomial", degree=Q, gamma=1, coef0=1, cost=cost, scale=FALSE)
    pred <- predict(m, train1.5[,2:3])
    Ein <- sum((train1.5$digit == 1) != pred)/length(pred)
    pred <- predict(m, test1.5[,2:3])
    Eout <- sum((test1.5$digit == 1) != pred)/length(pred)
    c(Q=Q, cost=cost, nSV=m$tot.nSV, Ein=Ein, Eout=Eout)
    })
})


table(replicate(100,  {
    d <- sapply(c(0.0001, 0.001, 0.01, 0.1, 1), function(cost) {
        m <- svm(train1.5[2:3], as.factor(train1.5$digit == 1), kernel="polynomial", degree=2, gamma=1, coef0=1, cost=cost, scale=FALSE, cross=10)
        c(cost=cost, Ecv=1-m$tot.accuracy/100)
    })
    d["cost",which.min(d["Ecv",])]
}))


mean(replicate(100,  {
    d <- sapply(c(0.0001, 0.001, 0.01, 0.1, 1), function(cost) {
        m <- svm(train1.5[2:3], as.factor(train1.5$digit == 1), kernel="polynomial", degree=2, gamma=1, coef0=1, cost=cost, scale=FALSE, cross=10)
        c(cost=cost, Ecv=1-m$tot.accuracy/100)
    })
    (d["Ecv",2])
}))

sapply(c(0.01, 1, 100, 1e4, 1e6), function(cost) {
    m <- svm(train1.5[2:3], as.factor(train1.5$digit == 1), kernel="radial",
             gamma=1, cost=cost, scale=FALSE)
    pred <- predict(m, train1.5[,2:3])
    Ein <- sum((train1.5$digit == 1) != pred)/length(pred)
    pred <- predict(m, test1.5[,2:3])
    Eout <- sum((test1.5$digit == 1) != pred)/length(pred)
    c(cost=cost, nSV=m$tot.nSV, Ein=Ein, Eout=Eout)
})