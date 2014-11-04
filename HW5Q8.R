makeTarget <- function() {
    p1 <- runif(2, min=-1, max=1)
    p2 <- runif(2, min=-1, max=1)
    b <- (p1[2]-p2[2])/(p1[1]-p2[1])
    a <- p1[2]-b*p1[1]
    function(x) { sign(c(a,b,-1) %*% x)}
}

makeHypothesis <- function(w) {
    function(x) { sign(t(w) %*% x)}
}

makeInputs <- function(N) {
    rbind(1,matrix(data=runif(2 * N, min=-1, max=1), nrow=2, ncol = N, byrow=TRUE))
}

theta <- function(s) {
    exp(s)/(1+exp(s))
}

e <- function(x,y,w) {
    log(1+exp(-(t(w) %*% x)*y))
}

de <- function(x,y,w) {
    -y*x/as.numeric((1+exp(y*(t(w) %*% x))))
}

E <- function(x,y,w) {
    apply(e(x,y,w),1,sum)/ncol(x)
}

runSGD <- function(x,f) {
    y <- f(x)
    w <- c(0,0,0)
    eta <- 0.01
    for (t in 1:2000) {
        wold <- w
        for (n in sample(1:ncol(x))) {
            w <- w - eta * de(x[,n],y[n],w)
        }
        if (sqrt(sum((w-wold)^2)) < 0.01)
            break
    }
    xtest <- makeInputs(1000)
    ytest <- f(xtest)
    c(t=t, Ein=E(x,y,w), Eout=E(xtest,ytest,w))
}

doExperiment <- function(N) {
    f <- makeTarget()
    x <- makeInputs(N)
    runSGD(x,f)
}

Q8run <- function() {
    N <- 100
    numreps <- 100
    print(rowMeans(replicate(numreps,doExperiment(N))))
}