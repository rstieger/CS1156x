makeTarget <- function() {
    p1 <- runif(2, min=-1, max=1)
    p2 <- runif(2, min=-1, max=1)
    b <- (p1[2]-p2[2])/(p1[1]-p2[1])
    a <- p1[2]-b*p1[1]
    function(x) { sign(c(a,b,-1) %*% rbind(1,matrix(x,nrow=2)))}
}

makeHypothesis <- function(w) {
    function(x) { sign(t(w) %*% rbind(1,matrix(x,nrow=2)))}
}

getWeights <- function(h) {
    environment(h)$w
}
makeInputs <- function(N) {
    matrix(data=runif(2 * N, min=-1, max=1), nrow=2, ncol=N)
}

checkHypothesis <- function(f, h, x) {
    apply(x,2,function(x) {f(x) == h(x)})
}
outOfSampleError <- function(f, g) {
    x <- makeInputs(1000)
    1-mean(checkHypothesis(f,g,x))
}

runLR <- function(x, f) {
    x0 <- rbind(1, matrix(x,nrow=2))
    xdagger <- solve(x0%*%t(x0))%*%x0
    makeHypothesis(xdagger%*%t(f(x)))
}

runPLA <- function(x, f, weights = c(0,0,0)) {
    w <- weights
    N <- ncol(x)
    loopCount <<- 1
    done <- FALSE
    while (!done) {
        h <- makeHypothesis(w)
        c <- checkHypothesis(f,h,x)
        if (sum(c) == N) {
            done <- TRUE
        } else {
            element <- ceiling(runif(1,max=N))
            while (c[element] == TRUE) {
                element <- element + 1
                if (element > N) {
                    element <- 1
                }
            }
            w <- w + f(x[,element])[1] * rbind(1,matrix(x[,element],nrow=2))
        }
        loopCount <<- loopCount + 1
    }
    makeHypothesis(w)
 #   c(iterations=loopCount, error=outOfSampleError(f, g))
}

doExperiment <- function(N) {
    f <- makeTarget()
    x <- makeInputs(N)
    y <- f(x)
    g0 <- runLR(x,f)
    g <- runPLA(x, f, weights=environment(g0)$w)
    
    c(Ein =mean(1-checkHypothesis(f, g, x)), Eout = outOfSampleError(f,g), iterations = loopCount)
}

print(rowMeans(replicate(1000, doExperiment(10))))