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

makeInputs <- function(N) {
    matrix(data=runif(2 * N, min=-1, max=1), nrow=2, ncol = N, byrow=TRUE)
}

checkHypothesis <- function(f, h, x) {
    apply(x,2,function(x) {f(x) == h(x)})
}
outOfSampleError <- function(f, g) {
    x <- makeInputs(100)
    1-mean(checkHypothesis(f,g,x))
}
runPLA <- function(x, f) {
    w <- c(0,0,0)
    N <- ncol(x)
    loopCount <- 1
    done <- FALSE
    while (!done && loopCount < 100) {
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
         loopCount = loopCount + 1
    }
    data.frame(iterations=loopCount, error=outOfSampleError(f, makeHypothesis(w)))
}

doExperiment <- function(N) {
    f <- makeTarget()
    x <- makeInputs(N)
    y <- f(x)
#    plot(x=x[1,],y=x[2,],col=y+3,xlim=c(-1,1),ylim=c(-1,1))
#    abline(a=a,b=b)
    runPLA(x,f)
}