library(quadprog)

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
    matrix(data=runif(2 * N, min=-1, max=1), nrow=2, ncol=N)
}

checkHypothesis <- function(f, h, x) {
    apply(x,2,function(x) {f(x) == h(x)})
}

outOfSampleError <- function(f, g) {
    x <- makeInputs(1000)
    1-mean(checkHypothesis(f,g,x))
}

runPLA <- function(x, f, weights = c(0,0,0)) {
    w <- weights
    N <- ncol(x)
    loopCount <<- 1
    repeat {
        h <- makeHypothesis(w)
        c <- checkHypothesis(f,h,x)
        if (sum(c) == N) {
            break
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

runSVM <- function(x, y) {
    qp <- LowRankQP(Vmat=(t(y) %*% y) * (t(x) %*% x),
            dvec=rep(-1, times=length(y)), Amat=y, 
            bvec=0, uvec=rep(1000, times=length(y)), method="CHOL")
#     qp$alpha <- sapply(qp$alpha, function(x) {if (x < 1e-10) {0} else {x}})
    w <- x %*% (qp$alpha * t(y))
    b <- qp$beta
    list(g=makeHypothesis(matrix(c(b,w))), sv=sum(qp$alpha>1))
}

doExperiment <- function(N) {
    f <- makeTarget()
    x <- makeInputs(N)
    y <- f(x)
    gpla <- runPLA(x, f)
    gsvm <- runSVM(x, y)
    c(Epla = outOfSampleError(f,gpla), Esvm = outOfSampleError(f,gsvm$g), sv=gsvm$sv)
}

#  results <- replicate(1000, doExperiment(10))
   results <- replicate(100, doExperiment(100))
print(sum(results["Esvm",]<results["Epla",], na.rm=TRUE)/ncol(results))
print(mean(results["sv",]))