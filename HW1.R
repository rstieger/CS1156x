makeTarget <- function() {
    p1 <<- runif(2, min=-1, max=1)
    p2 <<- runif(2, min=-1, max=1)
    b <<- (p1[2]-p2[2])/(p1[1]-p2[1])
    a <<- p1[2]-b*p1[1]
    function(x) { sign(c(a,b,-1) %*% rbind(1,matrix(x,nrow=2)))}
}

makeHypothesis <- function(w) {
    function(x) { sign(t(w) %*% rbind(1,matrix(x,nrow=2)))}
}

makeInputs <- function(N) {
    matrix(data=runif(2 * N, min=-1, max=1), nrow=2, ncol = N, byrow=TRUE)
}

N <- 10
f <- makeTarget()
x <- makeInputs(N)
y <- f(x)
plot(x=x[1,],y=x[2,],col=y+3,xlim=c(-1,1),ylim=c(-1,1))
abline(a=a,b=b)

checkHypothesis <- function(f, h, x) {
    apply(x,2,function(x) {f(x) == h(x)})
}
runPLA <- function(x, f) {
    w <- c(0,0,0)
    done <- FALSE
    while (!done) {
        h <- makeHypothesis(w)
        c <- checkHypothesis(f,h,x)
        print(c)
        if (sum(c) == length(c)) {
            done <- TRUE
        } else {
            element <- ceiling(runif(1,max=ncol(x)))
            print(element)
            while (c[element] == TRUE) {
                element <- (element + 1) %% length(x)
            }
            w <- w + f(x[element]) * x[element]
        }
    }
    w
}


