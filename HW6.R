training <- read.table("in.dta", col.names=c("x1","x2","y"))
testdata <- read.table("out.dta", col.names=c("x1","x2","y"))
phi <- function(x1, x2) {
    as.matrix(cbind(1, x1, x2, x1^2, x2^2, x1*x2, abs(x1-x2), abs(x1+x2)))
}
lin <- function(z, y) {
    solve(t(z) %*% z) %*% t(z) %*% y
}
z <- phi(training[["x1"]], training[["x2"]])
y <- training[["y"]]
zdagger <- solve(t(z) %*% z) %*% t(z)
w_lin <- zdagger %*% y
error <- function(z, w, y) {
    predict <- sign(z %*% w)
    sum(predict != y)/length(y)
}
Ein <- error(z, w_lin, y)
Eout <- error(phi(testdata[["x1"]], testdata[["x2"]]), w_lin, testdata[["y"]])
print(c(Ein=Ein, Eout=Eout))

LRweight.decay <- function(k) 
{
    lambda <- 10^k
    w_reg <- solve(t(z) %*% z + diag(x=lambda, nrow=8)) %*% t(z) %*% y
    Ein <- error(z, w_reg, y)
    Eout <- error(phi(testdata[["x1"]], testdata[["x2"]]), w_reg, testdata[["y"]])
    print(c(k=k,Ein=Ein, Eout=Eout))
}
LRweight.decay(-3)
LRweight.decay(3)
LRweight.decay(-2)

LRweight.decay(-1)

LRweight.decay(0)

LRweight.decay(1)

LRweight.decay(2)

