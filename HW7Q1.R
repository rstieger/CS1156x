indata <- read.table("in.dta", col.names=c("x1","x2","y"))
validation <- indata[1:25,]
training <- indata[26:35,]
testdata <- read.table("out.dta", col.names=c("x1","x2","y"))
phi <- function(x1, x2, k=7) {
    as.matrix(cbind(1, x1, x2, x1^2, x2^2, x1*x2, abs(x1-x2), abs(x1+x2)))[,1:(k+1)]
}
lin <- function(z, y) {
    solve(t(z) %*% z) %*% t(z) %*% y
}
doTest <- function(k=7) {
    z <- phi(training[["x1"]], training[["x2"]], k)
    y <- training[["y"]]
    zdagger <- solve(t(z) %*% z) %*% t(z)
    w_lin <- zdagger %*% y
    error <- function(z, w, y) {
        predict <- sign(z %*% w)
        sum(predict != y)/length(y)
    }
    Ein <- error(z, w_lin, y)
    Eval <- error(phi(validation[["x1"]], validation[["x2"]], k), w_lin, validation[["y"]])
    Eout <- error(phi(testdata[["x1"]], testdata[["x2"]], k), w_lin, testdata[["y"]])
    print(c(k=k, Ein=Ein, Eval=Eval, Eout=Eout))
}
lapply(3:7, doTest)

