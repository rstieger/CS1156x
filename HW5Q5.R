e <- function(u,v) {
    (u*exp(v) - 2*v*exp(-u))^2
}

eta <- 0.1

de_du <- function(u,v) {
    2*(exp(v) + 2*v*exp(-u))*(u*exp(v)-2*v*exp(-u))
}

de_dv <- function(u,v) {
    2*(u*exp(v) - 2*exp(-u))*(u*exp(v)-2*v*exp(-u))
}

Q5run <- function() {
    u <- 1
    v <- 1
    for (n in 1:20) {
        du <- -eta * de_du(u,v)
        dv <- -eta * de_dv(u,v)
        u <- u + du
        v <- v + dv
        if (e(u,v) < 1e-14) {
            print(c(n=n,u=u,v=v))
            break
        }
    }
}

Q7run <- function() {
    u <- 1
    v <- 1
    for (n in 1:15) {
        du <- -eta * de_du(u,v)
        u <- u + du
        dv <- -eta * de_dv(u,v)
        v <- v + dv
    }
    print(c(E=e(u,v),u=u,v=v))

}