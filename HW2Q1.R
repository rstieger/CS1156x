runExperiment <- function() {
    numCoins <- 1000
    numFlips <- 10
    coinflips <- matrix(rbinom(numCoins * numFlips, 1, 0.5), nrow=numCoins)
    coinfreqs <- rowMeans(coinflips)
    c(v1 = coinfreqs[1],
        vrand = coinfreqs[ceiling(runif(1, max=numCoins))],
        vmin = min(coinfreqs)
    )
}
numReps <- 100000
rowMeans(replicate(numReps, runExperiment()))
