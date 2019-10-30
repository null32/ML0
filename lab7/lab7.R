library("MASS")

n <- 100
sigma1 <- matrix(c(1,0,0,1), 2, 2)
sigma2 <- matrix(c(0.5, 0, 0, 0.5), 2, 2)

mu1 <- c(0,0)
mu2 <- c(2, 4)

xc1 <- mvrnorm(n=100, mu = mu1, Sigma = sigma1)
xc2 <- mvrnorm(n=100, mu = mu2, Sigma = sigma2)

plotxmin <- min(xc1[,1], xc2[,1]) - 1
plotymin <- min(xc1[,2], xc2[,2]) - 1
plotxmax <- max(xc1[,1], xc2[,1]) + 1
plotymax <- max(xc1[,2], xc2[,2]) + 1
plot(c(), type="n", xlab = "признак 1", ylab = "признак 2", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))

colors <- c("magenta", "cyan")
points(xc1, pch=21, col=colors[1], bg=colors[1])
points(xc2, pch=21, col=colors[2], bg=colors[2])

estimateMu <- function(xs) {
  l <- dim(xs)[2]
  res <- array(NA, l)
  for (i in seq(l)) {
    res[i] <- mean(xs[,i])
  }
  return(res)
}

estimateSigma <- function(xs, mu) {
  rows <- dims(xs)[1]
  cols <- dims(xs)[2]
  
  res <- matrix(0, cols, cols)
  for (i in seq(rows)) {
    res <- res + (xs[1,] - mu) %*% t(xs[1,] - mu)
  }
  
  return(res/(rows - 1))
}

