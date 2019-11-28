library("MASS")

n <- 300
sigma1 <- matrix(c(1, 0, 0, 10), 2, 2)

mu1 <- c(0, 10)
mu2 <- c(10, 0)

xc1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xc2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma1)

plotxmin <- min(xc1[,1], xc2[,1]) - 1
plotymin <- min(xc1[,2], xc2[,2]) - 1
plotxmax <- max(xc1[,1], xc2[,1]) + 1
plotymax <- max(xc1[,2], xc2[,2]) + 1
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="Линейный дискриминат Фишера")

colors <- c("magenta", "cyan")
points(xc1, pch=21, col=colors[1], bg=colors[1])
points(xc2, pch=21, col=colors[2], bg=colors[2])

estimateMu <- function(xs) {
  l <- dim(xs)[2]
  res <- matrix(NA, 1, l)
  for (i in seq(l)) {
    res[1, i] <- mean(xs[,i])
  }
  return(res)
}

estimateSigma <- function(xs1, mu1, xs2, mu2) {
  rows1 <- dim(xs1)[1]
  cols <- dim(xs1)[2]
  rows2 <- dim(xs2)[1]
  
  res <- matrix(0, cols, cols)
  for (i in seq(rows1)) {
    res <- res + t(xs1[i,] - mu1) %*% (xs1[i,] - mu1)
  }
  for (i in seq(rows2)) {
    res <- res + t(xs2[i,] - mu2) %*% (xs2[i,] - mu2)
  }
  
  return(res/(rows1 + rows2 + 2))
}

getFunc <- function(sigma1, mu1, mu2) {
  d1 <- det(sigma1)
  invs1 <- solve(sigma1)
  
  b <- invs1 %*% t(mu1 - mu2)
  
  D <- b[1, 1] # x
  E <- b[2, 1] # y
  mu <- (mu1 + mu2)
  G <- c(mu %*% b) / 2
  
  func <- function(x) {
    -x*D/E + G/E
  }
  
  return(func)
}

mu1 <- estimateMu(xc1)
mu2 <- estimateMu(xc2)
sigma1 <- estimateSigma(xc1, mu1, xc2, mu2)

func <- getFunc(sigma1, mu1, mu2)

x <- seq(plotxmin-5, plotxmax+5, len = 100)
lines(x, func(x), lwd = 2.5, type="l")
lines(c(mu1[1], mu2[1]), c(mu1[2], mu2[2]), col = 'gray', lwd = 2)
