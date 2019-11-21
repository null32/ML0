library("MASS")

n <- 300
sigma1 <- matrix(c(1, 0, 0, 10), 2, 2)

mu1 <- c(11, 15)
mu2 <- c(15, 10)

xc1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xc2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma1)

plotxmin <- min(xc1[,1], xc2[,1]) - 1
plotymin <- min(xc1[,2], xc2[,2]) - 1
plotxmax <- max(xc1[,1], xc2[,1]) + 1
plotymax <- max(xc1[,2], xc2[,2]) + 1
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))

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

getFunc <- function(sigma, mu1, mu2) {
  d <- det(sigma)
  invs <- solve(sigma)
  
  b <- invs %*% t(mu1) - invs %*% t(mu2)
  
  D <- -2 * b[1, 1] # x
  E <- -2 * b[2, 1] # y
  G <- c(mu1 %*% invs %*% t(mu1) - mu2 %*% invs %*% t(mu2))
  
  func <- function(x, y) {
    x*D + y*E + G
  }
  
  return(func)
}

mu1 <- estimateMu(xc1)
mu2 <- estimateMu(xc2)
sigma1 <- estimateSigma(xc1, mu1, xc2, mu2)

func <- getFunc(sigma1, mu1, mu2)

x <- seq(plotxmin-5, plotxmax+5, len = 100)
y <- seq(plotymin-5, plotymax+5, len = 100)
z <- outer(x, y, func)
contour(x, y, z, levels = 0, add = TRUE, drawlabels = TRUE, lwd = 2.5)