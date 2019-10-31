library("MASS")

n <- 100
sigma1 <- matrix(c(1,0,0,1), 2, 2)
sigma2 <- matrix(c(2, 0, 0, 0.4), 2, 2)

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
  res <- matrix(NA, 1, l)
  for (i in seq(l)) {
    res[1, i] <- mean(xs[,i])
  }
  return(res)
}

estimateSigma <- function(xs, mu) {
  rows <- dim(xs)[1]
  cols <- dim(xs)[2]
  
  res <- matrix(0, cols, cols)
  for (i in seq(rows)) {
    res <- res + t(xs[1,] - mu) %*% (xs[1,] - mu)
  }
  
  return(res/(rows - 1))
}

getFunc <- function(sigma1, mu1, sigma2, mu2) {
  d1 <- det(sigma1)
  d2 <- det(sigma2)
  invs1 <- solve(sigma1)
  invs2 <- solve(sigma2)
  
  a <- sigma1 - sigma2
  b <- sigma1 %*% t(mu1) - sigma2 %*% t(mu2)
  
  A <- a[1,1]
  B <- a[2,2]
  C <- a[1, 2] + a[2, 1]
  D <- c*m2 + b*m2 - 2*d*m1 - f*m4 - g*m4 + 2*h*m3
  E <- c*m1 + b*m1 - 2*a*m2 - f*m3 - g*m3 + 2*e*m4
  G <- mu1 %*% solve(sigma1) %*% t(mu1) - mu2 %*% solve(sigma2) %*% t(mu2)
  G1 <- d*m1^2 - c*m1*m2 - b*m1*m2 + a*m2^2 - h*m3^2 + g*m3*m4 + f*m3*m4 - e*m4^2
  
  func <- function(x, y) {
    x^2 * A + y^2 * B + x*y*C + x*D + y*E + G
  }
  
  return(func)
}

mu1 <- estimateMu(xc1)
mu2 <- estimateMu(xc2)
sigma1 <- estimateSigma(xc1, mu1)
sigma2 <- estimateSigma(xc2, mu2)

print(mu1)
print(sigma1)
print(mu2)
print(sigma2)

func <- getFunc(sigma1, mu1, sigma2, mu2)

x <- seq(-10, 10)
y <- seq(-10, 10)
z <- outer(x, y, func)
contour(x, y, z, levels = 0, lwd = 1, add = TRUE)