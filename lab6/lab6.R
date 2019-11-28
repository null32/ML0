library("MASS")

n <- 300
sigma1 <- matrix(c(1, 0, 0, 2), 2, 2)
sigma2 <- matrix(c(1, 0, 0, 1), 2, 2)

mu1 <- c(5, 5)
mu2 <- c(10, 5)

xc1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xc2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma2)

plotxmin <- min(xc1[,1], xc2[,1]) - 1
plotymin <- min(xc1[,2], xc2[,2]) - 1
plotxmax <- max(xc1[,1], xc2[,1]) + 1
plotymax <- max(xc1[,2], xc2[,2]) + 1
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="Наивный нормальный байесовский классификатор")

colors <- c("red", "green")
points(xc1, pch=21, col=colors[1], bg=colors[1])
points(xc2, pch=21, col=colors[2], bg=colors[2])

getM <- function(xs) {
  l <- dim(xs)[2]
  res <- matrix(NA, 1, l)
  for (i in seq(l)) {
    res[1, i] <- mean(xs[,i])
  }
  return(c(res))
}

getD <- function(xs, mu) {
  rows <- dim(xs)[1]
  cols <- dim(xs)[2]
  
  res <- matrix(0, 1, cols)
  for (i in seq(rows)) {
    res <- res + (xs[i,] - mu)^2
  }
  
  return(c(res/(rows-1)))
}

getPyj <- function(x, M, D){
  return( (1/(D*sqrt(2*pi))) * exp(-1 * ((x - M)^2)/(2*D^2)) )
}

naiveBayes <- function(x, M, D, Prob, Prior) {
  res <- log(Prob * Prior)
  l <- length(x)
  
  for (i in seq(l)) {
    p <- getPyj(x[i], M[i], D[i])
    res <- res + log(p)
  }
  
  return(res)
}

m1 <- getM(xc1)
m2 <- getM(xc2)
d1 <- getD(xc1, m1)
d2 <- getD(xc2, m2)

l <- max(plotxmax - plotxmin, plotymax - plotymin)
x <- seq(plotxmin, plotxmax, l/50)
y <- seq(plotymin, plotymax, l/50)

for (i in x) {
  for (j in y) {
    res1 <- naiveBayes(c(i, j), m1, d1, 0.5, 1)
    res2 <- naiveBayes(c(i, j), m2, d2, 0.5, 1)
    color <- ifelse(res1 > res2, colors[1], colors[2])
    
    #cat(res1, " ", res2, " ", color, "\n")
    points(i, j, pch = 21, col = color)
  }
}
