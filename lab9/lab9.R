library("MASS")

# Для нормализации выборки
normaliData <- function(x) {
  n <- dim(x)[1]
  m <- dim(x)[2]
  res <- matrix(NA, n, m)
  for (i in seq(m)) {
    minVal <- min(x[,i])
    maxVal <- max(x[,i])
    res[,i] <- (x[,i] - minVal)/(maxVal - minVal)
  }
  return(res)
}

drawLine <- function(w, xmin = -2, xmax = -2, ...) {
  x <- seq(xmin, xmax, len = 100)
  f <- function(x) {
    return( - x*w[1]/w[2] + w[3]/w[2] )
  }
  y <- f(x)
  lines(x, y, type="l", ...)
}

# Квадратичная функция потерь для ADALINE
adaLoss <- function(xi, yi, w) {
  mi <- c(crossprod(w, xi)) * yi
  l <- (mi - 1)^2
  return(l)
}
# 
adaUpd <- function(xi, yi, w, eta) {
  wx <- c(crossprod(w, xi))
  #ld <- 2 * (wx - yi) * xi
  ld <- (wx - yi) * xi
  nextW <- w - eta * ld
  return(nextW)
}

# Кусочно линейная для Хебба
hebbLoss <- function(xi, yi, w) {
  mi <- c(crossprod(w, xi)) * yi
  return (max(-mi, 0))
}
# 
hebbUpd <- function(xi, yi, w, eta) {
  nextW <- w + eta * yi * xi
  return (nextW)
}

## Стохастический градиент
stgrad <- function(xl, eta = 1, lambda = 1/6, eps = 1e-5, loss, upd, ...) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- rep(0.5, n)
  
  Q <- 0
  Qprev <- Q
  
  # Начальное значение Q
  for (i in seq(l)) {
    xi <- xl[i, 1:n]
    yi <- xl[i, n+1]
    
    Q <- Q + loss(xi, yi, w)
  }
  
  iter <- 0
  repeat {
    # мало ли, бесконечный цикл может быть
    iter <- iter + 1
    if (iter > 1000) {
      break
    }
    
    mis <- array(dim = l)
    for (i in seq(l)) {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      
      mis[i] <- crossprod(w, xi) * yi
      #mis[i] <- adaLoss(xi, yi, w)
    }
    
    errorIndexes <- which(mis <= 0)
    if (length(errorIndexes) == 0) {
      break
    }
    
    i <- sample(errorIndexes, 1)
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    
    ex <- loss(xi, yi, w)
    
    w <- upd(xi, yi, w, eta)
    
    Q <- (1 - lambda) * Q + lambda * ex
    # достигли стабилизация Q
    if (abs(Q - Qprev) < eps) {
      break
    }
    Qprev <- Q
    
    drawLine(w, ...)
    #Sys.sleep(0.5)
  }
  
  return(w)
}

colors = c("magenta", "cyan")
n <- 100
m <- 100

sigma1 <- matrix(c(1, 0, 0, 1), 2, 2)
sigma2 <- matrix(c(1, 0, 0, 1), 2, 2)

mu1 <- c(5, 10)
mu2 <- c(11, 10)

xc1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xc2 <- mvrnorm(n=m, mu = mu2, Sigma = sigma2)

dat <- rbind(xc1, xc2)
dat <- normaliData(dat)
# fake property for wj
dat <- cbind(dat, rep(-1, n+m))
# classes
dat <- cbind(dat, c(rep(-1, n), rep(1, m)))

plotxmin <- min(dat[,1], dat[,1]) - 0.3
plotxmax <- max(dat[,1], dat[,1]) + 0.3
plotymin <- min(dat[,2], dat[,2]) - 0.5
plotymax <- max(dat[,2], dat[,2]) + 0.5
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="ADALINE & HEBB")

points(dat, pch=21, col=colors[ifelse(dat[,4] == -1, 1, 2)], bg=colors[ifelse(dat[,4] == -1, 1, 2)])

# adaline
resW <- stgrad(dat, loss = adaLoss, upd = adaUpd, lwd = 1, col = 'lightgreen', xmin = plotxmin, xmax = plotxmax)
drawLine(resW, lwd = 2, col = 'green', xmin = plotxmin, xmax = plotxmax)
# hebb
resW <- stgrad(dat, loss = hebbLoss, upd = hebbUpd, lwd = 1, col = 'pink', xmin = plotxmin, xmax = plotxmax)
drawLine(resW, lwd = 2, col = 'red', xmin = plotxmin, xmax = plotxmax)