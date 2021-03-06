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

# Логистическая регрессия
logregLoss <- function(xi, yi, w) {
  mi <- c(crossprod(w, xi)) * yi
  l <- log2(1+exp(-mi))
  return(l)
}
#
logressUpd <- function(xi, yi, w, eta) {
  sigmoid <- function(z) {
    return(1 / (1 + exp(-z)))
  }
  nextW <- w + eta * xi * yi * sigmoid(-yi * c(crossprod(w, xi)))
  return (nextW)
}

## Стохастический градиент
stgrad <- function(xl, eta = 1, lambda = 1/6, eps = 1e-5, loss, upd, iterBased = 0, ...) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- rep(0.5, n)
  
  Q <- 0
  Qprev <- Q
  Qhist <- rep(NA, 1000)
  Whist <- matrix(NA, 1000, n)
  isIterMax <- FALSE
  
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
    if ((iterBased == 0 && iter > 1000) || (iterBased > 0 && iter > iterBased)) {
      isIterMax <- TRUE
      break
    }
    
    mis <- array(dim = l)
    for (i in seq(l)) {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      
      mis[i] <- crossprod(w, xi) * yi
    }
    
    errorIndexes <- which(mis <= 0)
    if (length(errorIndexes) == 0 && iterBased == 0) {
      break
    }
    
    i <- ifelse(length(errorIndexes) > 0, sample(errorIndexes, 1), sample(seq(l), 1))
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    
    ex <- loss(xi, yi, w)
    w <- upd(xi, yi, w, eta)
    
    Q <- (1 - lambda) * Q + lambda * ex
    Qhist[iter] <- Q
    Whist[iter,] <- w
    # достигли стабилизация Q
    if (abs(Q - Qprev) < eps && iterBased == 0) {
      break
    }
    Qprev <- Q
    
    #drawLine(w, ...)
  }
  
  #print(Qhist[which(!is.na(Qhist))])
  
  if (isIterMax) {
    w <- Whist[which.min(Qhist),]
  }
  return(function(i) {
    if (i == 1) {
      return(w)
    }
    return(Qhist)
  })
}

colors = c("magenta", "cyan")
n <- 100
m <- 100

sigma1 <- matrix(c(1, 0, 0, 1), 2, 2)
sigma2 <- matrix(c(1, 0, 0, 1), 2, 2)

mu1 <- c(8, 10)
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
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="ADALINE & HEBB & LOGISTIC REGRESSION")

points(dat, pch=21, col=colors[ifelse(dat[,4] == -1, 1, 2)], bg=colors[ifelse(dat[,4] == -1, 1, 2)])

# adaline
#resAda <- stgrad(dat, loss = adaLoss, upd = adaUpd, lwd = 1, col = 'lightgreen', xmin = plotxmin, xmax = plotxmax)
#drawLine(resAda(1), lwd = 2, col = 'green', xmin = plotxmin, xmax = plotxmax)

# hebb
#resHebb <- stgrad(dat, loss = hebbLoss, upd = hebbUpd, lwd = 1, col = 'pink', xmin = plotxmin, xmax = plotxmax)
#drawLine(resHebb(1), lwd = 2, col = 'red', xmin = plotxmin, xmax = plotxmax)

# logress
resLogress <- stgrad(dat, loss = logregLoss, upd = logressUpd, lwd = 1, col = 'lightblue', xmin = plotxmin, xmax = plotxmax)
drawLine(resLogress(1), lwd = 2, col = 'blue', xmin = plotxmin, xmax = plotxmax)

w <- resLogress(1)

prob <- function(x, y, w) {
  sigmoid <- function(z) {
    return (1 / (1 + exp(-z)))
  }
  return ( sigmoid(c(crossprod(w, c(x, y, -1))) * -1) - sigmoid(c(crossprod(w, c(x, y, -1))) * 1) )
}

library(plotrix)
for (i in seq(len=50, from = plotxmin, to = plotxmax)) {
  for (j in seq(len=50, from = plotymin, to = plotymax)) {
    pp <- prob(i, j, w)
    if (pp < 0) {
      ca <- adjustcolor(colors[2], alpha.f = -pp)
    } else {
      ca <- adjustcolor(colors[1], alpha.f = pp)
    }
    draw.circle(i, j, radius = 0.005, col = ca, border = ca)
  }
}