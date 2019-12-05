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
    return( x*w[1]/w[2] - w[3]/w[2] )
  }
  y <- f(x)
  lines(x, y, type="l", ...)
}

# Квадратичная функция потерь для ADALINE
lossAda <- function(xi, yi, w) {
  mi <- c(crossprod(w, xi)) * yi
  l <- (mi - 1)^2
  return(l)
}
# её производная для град. спуска
lossAdaDer <- function(xi, yi, w) {
  wx <- c(crossprod(w, xi))
  #ld <- 2 * (wx - yi) * xi
  ld <- (wx - yi) * xi
  return(ld)
}

## Стохастический градиент для ADALINE
sgada <- function(xl, eta = 1, lambda = 1/6, eps = 1e-5, ...) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- rep(0.5, n)
  
  Q <- 0
  Qprev <- Q
  
  # Начальное значение Q
  for (i in seq(l)) {
    xi <- xl[i, 1:n]
    yi <- xl[i, n+1]
    
    Q <- Q + lossAda(xi, yi, w)
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
    }
    
    errorIndexes <- which(mis <= 0)
    if (length(errorIndexes) == 0) {
      break
    }
    
    i <- sample(errorIndexes, 1)
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    
    ex <- lossAda(xi, yi, w)
    
    w <- w - eta * lossAdaDer(xi, yi, w)
    
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
n <- 300
m <- 300

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
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="ADALINE")

points(dat, pch=21, col=colors[ifelse(dat[,4] == -1, 1, 2)], bg=colors[ifelse(dat[,4] == -1, 1, 2)])

resW <- sgada(dat, lwd = 1, col = 'gray', xmin = plotxmin, xmax = plotxmax)
drawLine(resW, lwd = 2, col = 'green', xmin = plotxmin, xmax = plotxmax)

## Функция потерь для правила Хэбба
lossPerceptron <- function(x) {
  return (max(-x, 0))
}
## Стохастический градиент с правилом Хебба
sg.Hebb <- function(xl, eta = 0.1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  ## initialize Q
  Q <- 0
  for (i in 1:l) {
    ## calculate the scalar product <w,x>
    wx <- sum(w * xl[i, 1:n])
    ## calculate a margin
    margin <- wx * xl[i, n + 1]
    # Q <- Q + lossQuad(margin)
    Q <- Q + lossPerceptron(margin)
  }
  repeat {
    ## Поиск ошибочных объектов
    margins <- array(dim = l)
    for (i in 1:l) {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      margins[i] <- crossprod(w, xi) * yi
    }
    ## выбрать ошибочные объекты
    errorIndexes <- which(margins <= 0)
    if (length(errorIndexes) > 0) {
      # выбрать случайный ошибочный объект
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      w <- w + eta * yi * xi
    } else {
      break
    }
  }
  return (w)
}