# euclid distance
dist <- function(a, b) {
  return (sqrt(sum( (a - b) ^ 2)))
}

#src: http://www.machinelearning.ru/wiki/index.php?title=Метод_Парзеновского_окна_(пример)
# Епанечникова
# h=0.35 LOO=0.04
core1 <- function(r) {
  if (abs(r) > 1) {
    return (0)
  }
  return ((3/4) * (1 - r*r))
}
# Квартическое
# h=0.35 LOO=0.04
core2 <- function(r) {
  if (abs(r) > 1) {
    return (0)
  }
  return ((15/16) * (1 - r*r)^2)
}
# Треугольное
# h=0.35 LOO=0.04
core3 <- function(r) {
  if (abs(r) > 1) {
    return (0)
  }
  return (1 - abs(r))
}
# Гауссовское
# h=0.1 LOO=0.04
core4 <- function(r) {
  (2*pi)^0.5 * exp(-0.5 * r*r)
}
# Прямоугольное
# h=0.35 LOO=0.04
core5 <- function(r) {
  if (abs(r) > 1) {
    return (0)
  }
  return (0.5)
}


# kNN algo
parwin <- function(dat, p, core=core1, h = c(0.35), ...) {
  ld <- length(dat[[1]])
  lh <- length(h)
  # calculate distances to each node in data
  dists <- vector("list", )
  for (i in seq(ld)) {
    dists[[i]] <- dist(dat[i,][1:2], p)
  }
  
  # add distance to initial data
  dat <- data.frame(dat, "Distance" = unlist(dists))
  # sort data by distance
  dat <- dat[order(dat$Distance),]
  
  res <- array(0, lh)
  for (i in seq(lh)) {
    vh <- h[i]
    # how much p is close to each class
    freq <- as.list(rep(0, length(levels(dat$Species))))
    names(freq) = levels(dat$Species)
    for (j in seq(ld)) {
      vd <- dat[j,]
      
      freq[[vd$Species]] <- freq[[vd$Species]] + core(vd$Distance / vh)
    }
    
    freq <- unlist(freq)
    if (max(freq) == 0) {
      res[i] <- ""
    }
    else
    {
      res[i] <- names(sort(freq, decreasing = TRUE))[1]
    }
  }
  
  return (res)
}

# Perform LOOCV on 'algo' with 'dat' 
loocv1 <- function(dat, algo, n, ...) {
  l <- length(dat[[1]])
  correct <- rep(0, n)
  
  for (i in seq(l)) {
    trainData <- dat[-i, ]
    control <- dat[i, ]
    
    res <- algo(trainData, control[1:2], ...)
    correct <- correct + (res != control$Species)
  }
  
  return (correct/l)
}

# plot LOO()
looFromH <- function(dat, algo, h, title, core) {
  res <- loocv1(dat, algo, length(h), h=h, core=core)
  lfromh <- data.frame("h"=h, "LOO"=res)
  plot(lfromh, type="l", main=title)
  
  # best h with lowest Q
  m = lfromh[which.min(lfromh$LOO),]
  points(m, pch=21, bg="green")
  
  text(m[["h"]], m[["LOO"]], adj=c(0,-1), sprintf("(%.3f; %.3f)", m[["h"]], m[["LOO"]]))
  
  return (m)
}

# example of usage
demo <- function(algo, title="Unknown plot") {
  # get data and plot it
  dat <- iris[3:5]
  colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main=title)
  
  # classification map
  for (i in seq(1.0, 7.0, 0.1)) {
    for (j in seq(0.1, 2.5, 0.1)) {
      cl <- algo(dat, c(i, j))
      points(i, j, pch = 21, col = colors[cl])
    }
  }
  
  points(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main=title)
}


demo(parwin, "Карта классификации")

#res <- loocv1(iris[3:5], parwin, 2, h=c(0.5, 1))
#print(res)

#res <- looFromH(iris[3:5], parwin, seq(0.1, 4, 0.05), "Ядро Прямоугольное", core5)
#print(res)
