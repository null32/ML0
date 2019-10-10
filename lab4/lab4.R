# euclid distance
dist <- function(a, b) {
  return (sqrt(sum( (a - b) ^ 2)))
}

knnW <- function(dat, group, p, k = 6) {
  ld <- dim(dat)[1]
  lc <- dim(table(dat$Species))
  
  # distance + index to then sort dat
  dists <- matrix(NA, ld, 2)
  for (i in seq(ld)) {
    e <- dat[i,]
    dists[i,] <- c(i, dist(e[1:2], p))
  }
  
  top <- dat[order(dists[,2])[1:k], 3]
  res <- sum(top == group)
  return (res)
}

stolp <- function(dat, funcW, ...) {
  ld <- dim(dat)[1]
  classCount <- dim(table(dat$Species))
  
  margins <- rep(0, ld)
  
  for (i in seq(ld)) {
    e <- dat[i,]
    otherClasses <- classes[which(names(classes) != names(classes[e$Species]))]
    wmain <- funcW(dat, e$Species, e[1:2], ...)
    
    wother <- sapply(otherClasses, function(x) { funcW(dat, x, e[1:2], ...) })
    print(wother)
    m <- max(wother)
    
    #print(w)
    margins[i] <- w
  }
  print(w)
}

stolp(iris[3:5], knnW)
#res <- knnW(iris[3:5], iris[1,]$Species, iris[1, 3:4])
#print(res)