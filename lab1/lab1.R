# euclid distance
dist <- function(a, b) {
  return (sqrt(sum( (a - b) ^ 2)))
}

# kNN algo
knn <- function(dat, p, k = c(6)) {
  # calculate distances to each node in data
  dists <- vector("list", length(dat[[1]]))
  for (i in 1:length(dat[[1]])) {
    dists[[i]] <- dist(dat[i,][1:2], p)
  }
  
  # add distance to initial data
  dat <- data.frame(dat, "Distance" = unlist(dists))
  # sort data by distance
  dat <- dat[order(dat$Distance),]

  res <- list()  
  for (i in seq(length(k))) {
    # take first k values from data
    datK <- head(dat, k[i])
    # count occurances of each group
    occs <- summary(datK$Species)
    # most occuring group  
    res[i] <- names(sort(occs, decreasing = TRUE))[1]
  }
  
  return (unlist(res))
}

# Perform LOOCV on 'algo' with 'dat' 
loocv1 <- function(dat, algo, k) {
  l <- length(dat[[1]])
  correct <- rep(0, length(k))
  
  for (i in seq(l)) {
    trainData <- dat[-i, ]
    control <- dat[i, ]
    
    res <- algo(trainData, control[1:2], k)
    correct <- correct + (res != control$Species)
  }
  
  return (correct/l)
}

# plot LOO(k)
looFromK <- function(dat, algo, k) {
  res <- loocv1(dat, algo, k)
  lfromk <- data.frame("k"=k, "LOO"=res)
  plot(lfromk, type="l", main="График зависимости LOO(k)")
  
  # best k with lowest Q
  m = lfromk[which.min(lfromk$LOO),]
  points(m, pch=21, bg="green")
  
  text(m[["k"]], m[["LOO"]], adj=c(0,-1), sprintf("(%i; %.3f)", m[["k"]], m[["LOO"]]))
  
  return (m)
}

# kwNN algo
kwnn <- function(dat, p, k=c(6), q = c(1)) {
  # calculate distances to each node in data
  dists <- vector("list", length(dat[[1]]))
  for (i in 1:length(dat[[1]])) {
    dists[[i]] <- dist(dat[i,][1:2], p)
  }
  
  # add distance to initial data
  dat <- data.frame(dat, "Distance" = unlist(dists))
  # sort data by distance
  dat <- dat[order(dat$Distance),]
  
  lk <- length(k)
  lq <- length(q)
  # result matrix with k values as rows
  # and q values as columns
  # matrix values are classification result 
  res <- array(0, c(lq, lk))
  for (iq in seq(lq)) {
    w <- q[iq]
    
    for (ik in seq(lk)) {
      kv <- k[ik]
      # how much p is close to each class
      freq <- as.list(rep(0, length(levels(dat$Species))))
      names(freq) = levels(dat$Species)
      
      for (j in seq(kv)) {
        e <- dat[j,]
        freq[[e$Species]] <- freq[[e$Species]] + w ^ j
      }

      res[iq, ik] <- names(sort(unlist(freq), decreasing = TRUE))[1]
    }
  }
  
  return (res)
}

# Perform LOOCV on 'algo' with 'dat' 
loocv2 <- function(dat, algo, k, q) {
  l <- length(dat[[1]])
  correct <- array(0, c(length(q), length(k)))
  
  for (i in seq(l)) {
    trainData <- dat[-i, ]
    control <- dat[i, ]
    
    res <- algo(trainData, control[1:2], k=k, q=q)
    correct <- correct + (res != control$Species)
  }
  
  return (correct/l)
}

# plot LOO(k)
looFromW <- function(dat, algo, k, q) {
  res <- loocv2(dat, algo, k, q)
  print(res)
  
  lk <- length(k)
  lq <- length(q)
  kq <- array(0, lk*lq)
  val <- array(0, lk*lq)
  
  for (i in seq(lk)) {
    for (j in seq(lq)) {
      kq[(i-1)*lq +j] <- k[i] + q[j]
      val[(i-1)*lq +j] <- res[j,i]
    }
  }
  
  lfromkq <- data.frame("qk"=kq, "LOO"=val,
                        "k"=unlist(lapply(k, function(x){rep(x, lq)})),
                        "q"=rep(q, lk))
  print(lfromkq)
  plot(lfromkq[1:2], type="l", main="График зависимости LOO(k, q)")
  
  # best k with lowest Q
  m = lfromkq[which.min(lfromkq$LOO),]
  points(m, pch=21, bg="green")
  
  text(m[["qk"]], m[["LOO"]], adj=c(0,-1), sprintf("(%i %.2f; %.3f)", m[["k"]], m[["q"]], m[["LOO"]]))
  
  return (m)
}

# example of usage
demo <- function(algo, title) {
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

proof <- function() {
  a1 <- seq(0, 5, 0.5)
  b1 <- unlist(lapply(a1, function(x) { 2*x - 2 }))
  a2 <- seq(0.25, 5, 0.5)
  b2 <- unlist(lapply(a2, function(x) { 2*x - 3 }))
  a <- c(a1, a2)
  b <- c(b1, b2)
  
  c1 <- seq(0, 5, 1)
  d1 <- unlist(lapply(c1, function(x) { 2*x + 4 }))
  c2 <- seq(0.5, 5, 1)
  d2 <- unlist(lapply(c2, function(x) { 2*x + 3 }))
  a <- c(a, c1, c2)
  b <- c(b, d1, d2)
  
  colors <- factor(c(rep("red", 21), rep("green", 11)))
  cn <- levels(colors)
  df <- data.frame(x=a, y=b, Species=colors)
  plot(df[1:2], pch=21, bg=cn[df$Species], col=cn[df$Species])

  for (i in seq(0, 5, 0.25)) {
    for (j in seq(-4, 5, 1)) {
      p <- c(i, j + 2*i)
      res1 <- knn(df, p)
      res2 <- kwnn(df, p, q=c(0.5))
      if (res1 != res2) {
        points(p[1], p[2], pch=25, col=res2, bg=res2)
      }
      else
      {
        points(p[1], p[2], pch=21, col=res1)
      }
    }
  }
  
  points(df[1:2], pch=21, bg=cn[df$Species], col=cn[df$Species])
}

#demo(knn, "Карта классификации kNN")
#demo(kwnn, "Карта классификации kwNN")

#res <- knn(iris[3:5], c(5, 1.5))
#print(res)

#res <- loocv1(iris[3:5], knn, k=1:150)
#print(res)

#res <- looFromK(iris[3:5], knn, 1:150)
#print(res)

#res <- kwnn(iris[3:5], c(5, 1.5), w=c(0.5, 0.6))
#print(res)

#res <- loocv2(iris[3:5], kwnn, k=5:10, q=seq(0.05, 1, 0.05))
#print(res)

#res <- looFromW(iris[3:5], kwnn, k=5:10, q=seq(0.1, 1, 0.05))
#print(res)

proof()
