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
  plot(lfromk, type="l")
  
  # best k with lowest Q
  m = lfromk[which.min(lfromk$LOO),]
  points(m, pch=21, bg="green")
  
  return (m)
}

# kwNN algo
kwnn <- function(dat, p, k=6, q = c(0.8)) {
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
  # take first k values from data
  datK <- head(dat, k)
  
  freq <- as.list(rep(0, length(levels(dat$Species))))
  names(freq) = levels(dat$Species)
  
  for (j in seq(length(q))) {
    for (l in seq(k)) {
      e <- datK[l,]
      freq[[e$Species]] <- freq[[e$Species]] + q[j] ^ l
    }
    # most occuring group  
    res[j] <- names(sort(unlist(freq), decreasing = TRUE))[1]
  }
  
  return (unlist(res))
}

# Perform LOOCV on 'algo' with 'dat' 
loocv2 <- function(dat, algo, q) {
  l <- length(dat[[1]])
  correct <- rep(0, length(q))
  
  for (i in seq(l)) {
    trainData <- dat[-i, ]
    control <- dat[i, ]
    
    res <- algo(trainData, control[1:2], q=q)
    correct <- correct + (res != control$Species)
  }
  
  return (correct/l)
}

# plot LOO(k)
looFromW <- function(dat, algo, q) {
  res <- loocv2(dat, algo, q)
  print(res)
  lfromk <- data.frame("q"=q, "LOO"=res)
  plot(lfromk, type="l")
  
  # best k with lowest Q
  m = lfromk[which.min(lfromk$LOO),]
  points(m, pch=21, bg="green")
  
  return (m)
}

# example of usage
demo <- function(algo) {
  # get data and plot it
  dat <- iris[3:5]
  colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species])
  
  # classification map
  for (i in seq(1.0, 7.0, 0.1)) {
    for (j in seq(0.1, 2.5, 0.1)) {
      cl <- algo(dat, c(i, j))
      points(i, j, pch = 21, col = colors[cl])
    }
  }
}

#demo(knn)
#demo(kwnn)

#res <- knn(iris[3:5], c(5, 1.5))
#print(res)

#res <- loocv1(iris[3:5], knn, k=1:150)
#print(res)

#res <- looFromK(iris[3:5], knn, 1:150)
#print(res)

#res <- kwnn(iris[3:5], c(5, 1.5), w=c(0.5, 0.6))
#print(res)

#res <- loocv2(iris[3:5], kwnn, q=seq(0.05, 1, 0.05))
#print(res)

res <- looFromW(iris[3:5], kwnn, q=seq(0.1, 1, 0.05))
print(res)