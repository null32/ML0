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
  for (i in seq(k)) {
    # take first k values from data
    datK <- head(dat, i)
    # count occurances of each group
    occs <- summary(datK$Species)
    # most occuring group  
    res[i] <- names(sort(occs, decreasing = TRUE))[1]
  }
  
  return (unlist(res))
}

# plot LOO(k)
looFromK <- function(dat, algo, k) {
  res <- loocv(dat, algo, k)
  plot(data.frame("k"=k, "LOO(k)"=res), type="l")
}

# Perform LOOCV on 'algo' with 'dat' 
loocv <- function(dat, algo, k) {
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

# example of usage
demo <- function() {
  # get data and plot it
  dat <- iris[3:5]
  colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species])
  
  # classify N random points
  N = 10
  points <- data.frame("x" = runif(N, 1, 7), "y" = runif(N, 0, 2.5))
  for (i in seq(10)) {
    p <- points[i, ]
    cl <- knn(dat, p)
    points(p[1], p[2], pch = 22, col = colors[cl], bg = colors[cl])
  }
}

#res <- knn(iris[3:5], c(2, 2), k=1:10)
#print(res)

#res <- loocv(iris[3:5], knn, k=1:150)
#print(res)

looFromK(iris[3:5], knn, seq(150))
