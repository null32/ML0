# euclid distance
dist <- function(a, b) {
  return (sqrt(sum( (a - b) ^ 2)))
}

# 1NN version 1
f1nn <- function(dat, p) {
  res = dat[1, ]
  resDist = dist(p, res[1:2])
  for (i in 2:length(dat[[1]])) {
    k <- dist(dat[i,][1:2], p)
    if (k < resDist) {
      res = dat[i, ]
      resDist = k
    }
  }

  return (res)
}

# 1NN version 2
f1nn2 <- function(dat, p) {
  dists <- vector("list", length(dat[[1]]))
  for (i in 1:length(dat[[1]])) {
    dists[[i]] <- dist(dat[i,][1:2], p)
  }
  dat <- data.frame(dat, "Distance" = unlist(dists))
  
  return ( dat[which.min(dat$Distance),] )
}

# get data and plot it
dat <- iris[3:5]
colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species])

# classify N random points
N = 10
points <- data.frame("x" = runif(N, 1, 7), "y" = runif(N, 0, 2.5))
for (i in seq(10)) {
  p <- points[i, ]
  cl <- f1nn(dat, p)
  points(p[1], p[2], pch = 22, col = colors[cl$Species], bg = colors[cl$Species])
}
