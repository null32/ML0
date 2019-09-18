# euclid distance
dist <- function(a, b) {
  return (sqrt(sum( (a - b) ^ 2)))
}

# 1NN version 2
knn <- function(dat, p, k = 6) {
  dists <- vector("list", length(dat[[1]]))
  for (i in 1:length(dat[[1]])) {
    dists[[i]] <- dist(dat[i,][1:2], p)
  }
  
  # add distance to initial data
  dat <- data.frame(dat, "Distance" = unlist(dists))
  # sort data by distance
  dat <- dat[order(dat$Distance),]
  # take first k values from data
  dat <- head(dat, k)
  # count occurances of each group
  res <- summary(dat$Species)
  
  # most occuring group
  return (names(sort(res, decreasing = TRUE))[1])
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
  cl <- knn(dat, p)
  points(p[1], p[2], pch = 22, col = colors[cl], bg = colors[cl])
}
