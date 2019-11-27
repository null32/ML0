dat <- iris[3:5]
colors = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
core <- function(r) {
  (2*pi)^0.5 * exp(-0.5 * r*r)
}

# plot(dat[1:2], pch=21, col=colors[dat$Species], bg=colors[dat$Species])

dist <- function(a, b) {
  (sqrt(sum((a-b)^2)))
} 

naiveBayes <- function(dat, x) {
  n <- dim(dat)[1]            # number of elements
  m <- dim(dat)[2] - 1        # number of properties
  lvls <- levels(dat$Species) # classes
  k <- length(lvls)           # number of classes
  
  prob <- 0
  dens <- 0
  
  res <- rep(0, k)
  names(res) <- lvls
  
  for (i in seq(k)) {         # for each class
    thisclass <- dat[dat$Species == lvls[i], ]
    l <- dim(thisclass)[1]
    prob <- n / l
    
    for (j in seq(l)) {       # for each element in class
      for (q in seq(m)) {     # for each property of element in class
        dens <- dens + log( core(dist(x[q], thisclass[j, q])) )
      }
    }
    
    res[i] <- log(prob) + dens
  }
  
  
  return (names(which.max(res)))
}

# example of usage
demo <- function(title="Unknown plot") {
  # get data and plot it
  plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main=title)
  
  # classification map
  for (i in seq(1.0, 7.0, 0.1)) {
    for (j in seq(0.1, 2.5, 0.1)) {
      cl <- naiveBayes(dat, c(i, j))
      points(i, j, pch = 21, col = colors[cl])
    }
  }
  
  points(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main=title)
}

demo()