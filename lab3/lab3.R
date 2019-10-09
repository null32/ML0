library("plotrix")

# euclid distance
dist <- function(a, b) {
  return (sqrt(sum( (a - b) ^ 2)))
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

# potfunc algo
potfunc <- function(dat, p, core, g, h) {
  ld <- dim(dat)[1]
  classCount <- dim(table(dat$Species))
  
  classes <- rep(0, classCount)
  names(classes) <- levels(dat$Species)
  
  for (i in seq(ld)) {
    e <- dat[i,]
    distance <- dist(p, e[1:2])
    
    weight <- core(distance / h[i]) * g[i]
    classes[e$Species] <- classes[e$Species] + weight
  }
  
  if (max(classes) == 0) {
    return ("")
  }
  return (names(which.max(classes)))
}

# count errors
errVal <- function(dat, core, g, h) {
  ld <- dim(dat)[1]
  err <- 0
  
  for (i in seq(ld)) {
    e <- dat[i,]
    res <- potfunc(dat, e[1:2], core, g, h)
    
    err <- err + (res != e$Species)
  }
  
  cat("err: ", err, "\n")
  #cat("g: ", g, "\n")
  return(err)
}

# calculate g
calcG <- function(dat, core, h, maxErr = 10) {
  ld <- dim(dat)[1]
  
  g <- rep(0, ld)
  i <- 1
  while(errVal(dat, core, g, h) > maxErr) {
    e <- dat[i,]
    res <- potfunc(dat, e[1:2], core, g, h)
    
    if (res != e$Species)
    {
      g[i] <- g[i] + 1
    }
    
    #i <- ((40+sample(1:110,1)[1])%%m)+1
    i <- sample(seq(ld), 1)
    #cat("i: ", i, "\n")
  }
  
  return(g)
}

# example of usage
demo1 <- function(dat, g, h) {
  # plot source data it
  colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main="Карта классификации")
  
  # draw potential circles
  coef <- 0.2
  m <- max(g)
  for (i in seq(length(g))) {
    e <- dat[i,]
    if (g[i] < 1) {
      next
    }
    c <- adjustcolor(colors[e$Species], g[i] / m * coef)
    draw.circle(e[,1], e[,2], h[i], col = c, border = c)
  }
  
  
  #points(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main=title)
  points(dat[1:2], pch = 21, col = "black", bg = colors[dat$Species], main=title)
}

# example of usage
demo2 <- function(dat, g, h) {
  # plot source data
  colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  plot(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species], main="Карта классификации")
  
  # classification map
  for (i in seq(1.0, 7.0, 0.1)) {
    for (j in seq(0.1, 2.5, 0.1)) {
      cl <- potfunc(dat, c(i, j), core4, g, h)
      points(i, j, pch = 21, col = colors[cl])
    }
  }
  
  points(dat[1:2], pch = 21, col = "black", bg = colors[dat$Species], main=title)
}

dat = iris
#dat <- rbind(iris[6:20,], iris[61:75,], iris[136:150,])
ld <- dim(dat)[1]
#h <- c(rep(1, ld/3), rep(0.25, (ld-ld/3)))
h <- c(rep(1, ld/3), rep(0.5, (ld-ld/3)))

res <- calcG(dat[3:5], core4, h, maxErr = 5);

print(res)
demo1(dat[3:5], res, h)
readline(prompt="Enter to continue")
demo2(dat[3:5], res, h)
