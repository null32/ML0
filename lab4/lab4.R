# euclid distance
dist <- function(a, b) {
  return (sqrt(sum( (a - b) ^ 2)))
}

# for each class get w(class)
knnW <- function(dat, p, k = 6) {
  ld <- dim(dat)[1]
  lc <- dim(table(dat$Species))
  
  # distance + index to then sort dat
  dists <- matrix(NA, ld, 2)
  for (i in seq(ld)) {
    e <- dat[i,]
    dists[i,] <- c(i, dist(e[1:2], p))
  }
  
  return (c(table(dat[order(dists[,2])[1:k], 3])))
}
# for each class get w(class)
kwnnW <- function(dat, p, k = 6, w = 0.5) {
  ld <- dim(dat)[1]
  lc <- dim(table(dat$Species))
  
  # distance + index to then sort dat
  dists <- matrix(NA, ld, 2)
  for (i in seq(ld)) {
    e <- dat[i,]
    dists[i,] <- c(i, dist(e[1:2], p))
  }
  
  top <- dat[order(dists[,2])[1:k],]
  res <- rep(0, lc)
  names(res) <- levels(dat$Species)
  
  for (i in seq(ld)) {
    e <- top[i,]$Species
    res[e] <- res[e] + w ^ i
  }
  
  return (res)
}

margins <- function(dat, funcW, ...) {
  ld <- dim(dat)[1]
  classCount <- dim(table(dat$Species))
  
  margins <- rep(0, ld)
  
  for (i in seq(ld)) {
    e <- dat[i,]
    ws <- funcW(dat[-i,], e[1:2], ...)
    w <- ws[e$Species] - max(ws[which(names(ws) != e$Species)])
    #print(w)
    margins[i] <- w
  }
  
  return (margins / max(abs(margins)))
}

foo <- function(x, y, col, n = 500, ...) {
  plot(x, y, type = "n", ...)
  e <- par('usr')
  height <- diff(e[3:4]) / (n - 1)
  y_up <- seq(0, e[4], height)
  y_down <- seq(0, e[3], - height)
  y_all <- c(rev(y_down), y_up)
  ncolor <- length(y_all)
  pal <- if (!is.function(col)) colorRampPalette(col)(ncolor) else col(ncolor)
  sapply(seq_len(n),
         function(i) {
           rect(min(x), y_all[i], max(x), y_all[i] + height, col = pal[i], border = NA)
         })
  polygon(c(min(x), x, max(x), rev(x)),
          c(e[4], ifelse(y > 0, y, 0),
            rep(e[4], length(y) + 1)), col = 'white', border = NA)
  polygon(c(min(x), x, max(x), rev(x)),
          c(e[3], ifelse(y < 0, y, 0),
            rep(e[3], length(y) + 1)), col = 'white', border = NA)
  box()
}

drawGradient <- function() {
  res <- margins(iris[3:5], knnW)
  res <- sort(res)
  
  palette <- colorRampPalette(c("darkred", "red", "yellow", "green", "darkgreen"))(150)
  foo(1:150, res, palette,
      main="График отступов относительно knn (k=6)",
      xlab="Ирисы Фишера",
      ylab="Отступ")
  
  # пограничные
  m <- 0.15
  #m <- min(abs(c(res[which.min(abs(res-0.1))], res[which.min(abs(res+0.1))])))
  polygon(c(0, 150), c(-m, -m))
  polygon(c(0, 150), c(m, m))
  
  m <- 0.4
  # неинформативыне и шумовые
  polygon(c(0, 150), c(-m, -m))
  polygon(c(0, 150), c(m, m))
  
  text(75, 0, "пограничные")
  text(75, 0.3, "неинформативные")
  text(75, -0.3, "шумовые")
  text(75, 0.6, "эталонные")
  text(75, -0.6, "ошибочные")
}

stolp <- function(dat, l=0, sigma=-0.1, funcW = kwnnW, doplot = TRUE) {
  colors = c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  
  margs <- margins(dat, funcW)
  
  # отброситьь выбросы
  bad <- which(margs < sigma)
  dat <- dat[-bad,]
  margs <- margs[-bad]

  # взять по эталону из каждого класса
  classes <- levels(dat$Species)
  omega <- data.frame(dat[-seq(dim(dat)[1]),])
  for (i in seq(length(classes))) {
    omega[dim(omega)[1]+1,] <- (dat[which(dat$Species == classes[i]),])[which.max(margs),]
  }
  
  queue <- seq(dim(dat)[1])[order(margs)]
  step <- 1
  while (dim(omega)[1] != dim(dat)[1]) {
    n <- dim(dat)[1]
    err <- rep(FALSE, n)
    for (i in seq(n)) {
      e <- dat[i,]
      err[i] <- names(which.max(funcW(omega, e[1:2]))) != e$Species
    }
    
    # Отрисовка
    if (doplot) {
      # Все
      plot(dat[1:2], pch=21, col = colors[dat$Species], main = sprintf("Шаг %i, ошибок %i", step, sum(err)))
      
      # ошибочные
      wrong <- dat[which(err),]
      points(wrong[1:2], pch=22, col = colors[wrong$Species], bg = colors[wrong$Species])
      
      # Эталонные
      points(omega[1:2], pch=21, col = colors[omega$Species], bg = colors[omega$Species])
      
    }
    
    if (sum(err) <= l) {
      break
    }
    
    omega[dim(omega)[1]+1,] <- dat[queue[1],]
    queue <- queue[-1]
    step <- step + 1
  }
  
  return (omega)
}

makeMap <- function(dat, funcW = kwnnW, title="Unknown plot") {
  # to set right scale and stuff
  plot(iris[3:4], type = "n", main=title)
  
  colors <- c("setosa" = "red", "versicolor" = "green", "virginica" = "blue")
  points(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species])
  
  # classification map
  for (i in seq(1.0, 7.0, 0.1)) {
    for (j in seq(0.1, 2.5, 0.1)) {
      cl <- names(which.max(funcW(dat, c(i, j))))
      points(i, j, pch = 21, col = colors[cl])
    }
  }
  
  points(dat[1:2], pch = 21, col = colors[dat$Species], bg = colors[dat$Species])
}

calcTime <- function() {
  omega <- stolp(iris[3:5], doplot = FALSE)
  print(omega)
  time <- Sys.time()
  makeMap(omega, title = "Карта классификации на основе STOLP")
  time <- Sys.time() - time
  print(time)
  
  time <- Sys.time()
  makeMap(iris[3:5], title = "Карта классификации на всех ирисах")
  time <- Sys.time() - time
  print(time)
}

checkQuality <- function(dat, funcW = kwnnW) {
  omega <- stolp(dat, funcW = funcW, doplot = FALSE)
  ld <- dim(dat)[1]
  e1 <- 0
  e2 <- 0
  for (i in seq(ld)) {
    x <- dat[-i,]
    e <- dat[i,]
    
    res <- funcW(x, e[1:2])
    e1 <- e1 + (names(which.max(res)) != e$Species)
    
    res <- funcW(omega, e[1:2])
    e2 <- e2 + (names(which.max(res)) != e$Species)
  }
  print(e1/ld)
  print(e2/ld)
}

#res <- margins(iris[3:5], knnW)
#res <- margins(iris[3:5], kwnnW)
#res <- sort(res)
#print(res)

#drawGradient()
#res <- stolp(iris[3:5])
#print(res)

#calcTime()

checkQuality(iris[3:5])
