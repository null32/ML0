library(shiny)
library(MASS)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well {
        padding: 1rem;
      }
    ")
    )
  ),
  
  titlePanel("ADALINE & HEBB & LOGRESS"),
  sidebarLayout(
    
    # input
    sidebarPanel(
      wellPanel(
        checkboxInput("midada", "Промежуточные линии для ADALINE", FALSE),
        checkboxInput("midhebb", "Промежуточные линии для HEBB", FALSE),
        checkboxInput("midlogress", "Промежуточные линии для LOGRESS", FALSE),
        tags$h2("Первый класс", style = "color: darkmagenta"),
        sliderInput(
          inputId = "n",
          label = "Количество элементов",
          min = 100,
          max = 500,
          value = 200,
          step = 50
        ),
        sliderInput(
          inputId = "sigma1a",
          label = "Элемент [1, 1]",
          min = 0.1,
          max = 20,
          value = 5,
          step = 0.1
        ),
        sliderInput(
          inputId = "sigma1b",
          label = "Элемент [2, 2]",
          min = 0.1,
          max = 20,
          value = 1,
          step = 0.1
        ),
        sliderInput(
          inputId = "mu1a",
          label = "Отклонение по X",
          min = 0,
          max = 20,
          value = 0
        ),
        sliderInput(
          inputId = "mu1b",
          label = "Отклонение по Y",
          min = 0,
          max = 20,
          value = 0
        )
      ),
      wellPanel(
        tags$h2("Второй класс", style = "color: darkcyan"),
        sliderInput(
          inputId = "m",
          label = "Количество элементов",
          min = 100,
          max = 500,
          value = 200,
          step = 50
        ),
        sliderInput(
          inputId = "sigma2a",
          label = "Элемент [1, 1]",
          min = 0.1,
          max = 20,
          value = 1,
          step = 0.1
        ),
        sliderInput(
          inputId = "sigma2b",
          label = "Элемент [2, 2]",
          min = 0.1,
          max = 20,
          value = 5,
          step = 0.1
        ),
        sliderInput(
          inputId = "mu2a",
          label = "Отклонение по X",
          min = 0,
          max = 20,
          value = 10
        ),
        sliderInput(
          inputId = "mu2b",
          label = "Отклонение по Y",
          min = 0,
          max = 20,
          value = 6
        )
      )
    ),
    # output
    mainPanel(
      plotOutput(
        outputId = "plot",
        height =  "600px",
      ),
      tags$h2("ADALINE", style = "color: green"),
      plotOutput(
        outputId = "q1",
        height =  "300px",
      ),
      tags$h2("HEBB", style = "color: red"),
      plotOutput(
        outputId = "q2",
        height =  "300px",
      ),
      tags$h2("LOGISTIC REGRESSION", style = "color: blue"),
      plotOutput(
        outputId = "q3",
        height =  "300px",
      ),
    )
  )
)

# Для нормализации выборки
normaliData <- function(x) {
  n <- dim(x)[1]
  m <- dim(x)[2]
  res <- matrix(NA, n, m)
  for (i in seq(m)) {
    minVal <- min(x[,i])
    maxVal <- max(x[,i])
    res[,i] <- (x[,i] - minVal)/(maxVal - minVal)
  }
  return(res)
}

drawLine <- function(w, xmin = -2, xmax = -2, ...) {
  x <- seq(xmin, xmax, len = 100)
  f <- function(x) {
    return( - x*w[1]/w[2] + w[3]/w[2] )
  }
  y <- f(x)
  lines(x, y, type="l", ...)
}

adaLoss <- function(xi, yi, w) {
  mi <- c(crossprod(w, xi)) * yi
  l <- (mi - 1)^2
  return(l)
}
adaUpd <- function(xi, yi, w, eta) {
  wx <- c(crossprod(w, xi))
  #ld <- 2 * (wx - yi) * xi
  ld <- (wx - yi) * xi
  nextW <- w - eta * ld
  return(nextW)
}

hebbLoss <- function(xi, yi, w) {
  mi <- c(crossprod(w, xi)) * yi
  return (max(-mi, 0))
}
hebbUpd <- function(xi, yi, w, eta) {
  nextW <- w + eta * yi * xi
  return (nextW)
}

logregLoss <- function(xi, yi, w) {
  mi <- c(crossprod(w, xi)) * yi
  l <- log2(1+exp(-mi))
  return(l)
}
logressUpd <- function(xi, yi, w, eta) {
  sigmoid <- function(z) {
    return(1 / (1 + exp(-z)))
  }
  nextW <- w + eta * xi * yi * sigmoid(-yi * c(crossprod(w, xi)))
  return (nextW)
}


## Стохастический градиент
stgrad <- function(xl, eta = 1, lambda = 1/6, eps = 1e-5, loss, upd, midlines = FALSE, iterBased = 0, ...) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- rep(0.5, n)
  
  Q <- 0
  Qprev <- Q
  Qhist <- rep(NA, 1000)
  Whist <- matrix(NA, 1000, n)
  isIterMax <- FALSE
  
  # Начальное значение Q
  for (i in seq(l)) {
    xi <- xl[i, 1:n]
    yi <- xl[i, n+1]
    
    Q <- Q + loss(xi, yi, w)
  }
  
  iter <- 0
  repeat {
    # мало ли, бесконечный цикл может быть
    iter <- iter + 1
    if ((iterBased == 0 && iter > 1000) || (iterBased > 0 && iter > iterBased)) {
      isIterMax <- TRUE
      break
    }
    
    mis <- array(dim = l)
    for (i in seq(l)) {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      
      mis[i] <- crossprod(w, xi) * yi
    }
    
    errorIndexes <- which(mis <= 0)
    if (length(errorIndexes) == 0 && iterBased == 0) {
      break
    }
    
    i <- ifelse(length(errorIndexes) > 0, sample(errorIndexes, 1), sample(seq(l), 1))
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    
    ex <- loss(xi, yi, w)
    w <- upd(xi, yi, w, eta)
    
    Q <- (1 - lambda) * Q + lambda * ex
    Qhist[iter] <- Q
    Whist[iter,] <- w
    # достигли стабилизация Q
    if (abs(Q - Qprev) < eps && iterBased == 0) {
      break
    }
    Qprev <- Q
    
    if (midlines) {
      drawLine(w, ...)
    }
  }
  
  #print(Qhist[which(!is.na(Qhist))])
  
  if (isIterMax) {
    w <- Whist[which.min(Qhist),]
  }
  return(function(i) {
    if (i == 1) {
      return(w)
    }
    return(Qhist)
  })
}

server <- function(input, output) {
  output$plot = renderPlot({
    colors = c("magenta", "cyan")
    n <- input$n
    m <- input$m
    
    sigma1i <- matrix(c(input$sigma1a, 0, 0, input$sigma1b), 2, 2)
    sigma2i <- matrix(c(input$sigma2a, 0, 0, input$sigma2b), 2, 2)
    
    mu1i <- c(input$mu1a, input$mu1b)
    mu2i <- c(input$mu2a, input$mu1b)
    
    xc1 <- mvrnorm(n=n, mu = mu1i, Sigma = sigma1i)
    xc2 <- mvrnorm(n=m, mu = mu2i, Sigma = sigma2i)
    
    dat <- rbind(xc1, xc2)
    dat <- normaliData(dat)
    # fake property for wj
    dat <- cbind(dat, rep(-1, n+m))
    # classes
    dat <- cbind(dat, c(rep(-1, n), rep(1, m)))
    
    plotxmin <- min(dat[,1], dat[,1]) - 0.3
    plotxmax <- max(dat[,1], dat[,1]) + 0.3
    plotymin <- min(dat[,2], dat[,2]) - 0.5
    plotymax <- max(dat[,2], dat[,2]) + 0.5
    plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="ADALINE & HEBB")
    
    points(dat, pch=21, col=colors[ifelse(dat[,4] == -1, 1, 2)], bg=colors[ifelse(dat[,4] == -1, 1, 2)])
    
    # adaline
    resW1 <- stgrad(dat, loss = adaLoss, upd = adaUpd, lwd = 1, col = 'lightgreen', midlines = input$midada, xmin = plotxmin, xmax = plotxmax)
    # hebb
    resW2 <- stgrad(dat, loss = hebbLoss, upd = hebbUpd, lwd = 1, col = 'pink', midlines = input$midhebb, xmin = plotxmin, xmax = plotxmax)
    # logress
    resW3 <- stgrad(dat, loss = logregLoss, upd = logressUpd, lwd = 1, col = 'lightblue', midlines = input$midlogress, xmin = plotxmin, xmax = plotxmax)
    
    drawLine(resW1(1), lwd = 2, col = 'red', xmin = plotxmin, xmax = plotxmax)
    drawLine(resW2(1), lwd = 2, col = 'green', xmin = plotxmin, xmax = plotxmax)
    drawLine(resW3(1), lwd = 2, col = 'blue', xmin = plotxmin, xmax = plotxmax)
    
    output$q1 = renderPlot({
      q <- resW1(2)
      x <- seq(length(q))
      plot(x, q, type='l', lwd = 1)
    })
    output$q2 = renderPlot({
      q <- resW2(2)
      x <- seq(length(q))
      plot(x, q, type='l', lwd = 1)
    })
    output$q3 = renderPlot({
      q <- resW3(2)
      x <- seq(length(q))
      plot(x, q, type='l', lwd = 1)
    })
  })
}

shinyApp(ui, server)