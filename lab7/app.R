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
  
  titlePanel("plug-in алгоритм"),
  sidebarLayout(
    
    # input
    sidebarPanel(
      actionButton("updateX", "Обновить выборки"),
      wellPanel(
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
          inputId = "pr1",
          label = "Значиомсть",
          min = 0.001,
          max = 10,
          value = 1,
          step = 0.1
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
          value = 8
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
          inputId = "pr2",
          label = "Значиомсть",
          min = 0.001,
          max = 10,
          value = 1,
          step = 0.1
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
          value = 2
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
      fluidRow(
        column(2, ""),
        column(2, h3("Входные")),
        column(1, ""),
        column(2, h3("Восстановленные"))
      ),
      fluidRow(
        column(3, h3("Класс 1 матрица", style = "color: darkmagenta")),
        column(1, h4(textOutput("sigma1ia")), h4("0")),
        column(1, h4("0"), h4(textOutput("sigma1ib"))),
        column(1, ""),
        column(1, h4(textOutput("sigma1oa")), h4("0")),
        column(1, h4("0"), h4(textOutput("sigma1ob"))),
      ),
      fluidRow(
        column(3, h3("Класс 1 отклонение", style = "color: darkmagenta")),
        column(1, h4(textOutput("mu1ia"))),
        column(1, h4(textOutput("mu1ib"))),
        column(1, ""),
        column(1, h4(textOutput("mu1oa"))),
        column(1, h4(textOutput("mu1ob"))),
      ),
      fluidRow(
        column(2, ""),
        column(2, h3("Входные")),
        column(1, ""),
        column(2, h3("Восстановленные"))
      ),
      fluidRow(
        column(3, h3("Класс 2 матрица", style = "color: darkcyan")),
        column(1, h4(textOutput("sigma2ia")), h4("0")),
        column(1, h4("0"), h4(textOutput("sigma2ib"))),
        column(1, ""),
        column(1, h4(textOutput("sigma2oa")), h4("0")),
        column(1, h4("0"), h4(textOutput("sigma2ob"))),
      ),
      fluidRow(
        column(3, h3("Класс 2 отклонение", style = "color: darkcyan")),
        column(1, h4(textOutput("mu2ia"))),
        column(1, h4(textOutput("mu2ib"))),
        column(1, ""),
        column(1, h4(textOutput("mu2oa"))),
        column(1, h4(textOutput("mu2ob"))),
      ),
    )
  )
)

estimateMu <- function(xs) {
  l <- dim(xs)[2]
  res <- matrix(NA, 1, l)
  for (i in seq(l)) {
    res[1, i] <- mean(xs[,i])
  }
  return(res)
}
estimateSigma <- function(xs, mu) {
  rows <- dim(xs)[1]
  cols <- dim(xs)[2]
  
  res <- matrix(0, cols, cols)
  for (i in seq(rows)) {
    res <- res + t(xs[i,] - mu) %*% (xs[i,] - mu)
  }
  
  return(res/(rows - 1))
}
getFunc <- function(sigma1, mu1, sigma2, mu2) {
  d1 <- det(sigma1)
  d2 <- det(sigma2)
  invs1 <- solve(sigma1)
  invs2 <- solve(sigma2)
  
  a <- invs1 - invs2
  b <- invs1 %*% t(mu1) - invs2 %*% t(mu2)
  
  A <- a[1,1] # x^2
  B <- a[2,2] # y^2
  C <- 2 * a[1, 2] # xy
  D <- -2 * b[1, 1] # x
  E <- -2 * b[2, 1] # y
  G <- c(mu1 %*% invs1 %*% t(mu1) - mu2 %*% invs2 %*% t(mu2)) + log(abs(det(sigma1))) - log(abs(det(sigma2)))
  
  func <- function(x, y) {
    x^2 * A + y^2 * B + x*y*C + x*D + y*E + G
  }
  
  return(func)
}
getConst <- function(x1, x2, l1, l2) {
  n1 <- dim(x1)[1]
  n2 <- dim(x2)[1]
  p1 <- n1 / (n1+n2)
  p2 <- n2 / (n1+n2)
  return( log((l2*p2)/(l1*p1)) )
}

server <- function(input, output) {
  getData <- eventReactive(input$updateX, {
    n <- input$n
    m <- input$m
    
    sigma1i <- matrix(c(input$sigma1a, 0, 0, input$sigma1b), 2, 2)
    sigma2i <- matrix(c(input$sigma2a, 0, 0, input$sigma2b), 2, 2)
    
    mu1i <- c(input$mu1a, input$mu1b)
    mu2i <- c(input$mu2a, input$mu1b)
    
    xc1 <- mvrnorm(n=n, mu = mu1i, Sigma = sigma1i)
    xc2 <- mvrnorm(n=m, mu = mu2i, Sigma = sigma2i)
    
    return (c(n, m, xc1, xc2))
  })
  
  output$plot = renderPlot({
    sigma1i <- matrix(c(input$sigma1a, 0, 0, input$sigma1b), 2, 2)
    sigma2i <- matrix(c(input$sigma2a, 0, 0, input$sigma2b), 2, 2)
    
    mu1i <- c(input$mu1a, input$mu1b)
    mu2i <- c(input$mu2a, input$mu1b)
    
    dataX <- getData()
    xc1 <- matrix(dataX[seq(3, dataX[1]*2+2)], dataX[1], 2)
    xc2 <- matrix(dataX[-seq(1, dataX[1]*2+2)], dataX[2], 2)
    
    prior1 <- input$pr1
    prior2 <- input$pr2
    
    plotxmin <- min(xc1[,1], xc2[,1]) - 1
    plotymin <- min(xc1[,2], xc2[,2]) - 1
    plotxmax <- max(xc1[,1], xc2[,1]) + 1
    plotymax <- max(xc1[,2], xc2[,2]) + 1

    mu1 <- estimateMu(xc1)
    mu2 <- estimateMu(xc2)
    sigma1 <- estimateSigma(xc1, mu1)
    sigma2 <- estimateSigma(xc2, mu2)
    
    func <- getFunc(sigma1, mu1, sigma2, mu2)
    
    x <- seq(plotxmin-5, plotxmax+5, len = 100)
    y <- seq(plotymin-5, plotymax+5, len = 100)
    z <- outer(x, y, func)
    
    # set proper plot size
    plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))
    
    # draw classes
    colors <- c("magenta", "cyan")
    points(xc1, pch=21, col=colors[1], bg=colors[1])
    points(xc2, pch=21, col=colors[2], bg=colors[2])
    
    # draw line
    contour(x, y, z, levels = getConst(xc2, xc1, prior2, prior1), add = TRUE, drawlabels = TRUE, lwd = 2.5)
    
    # fill table
    output$sigma1ia = renderText(sigma1i[1,1])
    output$sigma1ib = renderText(sigma1i[2,2])
    output$sigma1oa = renderText(sprintf("%.3f", sigma1[1,1]))
    output$sigma1ob = renderText(sprintf("%.3f", sigma1[2,2]))
    output$mu1ia = renderText(mu1i[1])
    output$mu1ib = renderText(mu1i[2])
    output$mu1oa = renderText(sprintf("%.3f", mu1[1]))
    output$mu1ob = renderText(sprintf("%.3f", mu1[2]))
    
    output$sigma2ia = renderText(sigma2i[1,1])
    output$sigma2ib = renderText(sigma2i[2,2])
    output$sigma2oa = renderText(sprintf("%.3f", sigma2[1,1]))
    output$sigma2ob = renderText(sprintf("%.3f", sigma2[2,2]))
    output$mu2ia = renderText(mu2i[1])
    output$mu2ib = renderText(mu2i[2])
    output$mu2oa = renderText(sprintf("%.3f", mu2[1]))
    output$mu2ob = renderText(sprintf("%.3f", mu2[2]))
  })
}

shinyApp(ui, server)