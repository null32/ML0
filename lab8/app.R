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
  
  titlePanel("Линейный дискриминант фишера"),
  sidebarLayout(
    
    # input
    sidebarPanel(
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
      ),
      wellPanel(
        tags$h2("Ковариационная матрица"),
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
      ),
    ),
    # output
    mainPanel(
      plotOutput(
        outputId = "plot",
        height =  "600px",
      ),
      fluidRow(
        column(1, h4("Риск:")),
        column(1, h4(textOutput("risk")))
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

estimateSigma <- function(xs1, mu1, xs2, mu2) {
  rows1 <- dim(xs1)[1]
  cols <- dim(xs1)[2]
  rows2 <- dim(xs2)[1]
  
  res <- matrix(0, cols, cols)
  for (i in seq(rows1)) {
    res <- res + t(xs1[i,] - mu1) %*% (xs1[i,] - mu1)
  }
  for (i in seq(rows2)) {
    res <- res + t(xs2[i,] - mu2) %*% (xs2[i,] - mu2)
  }
  
  return(res/(rows1 + rows2 + 2))
}

getFunc <- function(sigma1, mu1, mu2) {
  d1 <- det(sigma1)
  invs1 <- solve(sigma1)
  
  b <- invs1 %*% t(mu1 - mu2)
  
  D <- b[1, 1] # x
  E <- b[2, 1] # y
  mu <- (mu1 + mu2)
  G <- c(mu %*% b) / 2
  
  func <- function(x) {
    -x*D/E + G/E
  }
  
  return(func)
}

getRisk <- function(mu1, mu2, sigma) {
  mah <- (mu1 - mu2) %*% solve(sigma) %*% t(mu1 - mu2)
  # print(mah)
  mah <- mah * -0.5
  res <- gausian(mah, 0, 1)
}
gausian <- function(x, M, D){
  return( (1/(D*sqrt(2*pi))) * exp(-1 * ((x - M)^2)/(2*D^2)) )
}

server <- function(input, output) {
  output$plot = renderPlot({
    
    n <- input$n
    m <- input$m
    
    sigma1i <- matrix(c(input$sigma1a, 0, 0, input$sigma1b), 2, 2)
    
    mu1i <- c(input$mu1a, input$mu1b)
    mu2i <- c(input$mu2a, input$mu2b)
    
    xc1 <- mvrnorm(n=n, mu = mu1i, Sigma = sigma1i)
    xc2 <- mvrnorm(n=m, mu = mu2i, Sigma = sigma1i)
    
    plotxmin <- min(xc1[,1], xc2[,1]) - 1
    plotymin <- min(xc1[,2], xc2[,2]) - 1
    plotxmax <- max(xc1[,1], xc2[,1]) + 1
    plotymax <- max(xc1[,2], xc2[,2]) + 1

    mu1 <- estimateMu(xc1)
    mu2 <- estimateMu(xc2)
    sigma1 <- estimateSigma(xc1, mu1, xc2, mu2)
    
    func <- getFunc(sigma1, mu1, mu2)
    
    # set proper plot size
    plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))
    
    # draw classes
    colors <- c("magenta", "cyan")
    points(xc1, pch=21, col=colors[1], bg=colors[1])
    points(xc2, pch=21, col=colors[2], bg=colors[2])
    
    #draw lines
    x <- seq(plotxmin-5, plotxmax+5, len = 100)
    lines(x, func(x), lwd = 2.5, type="l")
    lines(c(mu1i[1], mu2i[1]), c(mu1i[2], mu2i[2]), col = 'gray', lwd = 2)
    
    # fill table
    output$sigma1ia = renderText(sigma1i[1,1])
    output$sigma1ib = renderText(sigma1i[2,2])
    output$sigma1oa = renderText(sprintf("%.3f", sigma1[1,1]))
    output$sigma1ob = renderText(sprintf("%.3f", sigma1[2,2]))
    output$mu1ia = renderText(mu1i[1])
    output$mu1ib = renderText(mu1i[2])
    output$mu1oa = renderText(sprintf("%.3f", mu1[1]))
    output$mu1ob = renderText(sprintf("%.3f", mu1[2]))
    
    output$sigma2ia = renderText(sigma1i[1,1])
    output$sigma2ib = renderText(sigma1i[2,2])
    output$sigma2oa = renderText(sprintf("%.3f", sigma1[1,1]))
    output$sigma2ob = renderText(sprintf("%.3f", sigma1[2,2]))
    output$mu2ia = renderText(mu2i[1])
    output$mu2ib = renderText(mu2i[2])
    output$mu2oa = renderText(sprintf("%.3f", mu2[1]))
    output$mu2ob = renderText(sprintf("%.3f", mu2[2]))
    
    output$risk = renderText(getRisk(mu1, mu2, sigma1))
  })
}

shinyApp(ui, server)