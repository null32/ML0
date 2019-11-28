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
  
  titlePanel("Наивный нормальный байесовский классификатор"),
  sidebarLayout(
    
    # input
    sidebarPanel(
      wellPanel(
        tags$h2("Первый класс", style = "color: red"),
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
          value = 1,
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
        tags$h2("Второй класс", style = "color: green"),
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
        column(2, h3("Мат ожидание")),
        column(1, ""),
        column(2, h3("Дисперсия"))
      ),
      fluidRow(
        column(3, h3("Класс 1", style = "color: red")),
        column(1, h4(textOutput("m1a")), h4(textOutput("m1b"))),
        column(1, ""),
        column(1, h4(textOutput("d1a")), h4(textOutput("d1b"))),
      ),
      fluidRow(
        column(3, h3("Класс 2", style = "color: green")),
        column(1, h4(textOutput("m2a")), h4(textOutput("m2b"))),
        column(1, ""),
        column(1, h4(textOutput("d2a")), h4(textOutput("d2b"))),
      ),
    )
  )
)

getM <- function(xs) {
  l <- dim(xs)[2]
  res <- matrix(NA, 1, l)
  for (i in seq(l)) {
    res[1, i] <- mean(xs[,i])
  }
  return(c(res))
}

getD <- function(xs, mu) {
  rows <- dim(xs)[1]
  cols <- dim(xs)[2]
  
  res <- matrix(0, 1, cols)
  for (i in seq(rows)) {
    res <- res + (xs[i,] - mu)^2
  }
  
  return(c(res/(rows-1)))
}

getPyj <- function(x, M, D){
  return( (1/(D*sqrt(2*pi))) * exp(-1 * ((x - M)^2)/(2*D^2)) )
}

naiveBayes <- function(x, M, D, Prob, Prior) {
  res <- log(Prob * Prior)
  l <- length(x)
  
  for (i in seq(l)) {
    p <- getPyj(x[i], M[i], D[i])
    res <- res + log(p)
  }
  
  return(res)
}

server <- function(input, output) {
  output$plot = renderPlot({
    
    n <- input$n
    m <- input$m
    
    sigma1i <- matrix(c(input$sigma1a, 0, 0, input$sigma1b), 2, 2)
    sigma2i <- matrix(c(input$sigma2a, 0, 0, input$sigma2b), 2, 2)
    
    mu1i <- c(input$mu1a, input$mu1b)
    mu2i <- c(input$mu2a, input$mu1b)
    
    xc1 <- mvrnorm(n=n, mu = mu1i, Sigma = sigma1i)
    xc2 <- mvrnorm(n=m, mu = mu2i, Sigma = sigma2i)
    
    plotxmin <- min(xc1[,1], xc2[,1]) - 1
    plotymin <- min(xc1[,2], xc2[,2]) - 1
    plotxmax <- max(xc1[,1], xc2[,1]) + 1
    plotymax <- max(xc1[,2], xc2[,2]) + 1
    
    m1 <- getM(xc1)
    m2 <- getM(xc2)
    d1 <- getD(xc1, m1)
    d2 <- getD(xc2, m2)
    
    l <- max(plotxmax - plotxmin, plotymax - plotymin)
    x <- seq(plotxmin, plotxmax, l/50)
    y <- seq(plotymin, plotymax, l/50)
    
    # set proper plot size
    plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax))
    
    # draw classes
    colors <- c("red", "green")
    points(xc1, pch=21, col=colors[1], bg=colors[1])
    points(xc2, pch=21, col=colors[2], bg=colors[2])
    
    # draw classification map
    for (i in x) {
      for (j in y) {
        res1 <- naiveBayes(c(i, j), m1, d1, 0.5, 1)
        res2 <- naiveBayes(c(i, j), m2, d2, 0.5, 1)
        color <- ifelse(res1 > res2, colors[1], colors[2])
        
        #cat(res1, " ", res2, " ", color, "\n")
        points(i, j, pch = 21, col = color)
      }
    }
    
    # fill table
    output$m1a = renderText(sprintf("%.3f", m1[1]))
    output$m1b = renderText(sprintf("%.3f", m1[2]))
    output$d1a = renderText(sprintf("%.3f", d1[1]))
    output$d1b = renderText(sprintf("%.3f", d1[2]))
    
    output$m2a = renderText(sprintf("%.3f", m2[1]))
    output$m2b = renderText(sprintf("%.3f", m2[1]))
    output$d2a = renderText(sprintf("%.3f", d2[1]))
    output$d2b = renderText(sprintf("%.3f", d2[2]))
  })
}

shinyApp(ui, server)