library(shiny)
library(MASS)
library(kernlab)
library(ROCR)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well {
        padding: 1rem;
      }
    ")
    )
  ),
  
  titlePanel("SVM & ROC Curve"),
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
      tags$h2("ROC Curve", style = "color: green"),
      plotOutput(
        outputId = "c1",
        height =  "500px",
      ),
    )
  )
)

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
    # classes
    dat <- cbind(dat, c(rep(-1, n), rep(1, m)))
    
    plotxmin <- min(dat[,1], dat[,1]) - 0.3
    plotxmax <- max(dat[,1], dat[,1]) + 0.3
    plotymin <- min(dat[,2], dat[,2]) - 0.5
    plotymax <- max(dat[,2], dat[,2]) + 0.5
    plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="ADALINE & HEBB")
    
    points(dat, pch=21, col=colors[ifelse(dat[,3] == -1, 1, 2)], bg=colors[ifelse(dat[,3] == -1, 1, 2)])
    
    svp <- ksvm(dat[,1:2], dat[,3], type = "C-svc", kernel = "vanilladot", C=100, scaled=c())
    
    w<-colSums(svp@coef[[1]] * dat[svp@SVindex,][,1:2])
    b<-svp@b
    
    abline(b/w[2], -w[1]/w[2], lwd=2)
    abline((b-1)/w[2], -w[1]/w[2])
    abline((b+1)/w[2], -w[1]/w[2])
    
    ypredscore <- predict(svp, dat[,1:2], type = "decision")
    pred <- prediction(ypredscore, dat[,3])
    perf <- performance(pred, measure = "tpr", x.measure = "fpr")
    output$c1 = renderPlot({
      plot(perf)
      lines(c(0, 1), c(0, 1), col="red")
    })
  })
}

shinyApp(ui, server)