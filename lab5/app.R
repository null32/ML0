library("shiny")

drawLines <- function(center, cv) {
  det <- det(cv)
  
  if (det <= 0) {
    plot.new()
    return("Определитель меньше 0")
  }
  
  a <- cv[2, 2] / det
  b <- -cv[1, 2] / det
  c <- -cv[2, 1] / det
  d <- cv[1, 1] / det
  
  x0 <- center[1]
  y0 <- center[2]
  
  A <- d
  B <- a
  C <- -c -b
  D <- -2*d*x0 + y0*(c+b)
  E <- -2*a*y0 + x0*(c+b)
  F <- d*x0^2 + a*y0^2 + x0*y0*(-c-b)
  
  func <- function(x, y) {
    1 / (2*pi*sqrt(det)) * exp(-0.5 * (x^2*A + y^2*B + x*y*C + x*D + y*E + F))
  }
  
  X <- seq(-2-0.1, 2+0.1, 0.1)
  Y <- seq(-2-0.1, 2+0.1, 0.1)
  Z <- outer(X, Y, func)
  
  contour(X, Y, Z)
}


ui <- fluidPage(
  titlePanel("Линии уровня"),
  sidebarLayout(
    # input
    sidebarPanel(
      sliderInput(
        inputId = "a",
        label = "Элемент [1, 1]",
        min = 0,
        max = 1,
        value = 1
      ),
      sliderInput(
        inputId = "b",
        label = "Элемент [1, 2] и [2, 1]",
        min = 0,
        max = 1,
        value = 0
      ),
      sliderInput(
        inputId = "d",
        label = "Элемент [2, 2]",
        min = 0,
        max = 1,
        value = 1
      )
    ),
    # output
    mainPanel(
      textOutput(outputId = "errMsg"),
      plotOutput(
        outputId = "plot"
      )
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    par(pty="s")
    m <- matrix(c(input$a, input$b, input$b, input$d), 2, 2)
    
    err <- drawLines(c(0, 0), m)
    if (!is.null(err)) {
      output$errMsg <- renderText({
        err
      })
    }
    else
    {
      output$errMsg <- renderText({
        ""
      })
    }
  })
}

shinyApp(ui, server)