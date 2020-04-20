library(shiny)

fun2 <- function(i,n,dat) {
  dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
  incProgress(1/n, detail = paste("Doing part", i))
  Sys.sleep(0.1)
  return(dat)
}

fun1 <- function(n) {
  dat <- data.frame(x = numeric(0), y = numeric(0))
  for (i in 1:n) { dat <- fun2(i,n,dat) }
  return(dat)
}

server <- function(input, output) {
  output$plot <- renderPlot({
    input$goPlot # Re-run when button is clicked
    withProgress(message = 'Making plot', value = 0, {
      n <- 10
      dat <- fun1(n)
    })
    plot(dat$x, dat$y)
  })
}

ui <- shinyUI(basicPage(
  plotOutput('plot', width = "300px", height = "300px"),
  actionButton('goPlot', 'Go plot')
))

shinyApp(ui = ui, server = server)