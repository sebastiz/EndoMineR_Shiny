library(shiny)

ui <- fluidPage(
   
   #  title
   titlePanel("How do I store the text for reuse?"),
   
   sidebarLayout(
      sidebarPanel(),

      mainPanel(
        textInput("caption", "", ""),
        actionBttn("myText",label = "Store my text",size="sm")
      )
   )
)

server <- function(input, output) {
   
  observeEvent(input$myText,{

    input$caption #<-Where can I store this for later use in a separate session?
   
    
    
  },ignoreInit = TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)

