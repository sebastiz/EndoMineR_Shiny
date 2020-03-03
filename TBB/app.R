#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("My button issue"),

    # Sidebar with a slider input for number of bins 


            


        # Show a plot of the generated distribution
        mainPanel(
            box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = "A. Upload data",
                fileInput("pathology",label="",multiple = FALSE),br()),
            box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = "B. Upload data",
                fileInput("FileIn_endoscopy",label="",multiple = FALSE),br())

        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observe({
        inFile_path <- input$pathology
        if (!is.null(inFile_path)) {   
            dataFile <- read_excel(inFile_path$datapath, sheet=1)
            dat <- data.frame(EndoPaste(dataFile)[1], stringsAsFactors=FALSE)
            RV2$data<-dat
            enable("textPrepPath")
        }
        else{disable("textPrepPath")}
    })
    
    observe({
        inFile_endoscopy <- input$FileIn_endoscopy
        if (!is.null(inFile_endoscopy)) {   
            dataFile <- read_excel(inFile_endoscopy$datapath, sheet=1)
            dat <- data.frame(EndoPaste(dataFile)[1], stringsAsFactors=FALSE)
            RV$data<-dat
            enable("textPrep")
        }
        else{disable("textPrep")}
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
