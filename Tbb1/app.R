library(shiny)

rv <- reactiveValues(
    data = data.frame(mtcars)
)

selectButtonColumn <- function(df, id, ...) {
    # function to create one action button as string
    f <- function(i) {
        as.character(
            actionButton(
                # The id prefix with index
                paste(id, i, sep="_"),
                label = NULL,
                icon = icon('trash'),
                onclick = 'Shiny.setInputValue(\"deletePressed\", this.id, {priority: "event"})'))
    }

    deleteCol <- unlist(lapply(seq_len(nrow(df)), f))
    
    # Return a data table
    DT::datatable(cbind(delete = deleteCol, df),
                  # Need to disable escaping for html as string to work
                  escape = FALSE,
                  options = list(
                      # Disable sorting for the delete column
                      columnDefs = list(list(targets = 1, sortable = FALSE))
                  ))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    DT::dataTableOutput("mytable")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mytable = DT::renderDataTable({
        selectButtonColumn(rv$data, 'report_button')
    })
    
    
    output$report_button <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.docx",
        content = function(file) {
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            browser()
            # Set up parameters to pass to Rmd document
            params <- list(EndoscopistChooserIn = theSelectedRow)
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv()),
                              
            )
        }
    )
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
