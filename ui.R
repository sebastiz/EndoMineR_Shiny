library(shiny)
library(rpivotTable)
library(EndoMineR)
library(shinydashboard)
library(shinythemes)
library(shinyFiles)
library(shinyBS)
library(dplyr)
library(esquisse)
library(jsmodule)
library(GGally)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(shinyalert)


# Module UI function
textPrepUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    textInput(ns("caption"), "", "Enter the comma separated headers here"),
    actionBttn(ns("textPrep1"),label = "SplitMe",size="sm")
    
    
    #bsPopover ("textPrep", "Split the text: Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
    #options = NULL)
  )
}







ui <-function(request) {fluidPage(theme=shinytheme("simplex"),
                                  useShinyjs(),
                                  themeSelector(),  
                                  useShinyalert(), 

            tags$script(HTML('$(document).on("click", "input", function () {
                       var checkboxes = document.getElementsByName("row_selected");
                       var checkboxesChecked = [];
                       for (var i=0; i<checkboxes.length; i++) {
                       if (checkboxes[i].checked) {
                       checkboxesChecked.push(checkboxes[i].value);
                       }
                       }
                       Shiny.onInputChange("checked_rows",checkboxesChecked);
                       })')),
      tags$script("$(document).on('click', '#Main_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random())
                  });"),
      tags$style("html, body {overflow: visible !important;.rpivotTable{ overflow-x: scroll; }"),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tags$style(".dataTables_scrollBody {
        transform:rotateX(180deg);
      }
      .dataTables_scrollBody table {
        transform:rotateX(180deg);
      }"),
      

dashboardPage(
  
  dashboardHeader(title = 'EndoMineR',
                  tags$li( class="dropdown",a(href = 'https://ropensci.github.io/EndoMineR/articles/EndoMineRPrinciples.html',
                            icon("fa-li fa-bullseye"))),
                  tags$li(class="dropdown",a(href = 'https://twitter.com/GastroDS',
                            icon("fa-li fa fa-twitter"))),
                  tags$li(class="dropdown",a(href = 'https://github.com/ropensci/EndoMineR',
                            icon("fa-li fa fa-github fa-lg"))),
                  tags$li(class="dropdown",a(href = 'https://sebastiz.github.io/gastrodatascience/',
                            icon("fa-li fa fa-book"))),
                  tags$li(class="dropdown",a(href = ' https://sebastiz.github.io/gastroDS3',
                                             icon("fa-li fa fa-book")))
                  
                 
                                          
  ),
 
   dashboardSidebar(collapsed = TRUE
   ),

 
  dashboardBody(

    tabsetPanel(type = "tabs",
                tabPanel("Clean and Merge", verbatimTextOutput("summary"),
                         # Dataset 1 ----------------------------------------------------

    bsCollapse(id = "collapseExample", open = "Panel 1",
               bsCollapsePanel("Dataset 1 (eg. Endoscopy results)", "",
                               #########]]]]] File upload- endotable#############
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = "A. Upload data",
                                   fileInput("FileIn_endoscopy",label="",multiple = FALSE),br()),
                               
                               
                               
                               
                    
                               
                               
                               
                               
                               
                               ###########]]]]] Endotable events-textPrep ############                               
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = "B. Split the data",
                               textInput("caption", "", "Enter the comma separated headers here"),
                               actionBttn("textPrep",label = "Split",size="sm"),
                               
                               #textPrepUI("textPrep1"),
                                bsPopover ("textPrep", "Split the text: Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               HTML('&nbsp;'),
                               
                               ###########]]]]] Endotable events-Date standardiser ###########
                              
                               actionBttn("DateStandardiserEndo",label = "", icon = icon("far fa-calendar-alt"),size="sm"),
                               bsPopover ("DateStandardiserEndo", "Data standardiser: Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                          options = NULL),
                               HTML('&nbsp;'),
                               
                               ###########]]]]] Endotable events-HospNum standardiser ###########
                               actionBttn("HospitalNumberExtractorEndo",label = "", icon = icon("fas fa-barcode"),size="sm"),
                               bsPopover ("HospitalNumberExtractorEndo", "Hospital Number Extractor: Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                          options = NULL)),
                               

                               
                               #########]]]]] Table Create- endotable#############
                               DT::dataTableOutput("endotable")),

# Dataset 2 ----------------------------------------------------              
               
                              
               bsCollapsePanel("Dataset 2 (eg. Pathology results)", "", 
                               fluidRow(
                                 
                                 #########]]]]] File upload- pathtable#############
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = "A. Upload data",
                                   fileInput("pathology",label="",multiple = FALSE),br()),

                               ###########]]]]] pathTable events- textPrep ############  
                               box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = "B. Split data",
                               textInput("captionPath", "", "Enter the comma separated headers here"),
                               actionBttn("textPrepPath",label = "Split",size="sm"),
                                bsPopover ("textPrepPath", "Split the data: Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                         options = NULL),
                               HTML('&nbsp;'),
                               
                               ###########]]]]] pathTable events-Date standardiser ###########
                               actionBttn("DateStandardiserEPath",label = "", icon = icon("far fa-calendar-alt"),size="sm"),
                               bsPopover ("DateStandardiserEPath", "Date Standardiser: Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                          options = NULL),
                               HTML('&nbsp;'),
                               
                               ###########]]]]] pathTable events-HospitalNumber standardiser ###########
                               actionBttn("HospitalNumberExtractorPath",label = "", icon = icon("fas fa-barcode"),size="sm"),
                               bsPopover ("HospitalNumberExtractorPath", "Hospital Number Selector: Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                          options = NULL))),

                               #########]]]]] Table Create- pathTable#############
                               DT::dataTableOutput("pathTable")),

# Final Dataset  ----------------------------------------------------
#########]]]]] Button - Text dataset merge #############
               bsCollapsePanel("Final Dataset (to merge dataset 1 & 2 or upload a pre-merged dataset)", "", 
                               fluidRow(column(4),
                                        column(4,
                               actionBttn(
                                 inputId = "Endomerge2Me",
                                 label = "Text Dataset Merge",
                                 size="lg",
                                 style = "simple",
                                 block = TRUE
                               )),
                               bsModal("Endomerge2_modal", "Merge it", "Endomerge2Me", size = "small",
                                      
                                      uiOutput("DS1_DateChooser"),
                                      uiOutput("DS1_HospNumChooser"),
                                      uiOutput("DS2_DateChooser"),
                                      uiOutput("DS2_HospNumChooser"),
                                       footer = tagList(
                                         modalButton("Cancel"),
                                         actionBttn("Endomerge2", "OK",size="sm")
                                       )
                               ),
                               actionBttn("TermMapping",label = "Map your terms",size="sm"),
                               bsModal("TermMapping_Modal", "Merge it", "TermMapping", size = "large",
                                       fluidRow(column(6,uiOutput("Map_HospitalNumber"),
                                       uiOutput("Map_Endoscopist"),
                                       uiOutput("Map_ProcedurePerformed"),
                                       uiOutput("Map_EndoscopyDate")),
                                       column(6,uiOutput("Map_Findings"),
                                       uiOutput("Map_Findings2"),
                                       uiOutput("Map_Events"),
                                       uiOutput("Map_MacroscopicText"),
                                       uiOutput("Map_MicroscopicText"))),
                                       
                                       footer = tagList(
                                         modalButton("Cancel"),
                                       actionBttn("MapMe", "OK",size="sm")
                                       )
                               )
                               
                               ),
                               br(),
                               
                               fluidRow(column(5),column(4,
                                 ############]]]]]  BOX ############
                                 #########]]]]] File upload- mergedtable#############
                            box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=TRUE,title = "Optional",

                                     fileInput("inFile_merged",label="",multiple = FALSE),
                                
                                
                                
                                 ############]]]]]  BOX ############
                                 ###########]]]]]  Button -textPrep ###########
                                 
                                     textInput("captionMerge", "", "Enter the comma separated headers here"),
                                     actionBttn("textPrepMerge",label = "Split",size="sm"),
                                      bsPopover  ("textPrepMerge", "Split the data: Enter the headers from the text separated by a comma to split according to the headers", placement = "bottom", trigger = "hover",
                                               options = NULL),

                                     ############]]]]]  Button - Date standardiser############
                                     actionBttn("DateStandardiserMerge",label = "", icon = icon("far fa-calendar-alt"),size="sm"),
                                     bsPopover ("DateStandardiserMerge", "Date Standardiser: Select only one date column then press the button", placement = "bottom", trigger = "hover",
                                                options = NULL),

                                     ############]]]]]  Button - Hospital standardiser############
                                actionBttn("HospitalNumberExtractorMerge",label = "", icon = icon("fas fa-barcode"),size="sm"),
                                     bsPopover ("HospitalNumberExtractorMerge", "Hospital Number Selector: Select only one hospital number column then press the button to standardise", placement = "bottom", trigger = "hover",
                                                options = NULL))),

                            
                            
                            bsPopover ("Endomerge2", "Text Merge: Make sure you have standarised both the date and hospital column in both the endoscopy and the pathology datasets, then press this button to get the datasets merged.", placement = "bottom", trigger = "hover",options = NULL),

                                 ############]]]]]  BOX ############
                                 
                                 ############]]]]]  Button - EndoMerge############
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = " Clean columns and rows",width = 6,br(), br(),
                                     ############]]]]]  Button - Categorical standardiser############
                                     actionBttn("CategoricalDataMerge",label = "", icon = icon("far fa-flushed"),size="sm"),
                                     bsPopover ("CategoricalDataMerge", "Categorical standardiser: Select only one categorical column then press the button to standardise", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                    #textOutput('CategoricalDataMerge_Validation'),
                                     
                                     
                                     
                                     ############]]]]]  Button - Numeric standardiser############
                                     
                                     HTML('&nbsp;'),
                                     actionBttn("NumericDataMerge",label = "NUM",size="sm"),
                                     bsPopover ("NumericDataMerge", "Numeric column standardiser: Select only one numeric column then press the button to standardise", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     
                                     ############]]]]]  Button - Negex############
                                     HTML('&nbsp;'),
                                     actionBttn("NegExMerge",label = "Negex",size="sm"),
                                     bsPopover ("NegExMerge", "Negative phrase deletion: Select only one text column to exclude all sentences with negative expressions", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     HTML('&nbsp;'),
                                    
                                    ############]]]]]  Button - Image Merge############
                                     actionBttn("MergeWithImages",label = "Image Merge",size="sm"),
                                     bsPopover ("MergeWithImages", "Image Merge: Press here to merge with images. The images must be from a html export with hospital numbers and dates so they can be merged.", placement = "bottom", trigger = "hover",options = NULL),
                                     
                                     bsModal("modalExampleImages", "Image Merge menu", "MergeWithImages", size = "size",
                                             shinyFilesButton("Btn_GetFileImage", "Choose the html file with the endoscopy details and pictures" ,
                                                              title = "Please select a file:", multiple = FALSE, buttonType = "default", class = NULL),
                                             br(),br(), br(),
                                             textOutput("txt_file"),
                                             br(), br(),br(), br(),
                                             
                                             textInput("captionDelim", "Which words or phrases in the html separates the procedures (eg 'Procedure Number:)", "delimiting word or phrase",width="45%"),
                                             uiOutput("ImageMerge_DelimTextPickers"),
                                             uiOutput("ImageMerge_DateChooser"),
                                             uiOutput("ImageMerge_HospNumChooser"),
                                             br(), br(), br(),
                                             shinyDirButton('folder', 'Choose folder containing all the image', 'Please select a folder containing all the images (usually in the same parent folder as the html report)', FALSE),
                                             br(),br(), br(),
                                             textOutput("folder_file"),
                                             br(), br(),br(), br(),
                                             actionBttn("MergeImages",label = "Merge the images with your dataset",size="sm")),
                                     HTML('&nbsp;'),
                                     actionBttn(inputId = "Del_row_head",label = "Delete selected rows",size="sm"),
                                     bsPopover ("Del_row_head", "Row deletion: Select individual rows with the checkbox and then press here to delete from the dataset", placement = "bottom", trigger = "hover",options = NULL),
                                    
                                    ############]]]]]  Button- Remove Duplicates############
                                    actionBttn("RemoveDuplicates",label = "DUP",icon = icon("faa fa-twins"),size="sm"),
                                    bsPopover ("RemoveDuplicates", "Duplicate report removal: Press to remove rows where one procedure has pathology reported for two procedures (eg merged pathology report when OGD and colonoscopy performed", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     bsModal("Rem_DupModal", "Remove Duplicates", "RemoveDuplicates", size = "small",
                                             uiOutput("ProcPerf_RemDepsChooser"),
                                             uiOutput("LexiconChecker_RemDupsChooser"),
                                             footer = tagList(
                                               modalButton("Cancel"),
                                               actionBttn("RemovDupsModal_Okbtn", "OK",size="sm")
                                             )
                                     )
                                 ),
                                 


                                 ############]]]]]  BOX ############
                                 ############]]]]]  Button - Biopsy Number standardiser############
                                 box(status = "primary", solidHeader = TRUE,collapsible = T,collapsed=FALSE,title = "Derive new columns",br(), br(),
                                     actionBttn("NumBxMerge",label = "",icon = icon("fas fa-microscope"),size="sm"),
                                      bsPopover ("NumBxMerge", "Number of biopsies: Select column (usually a macroscopic description column from pathology) to extract the total number of biopsies", placement = "bottom", trigger = "hover",
                                               options = NULL),

                                     bsModal("NumBxModal", "Get the number of biopsies taken", "NumBxMerge", size = "small",
                                             textInput("new_name", "Enter the delimiter here:", "") ,

                                             footer = tagList(
                                               modalButton("Cancel"),
                                               actionBttn("NumBxModal_ok", "OK",size="sm")
                                             )
                                     ),
                                  
                                     ############]]]]]  Button - Biopsy size standardiser############
                                     HTML('&nbsp;'),
                                     actionBttn("BxSizeMerge",label = "",icon = icon("fas fa-sort-numeric-up"),size="sm"),
                                      bsPopover ("BxSizeMerge", "Size of biopsies: Select column (usually a macroscopic description column from pathology) to extract the average biopsy size ", placement = "bottom", trigger = "hover",
                                               options = NULL),
                                     HTML('&nbsp;'),

                                     ############]]]]]  Button - Endoscopist standardiser############
                                     actionBttn("EndoscEndoscopistMerge",label = "", icon = icon("user-md custom"),size="sm"),
                                      bsPopover ("EndoscEndoscopistMerge", "Endoscopist name standardiser: Standardise the endoscopist column", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     HTML('&nbsp;'),
                                     
                                     ############]]]]]  Button - Medication standardiser############
                                     actionBttn("EndoscMedsMerge",label = "",icon = icon("fas fa-pills"),size="sm"),
                                      bsPopover ("EndoscMedsMerge", "Medication extraction: Select the medication column to extract medications", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     HTML('&nbsp;'),
                                     
                                     ############]]]]]  Button - Instrument standardiser############
                                     actionBttn("EndoscInstrumentMerge",label = "q",icon = icon("stethoscope custom"),size="sm"),
                                      bsPopover ("EndoscInstrumentMerge", "Instrument extraction: Select the Instrument column to clean instrument names", placement = "bottom", trigger = "hover",
                                                   options = NULL),
                                     HTML('&nbsp;'),
                                     
                                     ############]]]]]  Button - EndoEvent standardiser############
                                     actionBttn("EndoEvent",label = "Events",icon = icon("fas fa-sort-numeric-up"),size="sm"),
                                     bsPopover ("EndoEvent", "Endoscopic Event extraction: Select in order: \nEndoscopic Findings, ProcedurePerformed, Macroscopicdescription and Histology text", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     HTML('&nbsp;'),
                                     
                                     
                                     ############]]]]]  Button - EndoEvent column select for modal ############
                                     bsModal("EndoEventModal", "Get the events that happened at endoscopy", "EndoEvent", size = "small",
                                             uiOutput("EndoEventColSelect_colEndoFindings"),
                                             uiOutput("EndoEventColSelect_colProcPerf"),
                                             uiOutput("EndoEventColSelect_colMacroDescript"),
                                             uiOutput("EndoEventColSelect_colHistol"),
                                             footer = tagList(
                                               modalButton("Cancel"),
                                               uiOutput("EndoEventcol_sel"),
                                               actionBttn("EndoEventModalbtn", "OK")
                                             )
                                     ),
                                     
                                     ############]]]]]  Button - Regex############
                                     actionBttn("Regex",label = "",icon = icon("fas fa-sort-numeric-up"),size="sm"),
                                     bsPopover ("Regex", "Custom regular expression: Put in a regular expression or a keyowrd to derive a new column with those elements extracted so you can filter on them", placement = "bottom", trigger = "hover",
                                                options = NULL),
                                     bsModal("RegexColAdder", "Add a column defined by a regular expression", "Regex", size = "small",
                                             textInput("regexSearch", "Enter the search term here", "") ,
                                             footer = tagList(
                                               modalButton("Cancel"),
                                               actionBttn("regexSearch_ok", "OK",size="sm")
                                             )
                                     ),
                                     HTML('&nbsp;')
                                  
                                 ),



                               
                               #########]]]]] Table Create- mergedtable#############
                               DT::dataTableOutput("mergedTable"))
               )

               ),
bookmarkButton()),






         
         
         
         
# Barrett's ----------------------------------------------------  


    tabPanel("Barrett's", tableOutput("table5"),
          mainPanel(width = 100,
                    navbarPage("",

                        tabPanel("Quality Metrics",
                          box(collapsible = T,collapsed=FALSE, title = "Results", status = "warning", solidHeader = TRUE,
                                                     ############]]]]]  BarrettsTable Plot Quality Endoscopist vs Worst grade Plot  ############
                            plotlyOutput("plotBarrQM"),
                            ############]]]]]  BarrettsTable Plot-Quality Documentation quality Plot ############
                            plotlyOutput("plotBarrEQ")),
                            #########]]]]]  Drilldown Table- BarrettsTable############# 
                              box(collapsible = T,collapsed=FALSE,
                              title = "Endoscopy Utilisation", status = "warning", solidHeader = TRUE,
                              plotlyOutput("endoscopyUse_EndoscopyUseBarr"),
                              
                              ############]]]]]  BarrettsTable Plot-Time Series Analysis Plot ############
                              plotlyOutput("plotBarrTSA")),
                          
                          box(collapsible = T,collapsed=FALSE,
                              title = "Results Table", status = "warning", solidHeader = TRUE,
                              DT::dataTableOutput("BarrDDR_Table")),
                          
                          #########]]]]]  Drilldown Table- polypTable############# 
                          
                          box(collapsible = T,collapsed=FALSE,
                              title = "Results Drill down Table", status = "warning", solidHeader = TRUE,
                              DT::dataTableOutput("drilldownBarr"))
                              ),
                        
                        
                        tabPanel("Raw Data",
                                 #########]]]]] Table Create- BarrettsTable#############  
                                 DT::dataTableOutput("BarrettsTable")
                        ),
                        
                        ############]]]]]  BarrTable Vislualisation Esquiss ############
                        tabPanel("Visualise",
                                 tags$div(
                                   style = "height: 700px;", # needs to be in fixed height container
                                   esquisserUI(
                                     id = "esquisseBarr",
                                     header = FALSE, # dont display gadget title
                                     choose_data = FALSE # dont display button to change data
                                   )
                                 )),
                        
                        ############]]]]]  BarrettsTable CrossTablulate  ############
                        tabPanel("Cross Tabulate", style="overflow: visible",
                                 fluidRow(rpivotTableOutput("BarrPivot"))),
                        

                        tabPanel("Theograph", sidebarPanel(width = 2,
                          # Select variable for the hospital number
                          
                          ############]]]]]  BarrettsTable Chooser-Theograph Endoscopist  ############
                          uiOutput("HospNumBarrTheo"),
                          # Select variable for the dates
                          
                          ############]]]]]  BarrettsTable Chooser-Theograph Date  ############
                          uiOutput("DatesBarrTheo")
                          
                          ############]]]]]  BarrettsTable Plot-Theograph Plot ############
                        ),mainPanel(plotlyOutput("plotBarrPT")))
                    )
            )
          ),

# Polyps  ----------------------------------------------------
    tabPanel("Polyps", tableOutput("table3"),

        mainPanel(width = 100,
          
          navbarPage("",
                      tabPanel("Quality Metrics", 
                               ############]]]]]  GRSTable Create ############
                               
                               box(collapsible = T,collapsed=FALSE,
                                   title = "Results", status = "warning", solidHeader = TRUE,
                                   plotlyOutput("plotPolypEQ")),
                               
                               box(collapsible = T,collapsed=FALSE,
                                   title = "Endoscopy Utilisation", status = "warning", solidHeader = TRUE,
                                   ############]]]]]  PolypTable Plot- EndoUtilisation Plot ############
                                   plotlyOutput("endoscopyUse_EndoscopyUsePolyp")
                               ),
                               
                               box(collapsible = T,collapsed=FALSE,
                                   title = "Results Table", status = "warning", solidHeader = TRUE,
                                   DT::dataTableOutput("GRS_Table")),
                               
                               #########]]]]]  Drilldown Table- polypTable############# 
                               
                               box(collapsible = T,collapsed=FALSE,
                                   title = "Results Drill down Table", status = "warning", solidHeader = TRUE,
                                   DT::dataTableOutput("drilldown"))
                     ),
                     tabPanel("Raw Data",
                              DT::dataTableOutput("polypTable")),
                     tabPanel("Visualise",
                              tags$div(
                                style = "height: 700px;", # needs to be in fixed height container
                                esquisserUI(
                                  id = "esquissePolyp",
                                  header = FALSE, # dont display gadget title
                                  choose_data = FALSE # dont display button to change data
                                )
                              )),

                     tabPanel("Cross Tabulate", style="overflow: visible",
                              ############]]]]]  PolypTable CrossTabulate ############
                              fluidRow(rpivotTableOutput("OverallPivotPolyp"))),
                      tabPanel("Theograph", plotOutput("plotPolypPF"))
          )
        )
    ),

# IBD  ----------------------------------------------------
    tabPanel("IBD", tableOutput("tableIBD"),
             
        mainPanel(width = 100,
          navbarPage("",
                      tabPanel("Quality Metrics", plotOutput("plotIBDQM")),
                     tabPanel("Raw Data", 
                              DT::dataTableOutput("IBD")),
                     tabPanel("Visualise",
                              radioButtons(
                                inputId = "dataIBD",
                                label = "Data to use:",
                                choices = c("Custom"),
                                inline = TRUE
                              ),
                              tags$div(
                                style = "height: 700px;", # needs to be in fixed height container
                                esquisserUI(
                                  id = "esquisseIBD",
                                  header = FALSE, # dont display gadget title
                                  choose_data = FALSE # dont display button to change data
                                )
                              )),

                     tabPanel("Cross Tabulate", style="overflow: visible",
                              fluidRow(rpivotTableOutput("OverallPivotIBD"))),
                     tabPanel("Endoscopy Utilisation", style="overflow: visible",
                              uiOutput("Date_endoscopyutilisationIBD"),
                              fluidRow(plotlyOutput("endoscopyUse_TimeSeriesIBD"))),
                      tabPanel("Theograph", plotOutput("plotIBDTheo")),
                      tabPanel("Time Series Analysis", plotOutput("plotIBDTSA"))
          )
 
        )
    ),

# Per endoscopist ----------------------------------------------------  

tabPanel("Per Endoscopist Report",
         mainPanel(width = 100,
                   navbarPage("",
                              box(collapsible = T,collapsed=FALSE,
                                title = "Results", status = "warning", solidHeader = TRUE, DT::dataTableOutput("performanceTable"),
                                "Box content here", br(), "More box content"),
                              
                              box(collapsible = T,collapsed=FALSE,
                                title = "Biopsies By Indication", status = "warning", solidHeader = TRUE,
                                plotlyOutput("IndicsVsBiopsies"),                        
                                "Box content here", br(), "More box content"),
                              
                              box(collapsible = T,collapsed=FALSE,
                                title = "GRS", status = "warning", solidHeader = TRUE,DT::dataTableOutput("GRS_perEndoscopist_Table"),
                                "Box content here", br(), "More box content"),
                              
                              box(collapsible = T,collapsed=FALSE,
                                title = "Barrett's", status = "warning", solidHeader = TRUE,
                                ############]]]]]  BarrettsTable Plot Quality Endoscopist vs Worst grade Plot  ############
                                plotlyOutput("plotBarrQM_Perform"),
                                ############]]]]]  BarrettsTable Plot-Quality Documentation quality Plot ############
                                plotlyOutput("plotBarrEQ_Perform"),
                                "Box content here", br(), "More box content")
                              
                ))
         ),
# Custom ----------------------------------------------------  


tabPanel("Custom", tableOutput("table53"),
         
         #########]]]]]  Table Create- CustomTable############# 

         mainPanel(width = 100,
                   navbarPage("",
                              tabPanel("Data", DT::dataTableOutput("CustomTable")),
                              ############]]]]]  CustomTable Visualisation Esquiss ############
                              tabPanel("Visualise",
                                       tags$div(
                                         style = "height: 700px;", # needs to be in fixed height container
                                         esquisserUI(
                                           id = "esquisseCustom",
                                           header = FALSE, # dont display gadget title
                                           choose_data = FALSE # dont display button to change data
                                         )
                                       )),
                              
                              ############]]]]]  CustomTable CrossTabulate############
                              tabPanel("Cross Tabulate", style="overflow: visible",
                                       fluidRow(rpivotTableOutput("OverallPivot"))),
                              tabPanel("Endoscopy Utilisation", style="overflow: visible",
                                       sidebarPanel(
                                         
                                         ############]]]]]  CustomTable Chooser - EndoUtilisation Date  ############
                                         uiOutput("Date_endoscopyutilisationCustom"),
                                         
                                         ############]]]]]  CustomTable Chooser - EndoUtilisation Event  ############
                                         uiOutput("endoscopicEventCustom"),width = 2),
                                       
                                       ############]]]]]  CustomTable Plot EndoUtilisation   ############
                                       mainPanel(verticalLayout(plotlyOutput("endoscopyUse_EndoscopyUseCustom"),
                                                                
                                                                ############]]]]]  CustomTable Plot- TimeSeriesAnalysis ############
                                                                plotlyOutput("plotCustomTSA"))
                                       )),
                              tabPanel("Theograph", sidebarPanel(width = 2,
                                                                 # Select variable for the hospital number
                                                                 
                                                                 ############]]]]]  CustomTable Chooser - Theograph HospNum  ############
                                                                 uiOutput("HospNumCustomTheo"),
                                                                 
                                                                 ############]]]]]  CustomTable Chooser - Theograph Date  ############
                                                                 # Select variable for the dates
                                                                 uiOutput("DatesCustomTheo")
                              ),mainPanel(plotlyOutput("plotCustomPT")))
                   )
         )
         
)
    )
  )
)
)}