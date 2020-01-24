enableBookmarking(store = "url")


list.of.packages <- c("shiny", "rpivotTable","shinydashboard","shinythemes","shinyFiles","shinyBS","dplyr","esquisse","jsmodule","GGally","plotly","shinyjs","shinyWidgets","shinyalert","shinyjqui","shinydashboardPlus","ggTimeSeries","profvis","shinycssloaders")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)