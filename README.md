# EndoMineR_Shiny
Shiny app for the {EndoMineR library}(https://docs.ropensci.org/EndoMineR/articles/EndoMineR.html). Very much work in progress. If you want to give it a go, you should do the following:

Install dependencies (for ubuntu/ mac)
1. sudo apt-get install libfontconfig1-dev
2. sudo apt-get install libcairo2-dev 
3. sudo apt-get install libxt-dev

Once these are installed you can run:

list.of.packages <- c("shiny", "rpivotTable","shinydashboard","shinythemes","shinyFiles","shinyBS","dplyr","esquisse","jsmodule","GGally","plotly","shinyjs","shinyWidgets","shinyalert","shinyjqui","shinydashboardPlus","ggTimeSeries","profvis","shinycssloaders")


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]


if(length(new.packages)) install.packages(new.packages)


shiny::runGitHub('EndoMineR_Shiny','sebastiz')
