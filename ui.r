library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Glomerular Filtration Rate Estimation"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    fileInput("GFRfile", "Input file:"),
    sliderInput("dilution","Dilution:", min=50, max=200, value=100),
  #  sliderInput("trhold","Outlier threshold:", min=1, max=10, value=5),
    helpText("Input file should a XLSX file with 4-5 columns and no header:
              'Animal' (optional), 'Time', 'F1',
              'F2' and 'F3' (other columns will be ignored)."),
    br(),
    div("GFRcalc 0.9.1, powered by R/Shiny, developed by Petr Simecek, source code on ",
        a("Github", href="https://github.com/simecek/GFRcalc"))
    
  ),
  
  mainPanel(
    textOutput("check"),
    tableOutput('contents'),
    uiOutput("plots")
  )
 )
)