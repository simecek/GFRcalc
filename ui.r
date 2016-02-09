library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Glomerular Filtration Rate Estimation"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    fileInput("GFRfile", "Input file:"),
    sliderInput("dilution","Dilution:", min=50, max=200, value=100),
    helpText("Input file should a XLSX file with 4-5 columns and no header:
              'Animal' (optional), 'Time', 'F1',
             'F2' and 'F3' (other columns will be ignored)."),
    checkboxInput("Animal", "Animal (A-Z)", value=TRUE),
    checkboxInput("MouseId", "Mouse ID", value=TRUE),
    checkboxInput("Weight", "Weight", value=TRUE),
    checkboxInput("InjectedVolume", "Injected volume", value=TRUE),
    checkboxInput("GFR2", "Two-compartment model (2C)", value=TRUE),
    checkboxInput("GFR4", "2 One-compartment models (2xC1)", value=TRUE),
    checkboxInput("GFR1", "One-compartment model (1C)", value=TRUE),
    checkboxInput("GFR3", "Piecewise linear model (PL)", value=TRUE),
    checkboxInput("Sigma.C2", "Square root of the error variance (Sigma.C2)", value=TRUE),
    checkboxInput("nNA", "Number of missing observations (nNA)", value=TRUE),
  #  sliderInput("trhold","Outlier threshold:", min=1, max=10, value=5),
    
    br(),
    div("GFRcalc 0.9.4, powered by R/Shiny, developed by Petr Simecek, source code on ",
        a("Github", href="https://github.com/simecek/GFRcalc"))
    
  ),
  
  mainPanel(
    textOutput("check"),
    tableOutput('contents'),
    uiOutput("plots")
  )
 )
)