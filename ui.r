library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Glomerular Filtration Rate Estimation"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    fileInput("GFRfile", "Input file:"),
    helpText("Input file should be a comma separated file
              including 'Animal' column with animal ids,
              'Time' column with time points 
             and fluorescence columns 'M1', 'M2', 'M3'")
  ),
  
  mainPanel(
   # textOutput("check"),
   #  plotOutput("oneComp", width="800px", height="800px"),
   # plotOutput("twoComp", width="800px", height="800px"),
    tableOutput('contents')
  )
 )
)