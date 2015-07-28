require(shiny)
require(readxl)
require(ggplot2)

source("helpers.R")
options(stringsAsFactors = FALSE)
max_plots <- 10

shinyServer(function(input, output) {
  
  # read data file 
  file.upload <- reactive({
    inFile <- input$GFRfile
    if (!is.null(inFile)) {
      file.rename(inFile$datapath, paste(inFile$datapath, "xlsx", sep=".")) # dirty hack, see issue 85
      tmp <- read_excel(paste(inFile$datapath, "xlsx", sep="."), sheet=2, col_names = FALSE)
      tmp <- tmp[!is.na(tmp[,1]),] # remove NA rows
      if (!is.character(tmp[,1])) { # if animal IDs missing, add them
        animalID <- rep(LETTERS, each=length(unique(tmp[,1])))[1:nrow(tmp)]
        tmp <- cbind(animalID, tmp)
      }
      tmp <- tmp[,1:5] # only 5 columns matters
      names(tmp) <- c("Animal", "Time", "M1", "M2", "M3")
      tmp
    } else {
      NULL
    }  
  })
  
  output$check <- renderText({dt <- file.upload(); check.format(dt)})
  
  output$contents <- renderTable({
    
    dt <- file.upload()
    
    if (!is.null(dt) & check.format(dt)=="") { # everything ok
    
      # are animals given? if not add them
      
      animals <- unique(dt$Animal)
      results <- NULL

      for (a in animals) {
  
        tmp <- subset(dt, Animal==a)
        tmp <- tmp[order(tmp$Time),]
        
        tmp$mean <- rowMeans(tmp[,c("M1","M2","M3")], na.rm=TRUE)
        start <- 2 # skip first observation
        tmp2 <- tmp[start:nrow(tmp),]

        fit1 <- oneexp(y=tmp2$mean,x=tmp2$Time)
        fit2 <- twoexp(y=tmp2$mean,x=tmp2$Time)
        
        newrow <- data.frame(Animal = a, 
                             GFR1 = tmp$mean[1]*ifelse(is.null(fit1), NA, coef(fit1)[2]/coef(fit1)[1]),
                             GFR2 = tmp$mean[1]*ifelse(is.null(fit2), NA, coef(fit2)[2]*coef(fit2)[4]/(coef(fit2)[1]*coef(fit2)[4]+coef(fit2)[2]*coef(fit2)[3])))
        
        results <- rbind(results, newrow)
        
      }
    
      return(results)
      
    } else {
      return (NULL)
    }
  }, digits=4)
  
  output$plots <- renderUI({
    
    dt <- file.upload()
    
    if (!is.null(dt) & check.format(dt)=="") { # everything ok
      plot_output_list <- lapply(1:length(unique(dt[,1])), function(i) {
        plotname <- paste0("plot", i)
        plotOutput(plotname, height = 300, width = 400)
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    }  
  })
  
  for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste0("plot", my_i)
      output[[plotname]] <- renderPlot({make.plot(file.upload(), my_i)})
    })
  }  
  
})
  