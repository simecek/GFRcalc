library(shiny)
library(readxl)
library(ggplot2)

source("helpers.R")
options(stringsAsFactors = FALSE)
max_plots <- 16

shinyServer(function(input, output) {
  
  # read data file 
  file.upload <- reactive({
    inFile <- input$GFRfile
    if (!is.null(inFile)) { # if any file name is given
      ext <- tools::file_ext(inFile$name)
      file.rename(inFile$datapath, paste(inFile$datapath, ext, sep=".")) # dirty hack, see issue 85
      
      output <- read_excel(paste(inFile$datapath, ext, sep="."), sheet=2, col_names = FALSE)
      output <- output[!is.na(output[,1]),] # remove NA rows
      if (!is.character(output[,1])) { # if animal IDs missing, add them
        animalID <- rep(LETTERS, each=length(unique(output[,1])))[1:nrow(output)]
        output <- cbind(animalID, output)
      }
      output <- output[,1:5] # only 5 columns matters
      names(output) <- c("Animal", "Time", "M1", "M2", "M3")
      
      # adding information about animals - should be on sheet 3
      animal.table <- read_excel(paste(inFile$datapath, ext, sep="."), sheet=3, col_names = FALSE)[2:4,-1]
      attr(output, "animals") <- t(animal.table)
      print(attr(output, "animals"))
      output
    } else {
      NULL
    }  
  })
  
  output$check <- renderText({dt <- file.upload(); check.format(dt)})
  
  output$contents <- renderTable({
    
    dt <- file.upload()
    
    if (!is.null(dt) & check.format(dt)=="") { # everything ok
    
      # info about animals given? if not add them
      
      animals <- unique(dt$Animal)
      animal.table <- attr(dt, "animals")
      results <- NULL

      for (a in animals) {
  
        tmp <- subset(dt, Animal==a)
        tmp <- tmp[order(tmp$Time),]
        
        # very rough outlier detection
        tmp[-1,c("M1","M2","M3")][outlier.detect(tmp[-1,], input$trhold)] <- NA
        
        tmp$mean <- rowMeans(tmp[,c("M1","M2","M3")], na.rm=TRUE)
        start <- 2 # skip first observation
        tmp2 <- tmp[start:nrow(tmp),]

        fit1 <- oneexp(y=tmp2$mean,x=tmp2$Time)
        fit2 <- twoexp(y=tmp2$mean,x=tmp2$Time)
        
        inj.volume <- as.numeric(animal.table[animals==a,3])
        
        newrow <- data.frame(Animal = a,
                             MouseId = animal.table[animals==a,1],
                             Weight = as.numeric(animal.table[animals==a,2]),
                             Injected_Volume = inj.volume,
                             GFR1 = input$dilution*inj.volume*tmp$mean[1]*ifelse(is.null(fit1), NA, coef(fit1)[2]/coef(fit1)[1]),
                             GFR2 = input$dilution*inj.volume*tmp$mean[1]*ifelse(is.null(fit2), NA, coef(fit2)[2]*coef(fit2)[4]/(coef(fit2)[1]*coef(fit2)[4]+coef(fit2)[2]*coef(fit2)[3])),
                             nNA = sum(is.na(tmp2[,c("M1","M2","M3")])))
        
        results <- rbind(results, newrow)
        
      }
    
      return(results)
      
    } else {
      return (NULL)
    }
  }, digits=1)
  
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
  