require(shiny)

source("helpers.R")

shinyServer(function(input, output) {
  output$contents <- renderTable({
  
    inFile <- input$GFRfile
  
    if (is.null(inFile))
      return(NULL)
  
    dt <- read.csv(inFile$datapath, as.is=TRUE)
    animals <- unique(dt$Animal)
    results <- NULL
    
    for (a in animals) {
      tmp <- subset(dt,Animal==a)
      tmp <- tmp[order(tmp$Time),]
      
      tmp$mean <- rowMeans(tmp[,c("M1","M2","M3")], na.rm=TRUE)
      start <- which.max(tmp$mean)[1]
      tmp <- tmp[start:nrow(tmp),]
      cf <- check.format(tmp)
      if (cf!="") warning(cf)
      fit1 <- oneexp(y=tmp$mean,x=tmp$Time)
      fit2 <- twoexp(y=tmp$mean,x=tmp$Time)
      
      newrow <- data.frame(Animal = a, 
                           GFR1 = 10^9*ifelse(is.null(fit1), NA, coef(fit1)[2]/coef(fit1)[1]),
                           GFR2 = 10^9*ifelse(is.null(fit2), NA, coef(fit2)[2]*coef(fit2)[4]/(coef(fit2)[1]*coef(fit2)[4]+coef(fit2)[2]*coef(fit2)[3])))
      results <- rbind(results, newrow)
      
    }
  
    return(results)
  })
  
})
  