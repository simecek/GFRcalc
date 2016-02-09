
library(readxl)
library(ggplot2)
source("helpers.R")

input.folder <- "H:/SmallProjects/1507 GFR examples/3files"
dilution <- 100

# check that folder exists and contain XLSX files
stopifnot(file.exists(input.folder))
files <- dir(input.folder, pattern=".*.xlsx")
stopifnot(length(files)>0)

# table with GFR values for all files
summary.table <- NULL

# for each XLSX file, estimate GFR values and make plots
for (f in files) {
  file.name <- paste0(input.folder, "/", f)
  dt <- read_excel(file.name, sheet=2, col_names = FALSE)
  dt <- dt[!is.na(dt[,1]),] # remove NA rows
  
  if (!is.character(dt[,1])) { # if animal IDs missing, add them
    animalID <- rep(LETTERS, each=length(unique(dt[,1])))[1:nrow(dt)]
    dt <- cbind(animalID, dt)
  }
  dt <- dt[,1:5] # only 5 columns matters
  names(dt) <- c("Animal", "Time", "M1", "M2", "M3")  

  animal.table <- read_excel(file.name, sheet=3, col_names = FALSE)[2:4,-1]
  attr(dt, "animals") <- t(animal.table)
  
  #stopifnot(check.format(dt)=="")    
  
  animals <- unique(dt$Animal)
  animal.table <- attr(dt, "animals")
  results <- NULL
  
  for (a in animals) {
    
    tmp <- subset(dt, Animal==a)
    tmp <- tmp[order(tmp$Time),]
    
    # very rough outlier detection
    tmp[-1,c("M1","M2","M3")][outlier.detect(tmp[-1,], 5)] <- NA
    
    tmp$mean <- rowMeans(tmp[,c("M1","M2","M3")], na.rm=TRUE)
    start <- 2 # skip first observation
    tmp2 <- tmp[start:nrow(tmp),]
    
    fit1 <- oneexp(y=tmp2$mean,x=tmp2$Time)
    fit2 <- twoexp(y=tmp2$mean,x=tmp2$Time)
    fit3 <- linint(y=tmp2$mean,x=tmp2$Time)
    fit4 <- twoexp.approx(y=tmp2$mean,x=tmp2$Time)
    
    inj.volume <- as.numeric(animal.table[animals==a,3])
    
    newrow <- data.frame(Animal = a,
                         MouseId = animal.table[animals==a,1],
                         Weight = as.numeric(animal.table[animals==a,2]),
                         "Injected Volume" = inj.volume,
                         
                         "C2" = dilution*inj.volume*tmp$mean[1]*ifelse(is.null(fit2), NA, coef(fit2)[2]*coef(fit2)[4]/(coef(fit2)[1]*coef(fit2)[4]+coef(fit2)[2]*coef(fit2)[3])),
                         "2xC1" = ifelse(is.null(fit4),NA,dilution*inj.volume*tmp$mean[1]/fit4),
                         "C1" = dilution*inj.volume*tmp$mean[1]*ifelse(is.null(fit1), NA, coef(fit1)[2]/coef(fit1)[1]),
                         "PL" = ifelse(is.null(fit3), NA, dilution*inj.volume*tmp$mean[1]/fit3),
                         
                         nNA = sum(is.na(tmp2[,c("M1","M2","M3")])),
                         check.names = FALSE)
    
    results <- rbind(results, newrow)
  }
  
  summary.table <- rbind(summary.table, results)
}