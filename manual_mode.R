
## Set the input parameters
# folder with input XLSX files
input.folder <- "/Users/ytakemon/Desktop/GFR/A1CFGFRDataFiles"
output.file <- "gfr_estimates.csv"
output.pdf  <- "gfr_plots.pdf" # if NULL then no plots
dilution <- 100 # (default=100)
verbose <- TRUE # if TRUE then print debugging messages
outlier.threshold <- 5 # if =Inf then no outlier removed

library(readxl)
library(ggplot2)
source("/Users/ytakemon/GitHub/GFRcalc/helpers.R")

# create PDF file
if (!is.null(output.pdf)) pdf(file=output.pdf)

# check that folder exists and contain XLSX files
stopifnot(file.exists(input.folder))
files <- dir(input.folder, pattern=".*.xlsx")
stopifnot(length(files)>0)

# table with GFR values for all files
summary.table <- NULL

# for each XLSX file, estimate GFR values and make plots
for (f in files) {

  if (verbose) cat("Opening file ", f, "\n")

  file.name <- paste0(input.folder, "/", f)
  dt <- read_excel(file.name, sheet=2, col_names = FALSE)
  dt <- dt[!is.na(dt[,1]),] # remove NA rows
  dt <- as.data.frame(dt)
  if (!is.character(dt[,1])) { # if animal IDs missing, add them
    animalID <- rep(LETTERS, each=length(unique(dt[,1])))[1:nrow(dt)]
    dt <- cbind(animalID, dt)
  }
  dt <- dt[,1:5] # only 5 columns matters
  names(dt) <- c("Animal", "Time", "M1", "M2", "M3")

  animal.table <- read_excel(file.name, sheet=3, col_names = FALSE)[2:4,-1]
  attr(dt, "animals") <- t(animal.table)

  animals <- unique(dt$Animal)
  animal.table <- attr(dt, "animals")
  results <- NULL

  #extra check - Time, M1, M2 and M3 should be numeric columns
  if (!(is.numeric(dt$Time) & is.numeric(dt$M1) &
        is.numeric(dt$M2)   & is.numeric(dt$M3))) {
    if (verbose) cat("Non-numeric values in file ", f, "\n")
    warning("Non-numeric values in file ", f)
    next
  }


  for (a in animals) {

    if (verbose) cat("  processing animal ", a, "\n")

    tmp <- subset(dt, Animal==a)
    tmp <- tmp[order(tmp$Time),]

    # very rough outlier detection
    tmp[-1,c("M1","M2","M3")][outlier.detect(tmp[-1,], outlier.threshold)] <- NA

    tmp$mean <- rowMeans(tmp[,c("M1","M2","M3")], na.rm=TRUE)
    start <- 2 # skip first observation
    tmp2 <- tmp[start:nrow(tmp),]

    # extra check for number of non-missing observations
    if (sum(!is.na(tmp2$mean))<5) {
      warning(paste("Skipped animal",a,"from",f,"because nb. of observations < 5"))
      if (verbose) cat("    not nough observations", "\n")
      next
    }

    # extra check for duplicated times
    if (any(duplicated(tmp2$Time))) {
      warning(paste("Skipped animal",a,"from",f,"because of duplicated 'Time'"))
      if (verbose) cat("    duplicated time", "\n")
      next
    }

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
                         "Sigma.C2" = ifelse(is.null(fit2),NA,as.integer(round(summary(fit2)$sigma))),
                         nNA = sum(is.na(tmp2[,c("M1","M2","M3")])),
                         check.names = FALSE)

    results <- rbind(results, newrow)

    # data for qplot
    if (!is.null(fit2)) {
      dt.plot <- data.frame(Time = rep(tmp2$Time,4),
                            Line = rep(c("F1","F2","F3","prediction"), each=nrow(tmp2)),
                            Fluorescence = c(tmp2$M1, tmp2$M2, tmp2$M3,
                                             predict(fit2, data.frame(x=tmp2$Time))))
    } else {
      medianM = median(c(tmp2$M1, tmp2$M2, tmp2$M3))
      dt.plot <- data.frame(Time = rep(tmp2$Time,4),
                            Line = rep(c("F1","F2","F3","prediction"), each=nrow(tmp2)),
                            Fluorescence = c(tmp2$M1, tmp2$M2, tmp2$M3,
                                             rep(medianM, nrow(tmp2))))
    }


    # remove missing points
    dt.plot <- subset(dt.plot, !is.na(Fluorescence))

    if (!is.null(output.pdf))
      plot(qplot(y=Fluorescence, x=Time, data=dt.plot) +
           geom_line(aes(group=Line, color=Line)) +
           ggtitle(paste(f,a)) +
           theme_bw())

  }

  rownames(results) <- NULL
  results <- cbind(file=f, results)

  summary.table <- rbind(summary.table, results)
}

# save output
write.csv(summary.table, output.file, row.names=FALSE)
if (!is.null(output.pdf)) dev.off()
