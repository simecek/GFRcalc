library(ggplot2)
library(MASS)
library(cowplot)

dt <- read.csv("GFR.csv", as.is=TRUE)

animals <- unique(dt$Animal)

pdf("plot_10examples_GFR.pdf",width=12)
for (a in animals) {
  tmp <- subset(dt, Animal == a)
  
  # fit robust linear model
  fit <- rlm(log(Mmean)~Time, data=tmp)  
  a1 <- tmp$Mmean[1]/2
  k1 <- - coef(fit)[2]
  
  # fit a model ~ A*exp(-B*Time)
  oneexp <- nls(Mmean ~ A*exp(-B*Time),
                   data = tmp[-1,], start= list(A=a1,B=k1))
  
  # plot prediction
  tmp2 <- data.frame(Time = rep(tmp$Time[-1],4),
                     Line = rep(c("a","b","c","model"), each=nrow(tmp)-1),
                     Fluorescence = c(tmp$M1[-1], tmp$M2[-1], tmp$M3[-1],
                                      predict(oneexp)))
  
  p1 <- qplot(y=Fluorescence, x=Time, data=tmp2)   +
    geom_line(aes(group=Line, color=Line)) +
    ggtitle(paste(a,"one exp model")) + 
    theme_bw()
  
  
  a1 <- coef(oneexp)[1]
  k1 <- coef(oneexp)[2]
  a2 <- a1/10
  k2 <- k1/10
  
  twoexp <- tryCatch(nls(Mmean ~ A*exp(-B*Time) + C*exp(-D*Time),
                   data = tmp[-1,], start=list(A=a1,B=k1,C=a2, D=k2),
                   algorithm="port"),
                   error = function(e) NULL)
  
  if (is.null(twoexp)) {
    pred = rep(mean(tmp$Mmean[-1]),nrow(tmp)-1)
  } else {
    pred= predict(twoexp)
  }  
  tmp2 <- data.frame(Time = rep(tmp$Time[-1],4),
                     Line = rep(c("a","b","c","pred"), each=nrow(tmp)-1),
                     Fluorescence = c(tmp$M1[-1], tmp$M2[-1], tmp$M3[-1],
                                      pred))
  
  p2 <- qplot(y=Fluorescence, x=Time, data=tmp2)   +
    geom_line(aes(group=Line, color=Line)) +
    ggtitle(paste(a,"two exp model")) + 
    theme_bw()
  print(plot_grid(p1, p2))
}
dev.off()

