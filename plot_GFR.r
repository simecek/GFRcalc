library(ggplot2)

dt <- read.csv("GFR.csv", as.is=TRUE)

animals <- unique(dt$Animal)
frange <- range(c(dt$M1, dt$M2, dt$M3), na.rm=TRUE)

pdf("plot_GFR.pdf")
for (a in animals) {
  tmp <- subset(dt, Animal == a)
  tmp2 <- data.frame(Time = rep(tmp$Time,3),
                     Line = rep(c("a","b","c"), each=nrow(tmp)),
                     Fluorescence = c(tmp$M1, tmp$M2, tmp$M3))
  
  p <- qplot(y=Fluorescence, x=Time, data=tmp2) +
    geom_smooth(method = "lm", color=I("black"), linetype=I(2))  +
    geom_line(aes(group=Line, color=Line)) +
    scale_y_log10() + 
    ggtitle(a) + 
    theme_bw()+ theme(legend.position="none") 
  print(p)
}
dev.off()
