
# analyze one animal
dt <- read.csv("GFR.csv", as.is=TRUE)
animals <- unique(dt$Animal)
results <- NULL

pdf("plot_10examples_GFR.pdf",width=12)

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
                       GFR1 = ifelse(is.null(fit1), NA, coef(fit1)[2]/coef(fit1)[1]),
                       GFR2 = ifelse(is.null(fit2), NA, coef(fit2)[2]*coef(fit2)[4]/(coef(fit2)[1]*coef(fit2)[4]+coef(fit2)[2]*coef(fit2)[3])))
  results <- rbind(results, newrow)
  
  if (!is.null(fit1)) pred1 <- predict(fit1) else pred1 <- rep(mean(tmp$mean),nrow(tmp))
  if (!is.null(fit2)) pred2 <- predict(fit2) else pred2 <- rep(mean(tmp$mean),nrow(tmp))
  
  p1 <- plot.pred(tmp, pred1) + ggtitle(paste(a,": one component model"))
  p2 <- plot.pred(tmp, pred2) + ggtitle(paste(a,": two component model"))

  require(cowplot)
  plot(plot_grid(p1, p2))
}
dev.off()

write.csv(results,"10examples_GFR.csv", row.names=FALSE)
