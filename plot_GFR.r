
# check file format
check.format <- function(tb, col.names=c("M1","M2","M3","Time"), min.rows=6) {
  if (is.null(tb)) return(invisible(""))
  if (!is.data.frame(tb)) return("Not a data frame.")
  if (!all(col.names %in% names(tb))) return("M1, M2, M3 and Time columns are required.") 
  for (c in col.names)
    if (!is.numeric(tb[,c])) return(paste("Column",c,"must be numeric."))
  if (any(is.na(tb$Time))) return("Time must not contain missing values.") 
  if (any(duplicated(tb$Time))) return("Time must not contain duplicated values.")
  if (nrow(tb) < min.rows) return(paste("Ar least",min.rows,"rows required."))
  return(invisible(""))
}

# fit log-normal regression
fit.rlm <- function(y,x) {
  require(MASS)
  fit <- rlm(log(y)~x) 
  fit
}

# fit one component model y = A*exp(-B*x) + noise
oneexp <- function(y,x) {
  a1 = exp(coef(fit.rlm(y,x))[1])
  k1 = -coef(fit.rlm(y,x))[2]
  
  tryCatch(nls(y ~ A*exp(-B*x), start= list(A=a1,B=k1)),
           error = function(e) NULL)
}

# fit two components model y = A*exp(-B*x) + C*exp(-D*x) + noise
twoexp <- function(y,x) {
  f1 <- oneexp(y,x)
  if (is.null(f1)) return(NULL)
  
  a1 <- coef(f1)[1]
  k1 <- coef(f1)[2]
  a2 <- a1/10
  k2 <- k1/10
  
  tryCatch(nls(y ~ A*exp(-B*x) + C*exp(-D*x),
               start=list(A=a1,B=k1,C=a2, D=k2),
               algorithm="port"),
           error = function(e) NULL)
}  

# ggplot prediction
plot.pred <- function(dt, pred) {
  require(ggplot2)
  tmp2 <- data.frame(Time = rep(dt$Time,4),
                     Line = rep(c("M1","M2","M3","model"), each=nrow(dt)),
                     Fluorescence = c(dt$M1, dt$M2, dt$M3,
                                      pred))
  
  qplot(y=Fluorescence, x=Time, data=tmp2) +
    geom_line(aes(group=Line, color=Line)) +
    theme_bw()
}

# analyze one animal
dt <- read.csv("GFR.csv", as.is=TRUE)
animals <- unique(dt$Animal)
results <- NULL

pdf("plot_10examples_GFR.pdf",width=12)
plot(0,1)
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
