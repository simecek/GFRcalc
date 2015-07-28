
# check file format
check.format <- function(tb, col.names=c("M1","M2","M3","Time"), min.rows=6) {
  if (is.null(tb)) return(invisible("")) # no file given
  if (!is.data.frame(tb)) return("ERROR: Not a data frame.")
  if (nrow(tb) %% 3 != 0) return("ERROR: Number of rows not divisible by 3.")
  if (!is.character(tb[,1]))
    return("ERROR: Animal column must be character.")
  for (c in 2:5)
    if (!is.numeric(tb[,c])) return(paste("ERROR: Column",c,"must be numeric."))
  if (any(is.na(tb[,2]))) return("ERROR: Time must not contain missing values.")
  if (any(tb[,2]<0 | tb[,2]>200)) return("ERROR: Time must be an integer between 0 and 200.")
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

make.plot <- function(dt, i) {
  animals <- unique(dt$Animal)
  a <- animals[i]
  
  tmp <- subset(dt, Animal == a)
  tmp <- tmp[order(tmp$Time),]
  
  tmp$mean <- rowMeans(tmp[,c("M1","M2","M3")], na.rm=TRUE)
  start <- 2 # skip first observation
  tmp2 <- tmp[start:nrow(tmp),]
  
  fit1 <- oneexp(y=tmp2$mean,x=tmp2$Time)
  fit2 <- twoexp(y=tmp2$mean,x=tmp2$Time)
  
  if (!is.null(fit2)) {
    dt.plot <- data.frame(Time = rep(tmp2$Time,4),
                       Line = rep(c("F1","F2","F3","prediction"), each=nrow(tmp2)),
                       Fluorescence = c(tmp2$M1, tmp2$M2, tmp2$M3,
                                        predict(fit2)))
  } else {
    medianM = median(c(tmp2$M1, tmp2$M2, tmp2$M3))
    dt.plot <- data.frame(Time = rep(tmp2$Time,4),
                          Line = rep(c("F1","F2","F3","prediction"), each=nrow(tmp2)),
                          Fluorescence = c(tmp2$M1, tmp2$M2, tmp2$M3,
                                           rep(medianM, nrow(tmp2))))
  }
  
  plot(qplot(y=Fluorescence, x=Time, data=dt.plot) +
         geom_line(aes(group=Line, color=Line)) +
         ggtitle(a) +  
         theme_bw())
}


