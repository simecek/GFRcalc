
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
  animal.table <- attr(tb, "animals") 
  if (any(is.na(as.numeric(as.character(animal.table[,3]))))) return("Injected volume must be numeric.")
  if (nrow(animal.table)!=length(unique(tb[,1]))) return("Number of animals differ between sheets 2 and 3.")
  return(invisible(""))
}

# fit log-normal regression
fit.rlm <- function(y,x) {
  require(MASS)
  fit <- lm(log(y)~x) 
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
  
  stopifnot(length(x) ==length(y))
  o <- order(x)
  x <- x[o]
  y <- y[o]
  n <- length(x)
  
  f1 <- oneexp(y[1:4],x[1:4])
  if (is.null(f1)) return(NULL)
  
  f2 <- oneexp(y[(n-3):n],x[(n-3):n])
  if (is.null(f2)) return(NULL)
  
  a1 <- coef(f1)[1]
  k1 <- coef(f1)[2]
  a2 <- coef(f2)[1]
  k2 <- coef(f2)[2]
  
  output <- tryCatch(nls(y ~ A*exp(-B*x) + C*exp(-D*x),
                         start=list(A=a1,B=k1,C=a2, D=k2),
                         algorithm="port"),
                     error = function(e) NULL)
  
  # if not converging, try diffetent optim. algorithm
  if (is.null(output)) 
    output <- tryCatch(nls(y ~ A*exp(-B*x) + C*exp(-D*x),
                           start=list(A=a1,B=k1,C=a2, D=k2)),
                       error = function(e) NULL)
  
  output
}  

# two components approximated by two one component models
twoexp.approx <- function(y,x) {
  
  stopifnot(length(x) ==length(y))
  o <- order(x)
  x <- x[o]
  y <- y[o]
  n <- length(x)
  
  f1 <- oneexp(y[1:4],x[1:4])
  if (is.null(f1)) return(NULL)
  
  f2 <- oneexp(y[(n-3):n],x[(n-3):n])
  if (is.null(f2)) return(NULL)
  
  # coefficients
  a1 <- coef(f1)[1]
  k1 <- coef(f1)[2]
  a2 <- coef(f2)[1]
  k2 <- coef(f2)[2]
  
  # intersect
  x0 <- (log(a2) - log(a1))/(k2-k1)

  a1/k1 * (1 - exp(-k1*x0)) + a2/k2 * exp(-k2*x0)  
}  
  

# integral of linear approximation from 0 to \int
linint <- function(y,x) {

  # enforse increasing ordering
  stopifnot(!any(duplicated(x)))
  stopifnot(length(x)==length(y))
  o <- order(x)
  x <- x[o]
  y <- y[o]
  
  # add 0 and \infty
  n <- length(x)
  x <- c(0,x,x[n]+(x[n]-x[n-1])/(y[n-1]-y[n])*y[n])
  y <- c(y[1]+(y[1]-y[2])*x[1]/(x[2]-x[1]),y,0)
  
  # integration
  sum(diff(x)*(y[-1]+y[-n])/2)
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
                                        predict(fit2, data.frame(x=tmp2$Time))))
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

outlier.detect <- function(tmp, trhold) {
  tmp$mean <- rowMeans(tmp[,c("M1","M2","M3")], na.rm=TRUE)
  fit <- lm(log2(tmp$mean)~tmp$Time)
  pred <- 2^predict(fit)
  output <- abs(log2(tmp[,c("M1","M2","M3")] / pred))>trhold/5
  return(output)

}
