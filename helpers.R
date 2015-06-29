
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
