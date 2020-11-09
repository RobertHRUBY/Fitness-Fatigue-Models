# General RMSE Function
rmse_func <- function(x,y,n,t){
      x <- x[t]
      z <- (x - y)^2
      return(sqrt(sum(z)/n))
    }

# Numerical Fit (R^2 and Adjusted R^2)
rsq_func <- function(x,y,n,t){
  x <- x[t]
  rsq_temp <- 1 - ( (sum((x-y)^2))  / (sum((y-mean(y))^2)) )
  adj_temp <- 1- ( (1-rsq_temp)*(n-1)/(n-4-1) )
  return(c(rsq_temp, adj_temp))
}

# Summary Statistics
summary_fun <- function(x){
    return(c("mean"=mean(x),"sd"=sd(x),"median"=median(x),
             "IQR"=IQR(x),"min"=min(x),"max"=max(x)))
}  
    