#Chapter 6

#' utility function - should change the name
#' @description 
#' Use when given absolute frequencies and the data seperately to get one data vector
#' @param X dataset assuming we have unique values
#' @param n_i absolute frequencies of the data from X
#' @return datavector
#' @export
utility <- function(X,n_i)
{
  vec <- c()
  len <- length(X)
  for(i in 1:len)
  {
    e <- X[i]*n_i[i]
    vec <- append(vec,e)
  }
  return(vec)
}


#' Quasi-variance
#' @description Calculates the quasi-variance given a dataset
#' @param X dataset - a datavector
#' @param n_i frequencies of the data, if none are given NULL is assumed
#' @return numeric value : quasivariance
#' @export
quasi_var <- function(X,n_i=NULL)
{
  ret <- 0
  if(is.null(n_i))
  {
    m <- mean(X)
    val <- sum((X-m)^2)/(length(X)-1)
    ret <- sqrt(val) 
  }
  else
  {
    K <- utility(X,n_i)
    ret <- quasi_var(K)
  }
  return(ret)
}

