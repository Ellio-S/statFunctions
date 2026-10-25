#Chapter 3

#' Moving Average
#' @description Calculates moving average
#' @param data dataset, recommended to be a time series object, or a vector
#' @param order order of the moving average
#' @return A time series object
#' @export
moving_average <- function(data,order)
{
  res <- c()
  if(order %% 2 == 1)
  {
    res <- stats::filter(data, rep(1, times=order)/order)
  }
  else
  {
    vec <- c(0.5)
    vec <- append(vec,rep(1,times=order-1))
    vec <- append(vec,0.5)
    res <- stats::filter(data,vec/order)
  }
  return(res)
}


#' rm_random - Remove random component
#' @description
#' Removes the random component (A) from a time series where the TC time cycle component already has been removed.
#' @param data dataset
#' @param order order or frequency of the time series 
#' @return E = EA/A
#' @export
rm_random <- function(data,order)
{
  norm <- colMeans(matrix(data,ncol=order,byrow=TRUE),na.rm=TRUE)
  return(norm)
}

#' Normalise
#' @description
#' Normalises a data set by dividing each element by the average
#' @param data dataset
#' @return a normalised dataset
#' @export
normalise <- function(data)
{
  res <- data/mean(data)
  return(res)
}