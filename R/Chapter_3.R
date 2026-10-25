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
  if(order %% 2 == 1) #odd order
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