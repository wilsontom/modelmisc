#' Confidence Interval
#'
#' Calculate a confidence interval for a vector of values
#'
#' @param x a numeric vector
#' @param ci a numeric value (0 - 1) for the required confidence interval
#' @return a numeric value for the lower and upper bounds of the confidence interval
#'
#' @export

conf_int <- function(x,ci = 0.975)
  {

  me <- mean(x)
  sdev <- sd(x)
  n <- length(x)

  error <- qt(as.numeric(ci), df = n-1)*sdev/sqrt(n)

  left <- me - error
  right <- me + error

  return(c(round(left,digits = 3), round(right, digits = 3)))

  }
