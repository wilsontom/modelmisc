#' Confidence Interval (Tidy)
#'
#' Calculate a confidence interval for a vector of values
#'
#' @param x a numeric vector
#' @param ci a numeric value (0 - 1) for the required confidence interval
#' @return a character with the value for the lower and upper bounds of the confidence interval wrapped in parentheses for making tables
#'
#' @export

conf_int_tidy <- function(x, ci = 0.975)
{
  me <- mean(x)
  sdev <- sd(x)
  n <- length(x)

  error <- qt(as.numeric(ci), df = n - 1) * sdev / sqrt(n)

  left <- round((me - error), digits = 3)
  right <- round((me + error), digits = 3)

  conf <- paste0('(', left, ',', right, ')')

  return(conf)

}
