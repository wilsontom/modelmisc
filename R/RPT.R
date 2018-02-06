#' RPT
#'
#' Calculates the Robustness Performance Trade-off
#'
#' @param stability a numeric value for model stability
#' @param performance a numeric value for model performance
#' @param beta a positive integer.  Default is 1, which treats stability and performance equally.
#' @return a numeric value for RPT between 0 and 1
#'
#' @export

RPT <- function(stability, performance, beta = 1)
{
  if (length(stability) != 1) {
    stop("...stability must be a numeric vale NOT a vector", call. = FALSE)
  }
  if (length(performance) != 1) {
    stop("...performance must be a numeric vale NOT a vector", call. = FALSE)
  }
  if (stability < 0 |
      stability > 1) {
    stop("..stability  must be between 0 and 1", call. = FALSE)
  }
  if (performance < 0 |
      performance > 1) {
    stop("...performance must be between 0 and 1", call. = FALSE)
  }

  rpt.tp <- ((beta ^ 2) + 1) * stability * performance
  rpt.bt <- ((beta ^ 2) * stability + performance)
  rpt.res <- (rpt.tp / rpt.bt)

  return(rpt.res)
}
