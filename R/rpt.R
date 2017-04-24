#' Robustness Performance Trade Off
#' @name RPT
#' @description Calculates the Robustness Performance Trade-off of a model
#' @param stabilityMeasure A numeric value for model stability
#' @param performanceMeasure A numeric value for model performance
#' @param beta A positive integer.  Default is 1, which treats stability and performance equally.
#' @return A numeric value for RPT between 0 and 1
#' @author Tom Wilson
#' @export
#'
rpt <- function(stabilityMeasure, performanceMeasure, beta = 1)
  {
  if(length(stabilityMeasure) != 1){stop("...stability must be a numeric vale NOT a vector", call. = FALSE)}
  if(length(performanceMeasure) != 1){stop("...performance must be a numeric vale NOT a vector", call. = FALSE)}
  if(stabilityMeasure < 0 | stabilityMeasure > 1){stop("..stability Measure must be between 0 and 1", call. = FALSE)}
  if(performanceMeasure < 0 | performanceMeasure > 1){stop("...performance Measure must be between 0 and 1", call. = FALSE)}

  rpt.tp <- ((beta ^2) + 1) * stabilityMeasure * performanceMeasure
  rpt.bt <- ((beta ^2) * stabilityMeasure + performanceMeasure)
  rpt.res <- (rpt.tp / rpt.bt)

  return(rpt.res)
  }

