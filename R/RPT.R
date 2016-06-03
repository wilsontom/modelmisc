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
RPT <- function(stabilityMeasure, performanceMeasure, beta = 1)
{
  if(stabilityMeasure < 0 | stabilityMeasure > 1){
    stop("..Stability Measure must be between 0 and 1")
  }
  if(performanceMeasure < 0 | performanceMeasure > 1){
    stop("...Performance Measure must be between 0 and 1")
  }
  rpt.tp <- ((beta ^2) + 1) * stabilityMeasure * performanceMeasure

  rpt.bt <- ((beta ^2) * stabilityMeasure + performanceMeasure)

  rpt.res <- (rpt.tp / rpt.bt)

  return(rpt.res)
}

