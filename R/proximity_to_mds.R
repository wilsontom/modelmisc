#' Multi Dimensional Scaling (MDS) of randomForest proximities
#'
#' @param x a `randomForest` object containing a valid proximity matrix
#' @return a `data.frame` of `cmdscale (1 - proxmimity)` for Dimension 1 and 2
#'
#' @export


proximity_to_mds <- function(x)
{

  if(class(x) != "randomForest"){
    stop(deparse(substitute(x)), " must be a 'randomForest' object", call. = FALSE)
  }

  if(is.null(x$proximity)){
    stop(deparse(substitute(x)), " must have a proximity matrix (run using `proximity = TRUE`) ", call. = FALSE)
  }

  prox_scale <- cmdscale(1 - x$proximity)
  prox_df <- data.frame(Dimension1 = prox_scale[,1], Dimension2 = prox_scale[,2])
  return(prox_df)
}
