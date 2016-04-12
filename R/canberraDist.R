#' Canberra Distance
#'
#' Calculate the Canberra Distance between two vectors of feature ranks. Vectors must be of equal lengths.
#'
#' @param x a character vector of features
#' @param y a second character vector of features
#' @param scale logical; if \code{TRUE} then the canberra distance is scaled by the maximum possible distance
#' to give value between 0 and 1. 0 = no similiarity, 1 = vectors are identical.
#' @return a numeric value for the canberra distance
#'
#' @author Tom Wilson <tpw2@@aber.ac.uk>
#' @export

canberraDist <- function(x,y, scale = TRUE)
  {

  cdist <- function(x,y)
  {
    if(length(x) != length(y)){
      stop("...lengths of x & y must be identical", call.  = FALSE)
    }

    can.dist <- sum(abs(x - y) /
                      (abs(x) + abs(y)))

    return(can.dist)
  }

  cd.res <- cdist(x,y)

  if(scale == TRUE){
    f.x <- rep(1:length(x), by = 1)
    f.y <- f.x[order(-f.x)]
    max.can <- cdist(f.x, f.y)

    cd.res <- (cd.res / max.can)
  }
  return(as.numeric(1 - cd.res))
  }
