#' Canberra Distance
#'
#' Calculate the Canberra Distance between two vectors of feature ranks. Input vectors must both be numeric and have equal
#' cardinality.
#'
#' @param x a numeric vector
#' @param y a numeric vector
#' @param scale logical; if \code{TRUE} then the canberra distance is scaled by the 1 - (maximum possible distance)
#' to give value between 0 and 1. 0 = vectors are identical, 1 = no similiarity
#' @return a numeric value for the canberra distance
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @references Jurman, G., Merler, S., Barla, A., Paoli, S., Galea, A., Furlanello, C., 2008. \emph{Algebraic
#' stability indicators for ranked lists in molecular profiling}. Bioinformatics 24 (2):258–264
#' @export

canberra_distance <- function(x,y, scale = TRUE)
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
