#' Relative Hamming Distance
#'
#' Calculate the Relative Hamming Distance between two feaure vectors
#'
#' @param x a character vector
#' @param y a character vector o
#' @param m a numeric value for the total number of features in the dataset
#' @return a numeric value for the Relative Hamming Distance
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#'
#' @references Dunne, K., Cunningham, P., Azuaje, F., 2002. \emph{Solutions to instability problems with
#' sequential wrapper-based approaches to feature selection}. Technical Report,Department of Computer Science, Trinity College, Dublin.
#'
#' @export

hammings_distance <- function(x,y,m)
  {
  if(!is.character(x)){
    stop("x input must be a character")
  }

  if(!is.character(y)){
    stop("y input must be a character")
  }
  x <- as.vector(na.omit(x))
  y <- as.vector(na.omit(y))

  hamming <- 1 - ((abs(length(which(!x %in% y))) +
                     abs(length(which(!y %in% x)))) /
                        m)
  return(hamming)
  }
