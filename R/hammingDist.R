#' Hamming Distance
#'
#' Calculate the relative hamming distance between two subsets of features
#'
#' @param x a character vector of features
#' @param y a second character vector of features
#' @param m a numeric value for the total number of features in the dataset
#' @return a numeric value for the hamming distance (betwene 0 and 1)
#'
#' @author Tom Wilson <tpw2@@aber.ac.uk>
#' @export

hammingDist <- function(x,y,m)
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
