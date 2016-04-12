#' Jaccard's Similiarity
#'
#' Calculate the Jaccard's Similiarity (or Taminoto Distance) between two subsets of features
#'
#' @param x a character vector of features
#' @param y a second character vector of features
#' @return a numeric value for the jaccard's similiarity (betwene 0 and 1)
#'
#' @author Tom Wilson <tpw2@@aber.ac.uk>
#' @export

jaccardsIndex <- function(x,y)
  {
  if(!is.character(x)){
    stop("x input must be a character")
  }
  if(!is.character(y)){
    stop("y input must be a character")
  }
  x <- as.vector(na.omit(x))
  y <- as.vector(na.omit(y))

  ji.in <- length(intersect(x,y))
  ji.un <- length(union(x,y))
  ji <- ji.in / ji.un

  if(ji  > 1 | ji < 0){
    stop("Jaacard index is not between 0,1")
  }
  return(ji)
  }
