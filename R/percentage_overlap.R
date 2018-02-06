#' Percentage overlap
#'
#' Calculate the percentage of overlap between two feature vectors
#'
#' @param x a character vector
#' @param y a character vector
#' @return two numeric values. Value one is the percentage of \code{x} which overlaps with \code{y}.
#' Value two is the percentage of \code{y} which overlaps with \code{x}
#'
#' @export

percentage_overlap <- function(x, y)
{
  if (!is.character(x)) {
    stop("x input must be a character")
  }
  if (!is.character(y)) {
    stop("y input must be a character")
  }
  x <- as.vector(na.omit(x))
  y <- as.vector(na.omit(y))

  perc_x <- length(intersect(x, y)) / length(x)
  perc_y <- length(intersect(x, y)) / length(y)

  return(c(perc_x, perc_y))
}
