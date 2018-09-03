#' Jaccard's Similiarity
#'
#' Calculate the Jaccard's Similiarity (or Taminoto Distance) between feature vectors
#'
#' @param x a character vector
#' @param y a character vector
#' @return a numeric value for the Jaccard's Similiarity Coefficent
#' @importFrom stats cmdscale na.omit predict qt sd
#'
#' @export

jaccards_index <- function(x, y)
{
  if (!is.character(x)) {
    stop("x input must be a character")
  }
  if (!is.character(y)) {
    stop("y input must be a character")
  }
  x <- as.vector(na.omit(x))
  y <- as.vector(na.omit(y))

  ji.in <- length(intersect(x, y))
  ji.un <- length(union(x, y))
  ji <- ji.in / ji.un

  if (ji  > 1 | ji < 0) {
    stop("Jaacard index is not between 0,1")
  }
  return(ji)
}
