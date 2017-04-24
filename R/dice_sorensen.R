#' Dice Sorensen Index
#'
#' Calculate the Dice-Sorenson Index between two feature vectors
#'
#' @param x a character vector
#' @param y a character vector
#' @return a numeric value for the Dice-Sorensen Index
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @references Zucknick, M., Richardson, S., Stronach, E.A., 2008. \emph{Comparing the characteristics of
#' gene expression profiles derived by univariate and multivariate classification
#' methods}. Statistical Applications in Genetics and Molecular Biology 7 (1):7
#' @references Loscalzo, S., Yu, L., Ding, C., 2009. \emph{Consensus group stable feature selection}. In:
#' Proceeding of the 15th ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD’09),pp.567–575.
#' @export

dice_sorensen <- function(x,y)
{
  if(!is.character(x)){
    stop("x input must be a character")
  }
  if(!is.character(y)){
    stop("y input must be a character")
  }
  x <- as.vector(na.omit(x))
  y <- as.vector(na.omit(y))

  dc.in <- 2 * (length(intersect(x,y)))
  dc.den <- length(x) + length(y)
  dc <-  round(dc.in / dc.den, digits =2)

  if(dc  > 1 | dc < 0){
    stop("Dice - Sorensen's index is not between 0 and 1", call. = FALSE)
  }
  return(dc)
}
