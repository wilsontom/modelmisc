



diceSorensen <- function(x,y)
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
