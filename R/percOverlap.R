


percOverlap <- function(x,y)
{
  if(!is.character(x)){
    stop("x input must be a character")
  }
  if(!is.character(y)){
    stop("y input must be a character")
  }
  x <- as.vector(na.omit(x))
  y <- as.vector(na.omit(y))

  perc_x <- length(intersect(x,y)) / length(x)
  perc_y <- length(intersect(x,y)) / length(y)

  if(dc  > 1 | dc < 0){
    stop("Dice - Sorensen's index is not between 0 and 1", call. = FALSE)
  }
  return(c(perc_x, perc_r))
}
