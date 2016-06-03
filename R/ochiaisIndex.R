

ochiaisIndex <- function(x,y)
{
  if(!is.character(x)){
    stop("x input must be a character")
  }
  if(!is.character(y)){
    stop("y input must be a character")
  }
  x <- as.vector(na.omit(x))
  y <- as.vector(na.omit(y))

  oi.in <- length(intersect(x,y))
  oi.den <- sqrt(length(x) * length(y))
  oi <-  round(oi.in / oi.den, digits =2)

  if(oi  > 1 | oi < 0){
    stop("Ochiai's index is not between 0 and 1", call. = FALSE)
  }
  return(oi)
}
