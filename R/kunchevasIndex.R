





kunchevasIndex <- function(x,y,m)
{
  if(!is.character(x)){
    stop("...x must be a character")
  }
  if(!is.character(y)){
    stop("...y must be a character")
  }

  x <- as.vector(na.omit(x))
  y <- as.vector(na.omit(y))

  if(length(x) != length(y)){
    stop("...vectors must have equal cardinality", call. = FALSE)
  }

  ku.num <- length(intersect(x,y)) * m - (length(x)^2)
  ku.den <- length(x) * (m - length(x))

  ku <- (ku.num / ku.den)

  if(ku  > 1 | ku < -1){
    stop("Kunchevas Index is not between -1 and 1", call. = FALSE)
  }
  return(ku)
}
