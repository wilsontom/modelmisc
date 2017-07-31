#' Variance Explained
#'
#' @param x a \code{prcomp} object
#' @return a numeric vector of percentage variance explained for each principal componenet (PC)
#' @export

# move to modelMisc
varExp <- function(x)
{
  if(class(x) != "prcomp"){0
    stop(deparse(substitute(x)), " must be a 'prcomp' object", call. = FALSE)
  }

  variance_explained <- round(((x$sdev^2) / sum(x$sdev^2)) * 100, digits= 2)

  return(as.numeric(variance_explained))
}
