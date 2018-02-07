#' Forest Accuracy
#'
#' Retrieve the forest accuracy (1 - OOB)
#'
#' @param model a randomForest model object
#' @return a numeric value for the model accuracy (0 - 1)
#'
#' @export

forest_accuracy <- function(model)
{
  oob <- model$err.rate
  acc <- 1 - as.numeric(oob[nrow(oob), "OOB"][[1]])
  return(round(acc, digits = 2))
}
