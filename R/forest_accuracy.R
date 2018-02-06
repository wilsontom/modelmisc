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
  return(1 - as.numeric(oob[nrow(oob), "OOB"][[1]]))
}
