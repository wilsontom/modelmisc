#' Calculate Cohen's Kappa from a randomForest object
#'
#' Calculte the training inter-rate agreement (Kappa) for a randomForest model. The votes cast during out-of-bag (OOB) predictions are used for
#' the predicition of class.
#'
#' @param model a randomForest classification model
#' @return a numeric value for the overall inter-rate agreement (Kappa)
#'
#' @export

forest_kappa <- function(model)
{
  if (class(model) != "randomForest") {
    stop(deparse(substitute(model)),
         " must be a randomForest object",
         call. = FALSE)
  }
  kappa_stat <-
    caret::confusionMatrix(model$predicted, model$y)$overall[["Kappa"]]
  return(as.numeric(round(kappa_stat), digits = 3))
}
