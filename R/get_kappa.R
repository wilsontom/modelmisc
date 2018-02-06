#' Cohen's Kappa
#'
#' Get the overall interate agreement rate (Cohen's Kappa)
#'
#' @param train_model a valid model object
#' @param test_data a \code{data.frame} to be used for prediction
#' @param test_class a vector of class lables for \code{test_data}
#' @return a numeric value for Kappa
#'
#'
#' @export

get_kappa <- function(train_model, test_data, test_class)
{
  # do some checking of dimensions and class names
  pred <- predict(train_model, test_data)
  kappa_stat <-
    caret::confusionMatrix(pred, test_class)$overall[["Kappa"]]

  return(as.numeric(kappa_stat))
}
