#' Calculate the test inter-rate agreement
#'
#' Calculate Cohen's Kappa using a training model and independant test-data
#'
#' @param train_model a training model
#' @param test_data a \code{data.frame} of data for test predictions
#' @param test_cls a vector of class labels for \code{test_data}
#'
#' @export

get_test_kappa <- function(train_model, test_data, test_cls)
{
  test_pred <- predict(train_model, test_data)
  kappa_stat <-
    caret::confusionMatrix(test_pred, test_cls)$overall[["Kappa"]]
  return(as.numeric(round(kappa_stat), digits = 3))
}
