#' Test ROC-AUC
#'
#' Clculate the ROC-AUC using a training model and independant test-data. If classification is multinomial, then Hand and Till's (2001) method for multi-class AUC is used. If
#' classification is binary, then the standard ROC-AUC apprach is used.
#'
#' @param train_model a training model
#' @param test_data a \code{data.frame} of data for test predictions
#' @param test_cls a vector of class labels for \code{test_data}
#'
#' @export

get_test_auc <- function(train_model, test_data, test_cls)
{
  test_cls <- factor(test_cls)

  if (length(unique(test_cls)) == 2) {
    pred <- predict(train_model, test_data, type = "response")
    auc <- AUC::auc(AUC::roc(pred, test_cls))
  }

  if (length(unique(test_cls)) > 2) {
    pred <- predict(train_model, test_data, type = "prob")
    auc <- HandTill2001::auc(HandTill2001::multcap(test_cls, pred))
  }

  return(round(auc, digits = 3))
}
