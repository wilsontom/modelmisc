#' Calculate the ROC-AUC from a randomForest model
#'
#' Calculte the training ROC-AUC for a randomForest model. The votes cast during out-of-bag (OOB) predictions are used for
#' the predicition of class. If classification is multinomial, then Hand and Till's (2001) method for multi-class AUC is used. If
#' classification is binary, then the standard ROC-AUC apprach is used.
#'
#' @param model a randomForest classification model
#' @return a numeric value for ROC-AUC
#'
#' @export

forest_auc <- function(model)
{
  if (class(model) != "randomForest") {
    stop(deparse(substitute(model)),
         " must be a randomForest object",
         call. = FALSE)
  }

  if (length(unique(model$y)) > 2) {
    auc <-
      HandTill2001::auc(HandTill2001::multcap(model$y, model$votes))
  }

  if (length(unique(model$y)) == 2) {
    auc <- AUC::auc(AUC::roc(model$predicted, model$y))
  }

  return(round(auc, digits = 2))
}
