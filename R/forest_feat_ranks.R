#' Random Forest feature rankings
#'
#' Create rankings for random forest feature importance scores
#'
#' @param model a randomForest model object
#' @param meth string of either \code{gini} (for MeanDecreaseGini) or \code{perm} (for MeanDecreaseAcc)
#' @return a \code{data.frame} of feature importance scores and rankings
#'
#' @export


forest_feat_ranks <- function(model, meth = "gini")
{
  # standard check here

  imp_df <- randomForest::importance(model, scale = FALSE)

  if (meth == "perm") {
    if (!"MeanDecreaseAccuracy" %in% colnames(imp_df)) {
      stop("no permutation importance; run model with importance = TRUE")
    } else{
      imp_vals <-
        data.frame(
          variable = rownames(imp_df),
          importance = imp_df[, "MeanDecreaseAccuracy"],
          row.names = NULL
        )
    }
  }

  if (meth == "gini") {
    imp_vals <-
      data.frame(
        variable = rownames(imp_df),
        importance = imp_df[, "MeanDecreaseGini"],
        row.names = NULL
      )
  }

  rank_ord <-
    rank(-abs(imp_vals$importance),
         na.last = TRUE,
         ties.method = "random")

  imp_df <- data.frame(imp_vals, rank = rank_ord)

  imp_df <- imp_df[order(imp_df$rank), ]

  return(imp_df)
}
