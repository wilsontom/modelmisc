#' Stratified Resampling
#'
#' Create a training and test set, stratfied by \code{class}
#'
#' @param x a \code{data.frame} of variables and observations
#' @param cls a vector of class information for stratifying. It is assumed that \code{cls} is balanaced
#' @param p a numeric value for the partitioning ratio (ie, 0.632)
#'
#' @return a list of four elements
#'   \itemize{
#'       \item{train_cls} \code{cls} vector for training set
#'       \item{train_x} training data
#'       \item{test_cls} \code{cls} vector for test set
#'       \item{test_x} test data
#'   }
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export

strat_resamp <- function(x,cls,p)
  {
  # balance checker + warning

  # use deparse warnings
  if(nrow(x) != length(cls)){
    stop("...class labels and data are not equal lengths", call. = FALSE)
  }

  if(!is.numeric(p)){
    stop("...The partitioning size (p) must be a numeric value", call. = FALSE)
  }

  n_ch <- min(table(cls))

  pn <- round(p * n_ch, digits = 0)

  dat_df <- data.frame(ind = seq(from = 1, to = nrow(x), by = 1), class = cls, x)

  p_ind <- data.frame(dat_df %>% dplyr::group_by(class) %>% dplyr::sample_n(pn))$ind

  dpart <- list()
  dpart$train_cls <- cls[p_ind]
  dpart$train_x <- data.frame(x[p_ind,])
  dpart$test_cls <- cls[-p_ind]
  dpart$test_x <- data.frame(x[-p_ind,])

  return(dpart)
  }
