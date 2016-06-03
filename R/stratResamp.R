#' Stratified Resampling
#'
#'
#' @param x
#' @param cl
#' @param p
#' @return
#'

stratResamp <- function(x,cl,p)
  {

  if(nrow(x) != length(cl)){
    stop("...Class labels and data are not equal lengths", call. = FALSE)
  }

  if(!is.numeric(p)){
    stop("...The partitioning size (p) must be a numeric value", call. = FALSE)
  }

  n_ch <- min(table(cl))

  pn <- round(p * n_ch, digits = 0)

  dat_df <- data.frame(ind = seq(from = 1, to = nrow(x), by = 1), class = cl, x)

  p_ind <- data.frame(dat_df %>% dplyr::group_by(class) %>% dplyr::sample_n(pn))$ind

  data_part <- list()
  data_part$train$class <- cl[p_ind]
  data_part$train$data <- x[p_ind,]
  data_part$test$class <- cl[-p_ind]
  data_part$test$data <- x[-p_ind,]

  return(data_part)
  }
