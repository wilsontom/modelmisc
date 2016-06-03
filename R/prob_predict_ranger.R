#'
#'
#'
#'
#'
#'
#'
#'
#'
#'

prob_predict_ranger <- function(x,test_data)
  {
  if(class(x) != "ranger"){
    stop("...model input must be a ranger object", call. = FALSE)
  }

  if(is.null(x$forest)){
    stop("...a forest must be present for predictions, use write.forest = TRUE", call. = FALSE)
  }

  pred_all <- predict(x, test_data$data, predict.all = TRUE)

  pred_all_vals <- pred_all$predictions

  pred_prob <- apply(pred_all_vals,1,function(x)(table(x) / sum(table(x))))


  ## prob_matrix

  prob_mat <- matrix(nrow = nrow(test_data$data), ncol = length(levels(factor(test_data$class))))
  colnames(prob_mat) <- levels(factor(test_data$class))

  for(i in 1:length(pred_prob)){
    match_idx <- match(names(pred_prob[[i]]), colnames(prob_mat))
    prob_mat[i,match_idx] <- pred_prob[[i]]
  }

  prob_mat[is.na(prob_mat)] <- 0

  return(prob_mat)
  }

