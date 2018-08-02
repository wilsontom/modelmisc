
#' @export

get_test_accuracy <- function(train_model, test_data, test_class)
  {


  predob <- predict(train_model, test_data)

  true_class <- predob == test_class

  acc <- length(which(true_class == 'TRUE'))  / length(true_class)

  return(acc)
  }
