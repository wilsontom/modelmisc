#' Prepare data for K-fold cross validation
#'
#' @param data A \code{data.frame} or matrix of observations and variables
#' @param class A vector of corresponding class lables for \code{data}
#' @param K The number of folds to create
#'
#' @return A list
#'
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#' @export
#' @importFrom cvTools cvFolds
#'
kfold <- function(data, class, K = 10)
  {
  if(nrow(data) != length(class)){
    stop("...number of observations must equal class length", call. = FALSE)
  }

  if((is.vector(class)) & !is.factor(class)){
      class <- as.factor(class)
  }

  idx_folds <- cvTools::cvFolds(nrow(data), type = "random", K)

  data_list <- list()
  class_list <- list()

  k_idx <- 1:K
  for(i in k_idx){
    idx_tmp <- which(idx_folds$which == k_idx[i])
    data_list[[i]] <- data[idx_folds$subsets[idx_tmp],]
  }

  k_idx <- 1:K
  for(i in k_idx){
    idx_tmp <- which(idx_folds$which == k_idx[i])
    class_list[[i]] <- class[idx_folds$subsets[idx_tmp]]
  }

  data_input <- list()
  for(i in seq_along(data_list)){
      data_input[[i]] <- list(data_list[[i]], class_list[[i]])
  }


  for(i in seq_along(data_input)){
    data_input[[i]][[3]] <- rep(i, length(data_input[[i]][[2]]))
  }

  data_matrix <- list()
  for(i in seq_along(data_input)){
    data_matrix[[i]] <- cbind(data_input[[i]][[3]],data_input[[i]][[2]],data_input[[i]][[1]])
    names(data_matrix[[i]])[1:2] <- c("fold", "class")
  }




  data_all <- do.call("rbind", data_matrix)
  data_final <- list()


  str(data_all)



  a1 <- expand.grid(1:K, 1:K)
  a2<- split(a1, f = a1[,2])

  for(i in seq_along(a2)){
    print(i)


    idx <- which(a2[[i]][,1] == a2[[i]][,2])
    a2[[i]] <- a2[[i]][-idx,]


}


  train_id <- list()
  test_id <- list()
  for(i in seq_along(a2)){

    train_id[[i]] <- a2[[i]][,1]
    test_id[[i]] <- unique(a2[[i]][,2])
  }

  data_final <- list()
  tmp <- list()
  tmp2 <- list()
  tmp3 <- list()
  for(i in seq_along(train_id)){
    for(k in 1:length(train_id[[i]])){

      tmp[[k]] <- which(data_all$fold == train_id[[i]][[k]])
    }
    tmp2[[i]] <- data_all[unlist(tmp),]
    tmp3[[i]] <- data_all[-unlist(tmp),]

    data_final[[i]] <- list(train = tmp2[[i]], test = tmp3[[i]])

    }





  for(i in 1:length(data_final)){
    data_final[[i]][[1]]$fold <- NULL
    data_final[[i]][[1]]$class <- as.factor(data_final[[i]][[1]]$class)
    }



  for(i in 1:length(data_final)){
    data_final[[i]][[2]]$fold <- NULL
    data_final[[i]][[2]]$class <- as.factor(data_final[[i]][[2]]$class)
  }











    ind <- train_id[[1]]

    tmp <- grepl(ind[2],data_all$fold)
    grepl(ind[3],data_all$fold)
    grepl(ind[4],data_all$fold)



    which(data_all$fold == any(train_id[[5]]))








    train <- data_all[match_idx,]
    test <- data_all[-match_idx,]
    data_final[[i]] <- list(train = train, test = test)
    #names(data_final[[i]]) <- c("train", "test")
  }



  test_id















  if(length(data_input) != K){
      stop("...something has gone wrong", call. = FALSE)
  }

  return(data_input)
  }























