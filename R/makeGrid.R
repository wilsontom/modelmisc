#' Make parameter grid
#'
#' Create grid for all optimisation combinations possible
#'
#' @param ntree A numeric vector, with elements.
#' @param mtry
#' @param nodesize
#' @param maxnodes
#'
#' @return
#' @author Tom Wilson \email{tpw2@@aber.ac.uk}
#'
#' @examples
#'
#'
#'
#'
makeGrid <- function(ntree = c(100,200,100), mtry = c(1,2), nodesize = c(1,2), maxnodes = NULL)
	{
	Trees <- seq(from = ntree[1], to = ntree[2], by = ntree[3])
	VarSplit <- c(1,2,3)
	NodeSize <- c(1,2)
  MaxNodes <- maxnodes

  if(is.null(maxnodes)){
  paramGrid <- expand.grid(Trees, VarSplit, NodeSize)
  names(paramGrid) <- c("ntree", "mtry", "nodesize")
  }else{
    paramGrid <- expand.grid(Trees, VarSplit, NodeSize, MaxNodes)
  names(paramGrid) <- c("ntree", "mtry", "nodesize", "maxnodes")
  }
  return(paramGrid)
	}



paramGrid <- makeGrid()
