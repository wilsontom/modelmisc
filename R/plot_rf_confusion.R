#' Plot Confusion Matrix from Random Forest Classification
#'
#' @param rf_model a randomForest classification model
#' @return a ggplot2 plot
#' @export

plot_rf_confusion <- function(rf_model)
{
  conf_mat <- rf_model$confusion[, -ncol(rf_model$confusion)]
  class_size <- apply(conf_mat, 1, sum)
  conf_mat[lower.tri(conf_mat)] <- NA

  conf_mat_perc <- (conf_mat / class_size) * 100

  melt_conf_mat <- reshape2::melt(conf_mat_perc, na.rm = TRUE)

  conf_plot <-
    ggplot(data = melt_conf_mat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = viridis::viridis(10)[1],
      high = viridis::viridis(10)[10],
      mid = viridis::viridis(10)[5],
      midpoint = 50,
      limit = c(0, 100),
      space = "Lab",
      name = "Accuracy (%)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      size = 12,
      hjust = 1
    )) +
    theme(axis.text.y = element_text(size = 12)) +
    labs(x = '', y = '') +
    coord_fixed()

  return(conf_plot)
}


