#' Bland Altman Plot
#'
#' @param x a numeric vector (A)
#' @param y a numeric vector (B)
#' @return a ggplot2 object
#' @export
#' @importFrom ggpubr ggscatter
#' @importFrom ggplot2 geom_hline labs ggplot aes_string geom_tile scale_fill_gradient2 coord_fixed element_text theme theme_minimal


bland_altman_plot <- function(x,y)
  {

  if(!is.numeric(x)){
    stop(deparse(substitute(x)), ' must be numeric', call. = FALSE)
  }
  if(!is.numeric(y)){
    stop(deparse(substitute(y)), ' must be numeric', call. = FALSE)
  }

  if(length(x) < 5){
    stop('vector length must be greater then 5', call. = FALSE)
  }
  if(length(y) < 5){
    stop('vector length must be greater then 5', call. = FALSE)
  }

  if(length(x) != length(y)){
    stop('vector lengths do no match', call. = FALSE)
  }

  xdiff <- (x + y) / 2
  ydiff <- (x - y)

  mediff <- mean(ydiff)
  udiff <- mediff + (2 * sd(ydiff))
  ldiff <- mediff - (2 * sd(ydiff))

  bland_altman_df <- data.frame(x = xdiff, y = ydiff)
  baplot <-
    ggscatter(
      bland_altman_df,
      x = 'x',
      y = 'y',
      shape = 21,
      size = 3
    ) +
    geom_hline(yintercept = mediff, colour = 'red') +
    geom_hline(yintercept = udiff, colour = 'blue') +
    geom_hline(yintercept = ldiff, colour = 'blue') +
    labs(x = 'A+B / 2', y = 'A - B')

  return(baplot)
}

