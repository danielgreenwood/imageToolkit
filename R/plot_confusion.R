#' Plot a nicely formatted model confusion matrix
#'
#' @param confusion_table The confusion table from the trained model
#'
#' @export
plot_confusion = function(confusion_table){

  confusion_p = signif(100 * prop.table(confusion_table, 2), digits = 3)

  confusion_p %>%
    ggplot2::ggplot() +
    ggplot2::aes(rev(Prediction), Reference, fill = n, label = sprintf("%0.1f", n)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(size = 10, colour = 'white') +
    viridis::scale_fill_viridis(direction = -1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = 'Prediction') +
    ggplot2::theme(legend.position = 'none')
}



