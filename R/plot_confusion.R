#' Plot a nicely formatted model confusion matrix
#'
#' @param confusion_table The confusion table from the trained model
#'
#' @export
plot_confusion = function(confusion_table){

  confusion_p = signif(100 * prop.table(confusion_table, 2), digits = 3)

  confusion_p %>%
    ggplot2::ggplot() +
    aes(rev(Prediction), Reference, fill = n, label = sprintf("%0.1f", n)) +
    geom_tile() +
    geom_text(size = 10, colour = 'white') +
    viridis::scale_fill_viridis(direction = -1) +
    theme_minimal() +
    labs(x = 'Prediction') +
    theme(legend.position = 'none')
}



