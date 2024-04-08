
#' Interactive plot of 2d intensity data
#'
#' @param data data
#' @param channel_1 channel 1
#' @param channel_2 channel 2
#' @param bins Number of bins
#' @param plot_all If FALSE, takes subsample of 500,000 data points to plot
#' @param plotly Make plot interactive using plotly
#'
#' @export
plot_intensity_2d = function(data, channel_1, channel_2, bins = 500, plot_all = FALSE, plotly = T){

  data = data %>%
    dplyr::ungroup()

  if(plot_all == FALSE & nrow(data) > 500000){
    data = data %>%
      dplyr::sample_n(500000)
  }

  lim_x = data %>%
    dplyr::pull(!!ensym(channel_1)) %>%
    quantile(c(0.0002, 0.9998), na.rm = T)

  lim_y = data %>%
    pull(!!ensym(channel_2)) %>%
    quantile(c(0.0002, 0.9998), na.rm = T)

  plot = data %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = !!ensym(channel_1), y = !!ensym(channel_2), fill = sqrt(..count..)) +
    ggplot2::geom_bin2d(bins = bins) +
    viridis::scale_fill_viridis() +
    ggplot2::lims(x = lim_x, y = lim_y) +
    ggplot2::theme_bw()

  if(plotly) plot = plotly::ggplotly(plot)

  return(plot)
}
