
#' Cluster variable 1 based on mean values. Pass columns directly (not as strings)
#' e.g. df %>% cluster_variable(drugname, gene, expression) %>% ggplot() Clusters by first variable only, so run twice to cluster both ways
#' @param df data.frame of values
#' @param cluster_var1 Column to cluster
#' @param cluster_var2 Column not to be clustered
#' @param value_var Column with values to be clustered by
#' @param method Clustering method
#'
#' @return data.frame with clustered column as reordered factor
#' @export
cluster_variable <- function(df, cluster_var1, cluster_var2, value_var, method = "euclidean") {

  cluster_var1_string = (as.character(deparse(substitute(cluster_var1))))

  order = dplyr::select(df, !!ensym(cluster_var1), !!ensym(cluster_var2), !!ensym(value_var)) %>%
    dplyr::group_by(!!ensym(cluster_var1), !!ensym(cluster_var2))  %>%
    dplyr::summarise_all(mean) %>%
    tidyr::pivot_wider(names_from = !!rlang::ensym(cluster_var2), values_from = !!rlang::ensym(value_var)) %>%
    column_to_rownames(var = cluster_var1_string)  %>%
    dist(method = method)  %>%
    stats::hclust %>%
    {.$labels[.$order]}

  #return(order)

  df[[cluster_var1_string]] = factor(df[[cluster_var1_string]], levels = order)

  return(df)
}
