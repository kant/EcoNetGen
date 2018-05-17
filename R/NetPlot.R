# See https://matthewlincoln.net/2014/12/20/adjacency-matrix-plots-with-r-and-ggplot2.html
# Also, see matrix plot method in dev version of ggraph


#' Plot network adjacency matrix
#'
#' @param graph an igraph object
#' @export
#' @importFrom ggplot2 aes_string ggplot scale_x_discrete scale_y_discrete
#' @importFrom ggplot2 element_blank theme_bw xlab ylab theme geom_raster
#' @importFrom igraph get.data.frame
#' @examples
#' \donttest{
#' set.seed(12345)
#' graph <- netgen()
#' adj_plot(graph)
#' }
#'
adj_plot <- function(graph){

  edge_list <- igraph::get.data.frame(graph, what = "edges")

  if("label" %in% names(edge_list)){
    g <- ggplot(edge_list, aes_string(x = "from", y = "to", fill="label"))
  } else {
    g <- ggplot(edge_list, aes_string(x = "from", y = "to"))
  }
      g +
      geom_raster() + theme_bw() +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(drop = FALSE) +
      xlab("") +
      ylab("") +
      theme(
        axis.text = element_blank(),
        aspect.ratio = 1,
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
}


