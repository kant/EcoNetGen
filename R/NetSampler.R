#' Network Sampling Routine
#'
#' @param network_in input network (as igraph object)
#' @param key_nodes_sampler sampling criteria for key nodes. See details.
#' @param neighbors_sampler sampling criteria for neighbors. see details.
#' @param n_key_nodes number of key nodes to sample.
#' @param n_neighbors number of first neighbors or fraction of first neighbors.
#'  See details.
#' @param hidden_modules list of the modules to exclude
#' (max 10 modules; only the first numb_hidden are used)
#' @param module_sizes integer vector giving the size of each module. see details.
#' @param cluster_fn a clustering function, from `igraph::cluster_*`. Default is
#' `igraph::cluster_edge_betweeness`.  Only used to compute module sizes if not
#' provided.
#' @details
#' Algorithm first samples n_key_nodes according the the requested `key_nodes_sampler`
#' criterion.  For each key node, the requested number or fraction of neighbors is
#' then sampled according to the `neighbors_sampler` criterion.  Optionally, a list of
#' modules can be designated as "hidden" and will be excluded from sampling.
#'
#' if `n_neighbors is greater than 1, assumes this is the number to sample.  If
#' `n_neighbors` is between 0 and 1, assumes this is the fration of neighbors to
#' sample.  (To sample 1 neighbor, use an explicit integer, `1L` (or as.`integer(1)`)
#' to sample 100% of neighbors, use a numeric, 1.0.
#'
#' Provide `module_sizes` list to improve performance.  If not provided, this will
#' will be calculated based on `igraph::cluster_edge_betweeness`.  Be sure to
#' provide a `module_sizes` vector whenever calling `netsampler` repeatedly on the
#' same network to avoid unnecessary performance hit from recalculating modules every
#' time.  See examples.
#'
#' @return the original input network (as an igraph network object),
#'  with the attribute `label` added to the edges and vertices indicating
#' if that edge or vertex was `sampled` or `unsampled`.
#'
#' @export
#' @importFrom igraph cluster_edge_betweenness as.undirected groups as_adjacency_matrix
#' delete_edge_attr E sizes
#'
#' @examples
#' \donttest{
#' set.seed(12345)
#' net <- netgen()
#' sample <- netsampler(net)
#'
#' ## Precompute `module_sizes` for replicate sampling of the same network:
#'  library(igraph)
#'  modules <- cluster_edge_betweenness(as.undirected(net))
#'  module_sizes <- vapply(igraph::groups(modules), length, integer(1))
#'  sample <- netsampler(net, module_sizes = module_sizes)
#'
#' }
#'
netsampler <-
  function(network_in,
           key_nodes_sampler = c("random", "lognormal", "Fisher log series",
                               "exponential", "degree", "module"),
           neighbors_sampler = c("random", "exponential"),
           n_key_nodes = 10,
           n_neighbors = 0.5,
           hidden_modules = NULL,
           module_sizes = NULL,
           cluster_fn = igraph::cluster_edge_betweenness
           ) {

    key_nodes_sampler <- match.arg(key_nodes_sampler)
    neighbors_sampler <- match.arg(neighbors_sampler)

    crit <- c(which(c("random", "lognormal", "Fisher log series",
                      "exponential", "degree", "module") ==
                    key_nodes_sampler) - 1,
              which(c("random", "exponential") == neighbors_sampler) - 1
    )

    #initial, final, delta,  final & delta not used.
    key_nodes <- c(n_key_nodes, 0, 0, 0)

    if(is.integer(n_neighbors)){
      if(n_neighbors == 1L)
        n_neighbors <- 1.1
    }

    anfn <- n_neighbors

    numb_hidden <- length(hidden_modules)

    ## NOTE: if module_sizes are not provided, then we calculate
    ## module sizes on the fly, though the FORTRAN netgen code
    ## returns these. Not clear what clustering algorithm
    ## Marcus's FORTRAN routine uses for that. This has some
    ## additional performance overhead; in particular, code
    ## seeking to create many resamplings from the same network
    ## should be sure to include module sizes list.

    if(is.null(module_sizes)){
      community <- cluster_fn(
        igraph::as.undirected(network_in))
      module_sizes <- igraph::sizes(community)
    }

    ## Convert igraph to integer vector
    net <-
      as.integer(
        as.matrix(igraph::as_adjacency_matrix(network_in)))

    ## number of nodes
    n <- as.integer(sqrt(length(net)))

    res <- .Fortran(
      "subsampling",
      input = as.integer(net),
      out = integer(n^2),
      as.integer(crit),
      as.integer(key_nodes),
      as.double(anfn),
      as.integer(numb_hidden),
      as.integer(hidden_modules),
      as.integer(n),
      as.integer(module_sizes),
      as.integer(length(module_sizes)),
      nodes_sampled = integer(n),
      edges_sampled = integer(n^2)
    )


    M <- res$edges_sampled
    M <- matrix(M, sqrt(length(M)))
    out <- igraph::graph_from_adjacency_matrix(M, weighted = TRUE)

    labs <- c("unsampled", "sampled")
    igraph::E(out)$label <- labs[igraph::E(out)$weight]
    out <- igraph::delete_edge_attr(out, "weight")

    node_labels <- labs[1 + as.integer(res$nodes_sampled > 0)]
    igraph::V(out)$label <- node_labels

    out
}



#    library(ggraph)
#    ggraph(out, layout = 'kk') +
#      geom_edge_link(aes(colour = label)) +
#      geom_node_point(aes(colour = label))


#sampled <- subgraph.edges(out, E(out)[sampled=="sampled"])
#ggraph(sampled, layout = 'kk') +
#        geom_edge_link(aes(colour = label)) +
#        geom_node_point(aes(colour = label))




