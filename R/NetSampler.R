#' Network Sampling Routine
#'
#' @param network_in input network (as igraph object)
#' @param module_sizes integer vector giving the size of each module (optional)
#' @param crit sampling criteria for key nodes and neighbors, see details
#' @param key_nodes number of key nodes to sample, from mi to mf at steps of delta-m
#' and number of realizations nr mi, mf, delta-m, nr
#' @param anfn  number of first neighbors or fraction of first neighbors to include, see details
#' @param numb_hidden number of modules to exclude
#' @param hidden_modules list of modules to exclude (max 10 modules; only the first numb_hidden are used)
#' @details afn argument can be written in numerous ways:
#' - if 0 < anfn <= 1 it is interpreted as a fraction of the total number of neighbors
#' - otherwise as the number of neighbors
#' - to add all neighbors use 1.0
#' - to add 1 neighbor per node use 1.1
#' - to add 2 neighbours use 2, etc
#'
#' sampling criteria for key nodes and neighbors
#'
#' criterion for key nodes
#'
#' - 0 for random
#' - 1 for lognormal
#' - 2 for Fisher log series
#' - 3 for exponential
#' - 4 for degree
#' - 5 for module
#'
#' criterion for neighbors
#'
#' - 0 = random
#' - 1 = weighted according to exponential distribution
#'
#' @return  output files are:
#' - out_name -- main output file with info on the sunetwork
#' - abund.txt / degree.txt / module.txt
#' - subnet.txt  -- sampled network
#' - netnodes.txt -- nodes in red or blue if belong subnet or not
#' - netlinks.txt --  links in red or blue if connect subnet or not
#' @export
#' @importFrom igraph cluster_edge_betweenness as.undirected groups as_adjacency_matrix
#' delete_edge_attr E
netsample <-
  function(network_in,
           crit = c(1,0),
           key_nodes = c(10, 50, 10, 1000),
           anfn = 0.5,
           numb_hidden = 0,
           hidden_modules = c(0,0,0,0,0,0,0,0,0,0),
           module_sizes = NULL
           ) {


    ## NOTE: if module_sizes are not provided, then we calculate
    ## module sizes on the fly, though the FORTRAN netgen code
    ## returns these. Not clear what clustering algorithm
    ## Marcus's FORTRAN routine uses for that. This has some
    ## additional performance overhead; in particular, code
    ## seeking to create many resamplings from the same network
    ## should be sure to include module sizes list.

    if(is.null(module_sizes)){
      community <- igraph::cluster_edge_betweenness(
        igraph::as.undirected(network_in))
      module_sizes <- vapply(igraph::groups(community),
                             length, integer(1))
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
      as.single(anfn),
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




