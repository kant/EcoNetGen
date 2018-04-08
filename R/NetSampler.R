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
netsample <-
  function(network_in,
           module_sizes = NULL,
           crit = c(1,0),
           key_nodes = c(10, 50, 10, 1000),
           anfn = 0.5,
           numb_hidden = 0,
           hidden_modules = c(1,5,6,0,0,0,0,0,0,0)
           ) {



    if(is.null(module_sizes)){
      community <- igraph::cluster_edge_betweenness(igraph::as.undirected(network_in))
      module_sizes <- vapply(igraph::groups(community), length, integer(1))
    }

    ## Convert igraph to integer vector
    net <- as.integer(as.matrix(igraph::as_adjacency_matrix(network_in)))

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

    #M <- res$out[1 : res$n_sampled]
    #M <- matrix(M, sqrt(length(M)))
    #s <- igraph::graph_from_adjacency_matrix(M)

    #
    res$nodes_sampled

    # 2 = sampled, 1 = not sampled, 0 = no link
    res$edges_sampled

    res
  }


# the output file has eleven columns with the following results:
# m  ssn  slc  rslc  hn  ncomp  V-ssn  V-slc   V-rslc  V-hn  V-ncomp
#
#where
#
# m = number of key nodes
# ssn = average size of sampled network
# slc = average size of largest connected component
# rslc = average size of largest connected component / ssn
# hn = average number of hidden nodes found
# ncomp = average number of components of sampled networks
# V-ssn = variance of size of sampled network
# V-slc = variance of size of largest connected component
# V-rslc = variance of size of largest connected component / ssn
# V-hn = variance of number of hidden nodes found
# V-ncomp = variance of number of components of sampled networks

