#' netgen
#'
#' Randomly generate a wide range of interaction networks
#'
#' @param net_size network size (number of nodes)
#' @param average_module average module size
#' @param min_module cutoff for the minimum modules size
#' @param min_submodule cutoff for submodules, used only for bipartite and tripartite networks
#' @param net_type network type, see details
#' @param net_degree average degree of connection
#' @param net_rewire global and local  network rewiring probabilities
#' @param mod_probs module probabilities for first 7 types,
#'   used for constructing mixed networks
#' @param verbose logical, default TRUE. Should a message report summary statistics?
#' @details
#' network type is one of
#' - mixed
#' - random
#' - scalefree
#' - nested
#' - bi-partite nested
#' - bi-partite random
#' - "ttbnr": tri-trophic bipartite nested-random
#' - "ttbnbn": tri-trophic bipartite nested-bipartite nested
#'
#'  NOTE: Function arguments have changed from those netgen 0.1.1 to be more intelligible.
#'  To restore the original function api on code that depends on the old version, you
#'  can simply add:
#'
#'  `netgen <- EcoNetGen:::netgen_v1`
#'
#'  to the top of your code after running `library(EcoNetGen)`.
#'
#' @importFrom igraph graph.data.frame graph_from_adjacency_matrix
#' @importFrom utils read.table
#' @return an `igraph` object
#' @export
#' @useDynLib EcoNetGen, .registration = TRUE
#' @examples
#' \donttest{
#' net <- netgen()
#' adj_plot(net)
#' }
netgen <- function(net_size = 50,
                   average_module = 10,
                   min_module = 0,
                   min_submodule = 0,
                   net_type = c("mixed",
                                "random",
                                "scalefree",
                                "nested",
                                "bi-partite nested",
                                "bi-partite random",
                                "tt-bn-r",
                                "tt-bn-bn"
                                ),
                   net_degree = 10,
                   net_rewire = c(0.3,0.0),
                   mod_probs = 0,
                   verbose = TRUE){

  net_type <- match.arg(net_type)
  net_type <- switch(net_type,
                     "mixed" = 0,
                     "random" = 1,
                     "scalefree" = 2,
                     "nested" = 3,
                     "bi-partite nested" = 41,
                     "bi-partite random" = 42,
                     "tt-bn-r" = 51,
                     "tt-bn-bn" = 52)
  n_modav = c(net_size, average_module)
  cutoffs = c(min_module, min_submodule)

  netgen_v1(n_modav, cutoffs, net_type, net_degree, net_rewire, mod_probs)

}


#' netgen_v1
#'
#' netgen function
#'
#' @param n_modav network size and average module size (integer vector, length 2)
#' @param cutoffs module and submodule minimum sizes (integer vector, length 2).
#'  (submodules are used only for bipartite and tripartite networks)
#' @param net_type integer indicating type, see details
#' @param net_degree average degree of connection
#' @param net_rewire global and local  network rewiring probabilities
#' @param mod_probs module probabilities for types 1 to 51,
#'   used for constructing mixed networks, net_type = 0
#' @param verbose logical, default TRUE. Should a message report summary statistics?
#' @details
#' network type
#' - 0 = mixed
#' - 1 = random
#' - 2 = scalefree
#' - 3 = nested
#' - 41 = bi-partite nested
#' - 42 = bi-partite random
#' - 51 = tri-trophic bipartite nested-random "ttbnr"
#' - 52 = tri-trophic bipartite nested-bipartite nested "ttbnbn"
#' @importFrom igraph graph.data.frame graph_from_adjacency_matrix
#' @importFrom utils read.table
#' @return an `igraph` object
#' @useDynLib EcoNetGen, .registration = TRUE
netgen_v1 <-
  function(
           n_modav = c(50, 10),
           cutoffs = c(3, 0),
           net_type = 1,
           net_degree = 10,
           net_rewire = c(0.3,0.0),
           mod_probs = 0,
           verbose = TRUE) {
    res <- .Fortran(
      "subnetgen",
      output = integer(n_modav[1]^2),
      as.integer(n_modav),
      as.integer(cutoffs),
      as.integer(net_type),
      as.double(net_degree),
      as.double(net_rewire),
      as.double(mod_probs),
      modcount = integer(1L)
    )


    if(verbose){
      cluster_stats(res$output, res$modcount)
    }
    M <- res$output
    M <- matrix(M, sqrt(length(M)))
    igraph::graph_from_adjacency_matrix(M)

  }

cluster_stats <- function(M, modcount){
  n <- sqrt(length(M))
  res <- .Fortran("clusters",
                  a = as.integer(M),
                  n = as.integer(n),
                  maxsize = integer(1L),
                  icount = integer(1L))

  message(paste(
  '\nmodule count =', modcount,
  '\naverage degree =', sum(M) / n,
  '\naverage module size =', n / modcount,
  '\nnumber of components =', res$icount,
  '\nsize of largest component =', res$maxsize))
}

