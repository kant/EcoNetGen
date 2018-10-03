#' netgen
#'
#' Randomly generate a wide range of interaction networks
#'
#' @param net_size network size (number of nodes)
#' @param ave_module_size average module size
#' @param min_module_size cutoff for the minimum modules size
#' @param min_submod_size cutoff for submodules, used only for bipartite and tripartite networks
#' @param net_type network type, see details
#' @param ave_degree average degree of connection
#' @param rewire_prob_global probability any given edge should be rewired
#' @param rewire_prob_local probability that edges within a module should be rewire locally
#'  (within the module)
#' @param mixing_probs module probabilities for first 7 types,
#'   used for constructing mixed networks
#' @param verbose logical, default TRUE. Should a message report summary statistics?
#' @details
#' network type is one of
#' - mixed
#' - random
#' - scalefree
#' - nested
#' - bi-partite nested (or short-hand "bn")
#' - bi-partite random (or short-hand "br")
#' - tri-trophic bipartite nested-random. (Can use short-hand "ttbnr")
#' - tri-trophic bipartite nested-bipartite nested (Can use short-hand "ttbnbn")
#'
#'  **Valid Parameter Ranges**
#'
#'  Please note that not all combinations of parameters will create valid networks.
#'  If an invalid combination is requested, `netgen()` will error with an informative
#'  message.  A list of these constraints is provided below for reference.
#'
#'
#' 1. `net_size >= ave_module_size`. If `net_size = ave_module_size`` the program
#'  generates a network with a single module.
#' 2. `ave_module_size > min_module_size`
#' 3. `ave_degree >= 1`. Preferably larger than 4, to ensure single component modules.
#' 4. `rewire_prob_global = 0` produces completely uncoupled modules. To ensure a single
#'  component network use `rewire_prob_global > 0` and sufficiently large.
#' 5. `rewire_prob_local = 0` produces idealized modules.
#'  Use `rewire_prob_local > 0` to add stochasticity to the modules.
#' 6. For tripartite networks `min_module_size > min_submod_size`.
#'  This also implies `min_module_size >= 2`.
#' 7. For scalefree networks (or mixed networks involving scalefree modules)
#'  `ave_degree < min_module_size`
#' 8. For mixed networks `mixing_probs` need to sum to `1`. If the sum is larger
#'  than one, only the first types, corresponding to `sum <=1`, will be sampled.
#'
#' @importFrom igraph graph.data.frame graph_from_adjacency_matrix
#' @importFrom utils read.table
#' @return an `igraph` object
#' @export
#' @useDynLib EcoNetGen, .registration = TRUE
#' @examples
#' library(EcoNetGen)
#' \donttest{
#' set.seed(12345)
#' net <- netgen()
#' adj_plot(net)
#' }
netgen <- function(net_size = 50,
                   ave_module_size = 10,
                   min_module_size = 6,
                   min_submod_size = 1,
                   net_type = c("mixed",
                                "random",
                                "scalefree",
                                "nested",
                                "bi-partite nested",
                                "bi-partite random",
                                "tri-trophic bipartite nested-random",
                                "tri-trophic bipartite nested-bipartite nested",
                                "bn",
                                "br",
                                "tt-bn-r",
                                "tt-bn-bn"
                                ),
                   ave_degree = 5,
                   rewire_prob_global = 0.2,
                   rewire_prob_local = 0.0,
                   mixing_probs = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.0 ,0.0),
                   verbose = FALSE){

  if(sum(mixing_probs) == 0){
    stop(paste("mixing_probs cannot all be zero"))
  }
  ## Normalize mixing probabilities
  mixing_probs <- mixing_probs / sum(mixing_probs)

  stopifnot(
    ave_module_size > 1,
    min_module_size > 1,
    min_submod_size >= 1,
    rewire_prob_global > 0,
    rewire_prob_global < 1,
    rewire_prob_local < 1,
    length(mixing_probs) == 7
    )

  if(ave_module_size >= net_size)
    stop(paste0("ave_module_size (",
                ave_module_size,
                ") must be less than net_size (",
                net_size, ")"))

  if(min_module_size <= min_submod_size){
    stop(paste0("min_module_size (",
               min_module_size,
               ") is not greater than min_submod_size (",
               min_submod_size, ")"))
  }

  if(ave_module_size < min_module_size){
    stop(paste0("ave_module_size (",
               ave_module_size,
               ") is not greater than or equal to min_module_size (",
               min_module_size, ")"))
  }



  net_type <- match.arg(net_type)
  net_type <- switch(net_type,
                     "mixed" = 0,
                     "random" = 1,
                     "scalefree" = 2,
                     "nested" = 3,
                     "bi-partite nested" = 41,
                     "bi-partite random" = 42,
                     "tri-trophic bipartite nested-random" = 51,
                     "tri-trophic bipartite nested-bipartite nested" = 52,
                     "bn" = 41,
                     "br" = 42,
                     "tt-bn-r" = 51,
                     "tt-bn-bn" = 52,
                     404)
  if(net_type == 404){
    stop("net_type not found")
  }

  if(net_type %in% c(0, 2)){
    if(ave_degree >= min_module_size )
    stop(paste0("Requested ave_degree (", ave_degree,
                ") must be less requested min_module_size (",
                min_module_size,
                ") for 'scalefree' or 'mixed' network types"))
  }

  n_modav <- c(net_size, ave_module_size)
  cutoffs <- c(min_module_size, min_submod_size)
  rewire_probs <- c(rewire_prob_global, rewire_prob_local)
  netgen_v1(n_modav, cutoffs, net_type, ave_degree,
            rewire_probs, mixing_probs, verbose)

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
           mod_probs = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.0 ,0.0),
           verbose = FALSE) {

    stopifnot(length(n_modav) == 2,
              length(cutoffs) == 2,
              length(net_type) == 1,
              length(net_degree) == 1,
              length(mod_probs) == 7)

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

