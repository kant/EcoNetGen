testthat::context("netsampler")

testthat::test_that("we can run netsample",{

  library(EcoNetGen)
  net <- netgen()
  testthat::expect_is(net, "igraph")
  res <- netsample(net)
  testthat::expect_is(res, "igraph")

})


testthat::test_that("net sample works with other settings", {

  network_in <- netgen()
  sn <- netsample(network_in,
                  module_sizes = NULL,
                  crit = c(1,1))
  testthat::expect_is(sn, "igraph")
  sn <- netsample(network_in,
                  anfn = 3)
  testthat::expect_is(sn, "igraph")
  ## sample by fisher log
  sn <- netsample(network_in,
                  module_sizes = NULL,
                  crit = c(2,0))
  testthat::expect_is(sn, "igraph")

  ## sampel by exp
  sn <- netsample(network_in,
                  module_sizes = NULL,
                  crit = c(3,0))
  testthat::expect_is(sn, "igraph")

  ## sample by degree
  sn <- netsample(network_in,
                  module_sizes = NULL,
                  crit = c(4,0))
  testthat::expect_is(sn, "igraph")

  ## sample by module
  sn <- netsample(network_in,
                  module_sizes = NULL,
                  crit = c(5,0))
  testthat::expect_is(sn, "igraph")

})

testthat::test_that("we can run netsample",{
      library(EcoNetGen)
      library(igraph)

      network_in <- netgen()
      module_sizes <- NULL
      crit <- c(1,0)
      key_nodes <- c(10, 50, 10, 1000)
      anfn <- 0.5
      numb_hidden <- 0
      hidden_modules <- c(1,5,6,0,0,0,0,0,0,0)
      community <- igraph::cluster_edge_betweenness(
        igraph::as.undirected(network_in))
      module_sizes <- vapply(igraph::groups(community),
                             length, integer(1))
      net <- as.integer(as.matrix(igraph::as_adjacency_matrix(network_in)))
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
      igraph::E(out)$sampled <- labs[igraph::E(out)$weight]

      node_labels <- labs[1+as.integer(res$nodes_sampled > 0)]
      igraph::V(out)$sampled <- node_labels



      ## if treat both sampled (2s) and unsampled (1s) edges as 1s...
      tmp <- as.integer(res$edges_sampled > 0)
      ## then input should be same as output network
      testthat::expect_identical(res$input, tmp)


      ## Test that sampled network and subset network match:
      int_to_igraph <- function(x, ...){
        M <- matrix(x, sqrt(length(x)))
        igraph::graph_from_adjacency_matrix(M, ...)
      }
      s <- int_to_igraph(res$out)
      fortran_sampled <- subgraph.edges(s, E(s))

      ## Subset the sampled network:
      sampled <- subgraph.edges(out, E(out)[sampled=="sampled"])

      ## How do we compare these? Ideally something stricted than this, but oh well:
      testthat::expect_equal(length(igraph::E(fortran_sampled)),
                             length(igraph::E(sampled)))

      ## Visual inspection with ggraph:
      # ggraph(sampled,layout = "kk") + geom_edge_link()
      # ggraph(fortran_sampled,layout = "kk") + geom_edge_link()

})



