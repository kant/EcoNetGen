testthat::context("netsampler")

testthat::test_that("we can run netsample",{

  library(EcoNetGen)
  net <- netgen()
  testthat::expect_is(net, "igraph")
  res <- netsample(net)
})


testthat::test_that("we can run netsample",{

      network_in <- netgen()
      module_sizes = NULL
      crit = c(1,0)
      key_nodes = c(10, 50, 10, 1000)
      anfn = 0.5
      numb_hidden = 0
      hidden_modules = c(1,5,6,0,0,0,0,0,0,0)
      community <- igraph::cluster_edge_betweenness(igraph::as.undirected(network_in))
      module_sizes <- vapply(igraph::groups(community), length, integer(1))
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

      ## if treat both sampled (2s) and unsampled (1s) edges as 1s...
      out <- as.integer(res$edges_sampled > 0)
      ## then input should be same as output network
      expect.identical(res$input, out)
})
