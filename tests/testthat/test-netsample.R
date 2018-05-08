testthat::context("netsampler")

testthat::test_that("we can run netsample",{

  library(EcoNetGen)
  net <- netgen()
  testthat::expect_is(net, "igraph")
  res <- netsampler(net)
  testthat::expect_is(res, "igraph")

  p1 <- adj_plot(res)
  testthat::expect_is(p1, "ggplot")
})


testthat::test_that("net sample works with other settings", {

  network_in <- netgen()
  sn <- netsampler(network_in,
                 key_nodes_sampler = "lognormal",
                 neighbors_sampler = "exponential")
  testthat::expect_is(sn, "igraph")

  sn <- netsampler(network_in,
                  n_neighbors = 3)
  testthat::expect_is(sn, "igraph")

  sn <- netsampler(network_in,
                  key_nodes_sampler = "Fisher")
  testthat::expect_is(sn, "igraph")

  sn <- netsampler(network_in,
                  key_nodes_sampler = "exponential")
  testthat::expect_is(sn, "igraph")

  sn <- netsampler(network_in,
                  key_nodes_sampler = "degree")
  testthat::expect_is(sn, "igraph")

  sn <- netsampler(network_in, "module")
  testthat::expect_is(sn, "igraph")

})

testthat::test_that("we can run netsample",{
      library(EcoNetGen)
      library(igraph)

      network_in <- netgen()
      out <- netsampler(network_in)

      ## Subset the sampled network:
      sampled <- igraph::subgraph.edges(out, E(out)[label=="sampled"])

      testthat::expect_is(out, "igraph")
      testthat::expect_is(sampled, "igraph")




  #    ## if treat both sampled (2s) and unsampled (1s) edges as 1s...
  #    tmp <- as.integer(res$edges_sampled > 0)
  #    ## then input should be same as output network
  #    testthat::expect_identical(res$input, tmp)


  #    ## Test that sampled network and subset network match:
  #    int_to_igraph <- function(x, ...){
  #      M <- matrix(x, sqrt(length(x)))
  #      igraph::graph_from_adjacency_matrix(M, ...)
  #    }
  #    s <- int_to_igraph(res$out)
  #    fortran_sampled <- subgraph.edges(s, E(s))

      ## How do we compare these? Ideally something stricted than this, but oh well:
   #   testthat::expect_equal(length(igraph::E(fortran_sampled)),
   #                           length(igraph::E(sampled)))

      ## Visual inspection with ggraph:
      # ggraph(sampled,layout = "kk") + geom_edge_link()
      # ggraph(fortran_sampled,layout = "kk") + geom_edge_link()

})



