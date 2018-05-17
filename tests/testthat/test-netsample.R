library(testthat)
testthat::context("netsampler")

set.seed(12345)
netgen_v1 <- EcoNetGen:::netgen_v1

testthat::test_that("we can run netsampler", {

  library(EcoNetGen)
  net <- netgen_v1()
  testthat::expect_is(net, "igraph")
  res <- netsampler(net)
  testthat::expect_is(res, "igraph")

  p1 <- adj_plot(res)
  testthat::expect_is(p1, "ggplot")
})


testthat::test_that("net sample works with other settings", {

  network_in <- netgen_v1()
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

      network_in <- netgen_v1()
      out <- netsampler(network_in)

      ## Subset the sampled network:
      sampled <- igraph::subgraph.edges(out, E(out)[label=="sampled"])

      testthat::expect_is(out, "igraph")
      testthat::expect_is(sampled, "igraph")



})



