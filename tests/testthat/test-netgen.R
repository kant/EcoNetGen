library(testthat)
testthat::context("netgen")
set.seed(12345)

test_that("we can create a random type network", {
  #skip_if(R.Version()$arch == "i386")

  net <- netgen()
  expect_is(net, "igraph")

})

netgen_v1 <- EcoNetGen:::netgen_v1

test_that("we can create a random type network (v1)", {
  #skip_if(R.Version()$arch == "i386")

  net <- netgen_v1()
  expect_is(net, "igraph")

})

test_that("we can create a mixed type network", {

  n_modav <- c(150,30)
  cutoffs <- c(15,5)
  net_type <- 0
  net_degree <- 10.0
  net_rewire <- c(0.07,0.2)
  mod_probs <- c(0.2, 0.0, 0.3, 0.3, 0.2, 0.0, 0.0)

  M <- netgen_v1(n_modav,cutoffs,net_type,net_degree,net_rewire,mod_probs)

  expect_is(M, "igraph")
  p1 <- adj_plot(M)
  expect_is(p1, "ggplot")

  })


test_that("we can create all types of network", {

        n_modav <- c(150,20)
        cutoffs <- c(15,5)
        net_type <- 0
        net_degree <- 10.0
        net_rewire <- c(0.07,0.2)
        mod_probs <- c(0.2, 0.0, 0.3, 0.3, 0.2, 0.0, 0.0)

        M <- netgen_v1(n_modav,cutoffs,1,net_degree,net_rewire,mod_probs)
        expect_is(M, "igraph")
        M <- netgen_v1(n_modav,cutoffs,2,net_degree,net_rewire,mod_probs)
        expect_is(M, "igraph")
        M <- netgen_v1(n_modav,cutoffs,3,net_degree,net_rewire,mod_probs)
        expect_is(M, "igraph")
        M <- netgen_v1(n_modav,cutoffs,41,net_degree,net_rewire,mod_probs)
        expect_is(M, "igraph")
        M <- netgen_v1(n_modav,cutoffs,51,net_degree,net_rewire,mod_probs)
        expect_is(M, "igraph")
        M <- netgen_v1(n_modav,cutoffs,42,net_degree,net_rewire,mod_probs)
        expect_is(M, "igraph")
        M <- netgen_v1(n_modav,cutoffs,52,net_degree,net_rewire,mod_probs)
        expect_is(M, "igraph")

})

test_that("setting seed creates a reproducible network", {

  set.seed(5555555)
  M <- netgen_v1()
  set.seed(5555555)
  M2 <- netgen_v1()

  expect_identical(igraph::as_adj(M), igraph::as_adj(M2))

})



