testthat::context("netsampler")

testthat::test_that("we can run netsample",{

  library(EcoNetGen)
  net <- netgen()
  testthat::expect_is(net, "igraph")
  ex <- netsample(net)
})
