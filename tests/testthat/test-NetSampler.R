testthat::context("netsampler")

testthat::test_that("we can run netsample",{

  net <- netgen()
  testthat::expect_is(net, "igraph")
  netsample(net)
})
