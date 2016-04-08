context("Test Data Downloads")

test_that("Test that the function returns the expected number of time series", {
  outData<-getNWCData("031601030306")
  expect_equal(length(names(outData)),2)
  outData<-getNWCData("031601030306", local=FALSE)
  expect_equal(length(names(outData)),3)
  outData<-getNWCData("03160103")
  expect_equal(length(names(outData)),2)
})
