context("Test Data Downloads")

test_that("Test that the function runs without errors", {
  outData<-getNWCData("031601030306")
  expect_equal(length(names(outData)),3)
})
