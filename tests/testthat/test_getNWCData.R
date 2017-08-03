context("Test Data Downloads")

test_that("Test that the function returns the expected number of time series", {
  outData<-getNWCData("031601030306")
  expect_equal(length(names(outData)),2)
  outData<-getNWCData("031601030306", local=FALSE)
  expect_equal(length(names(outData)),3)
  expect_true(length(outData$streamflow)>0)
  outData<-getNWCData("03160103")
  expect_equal(length(names(outData)),2)
})

test_that("Test that a HUC with NWIS data comes back as such.",{
  outData<-getNWCData("121101110705", local=FALSE)
  expect_equal(length(names(outData$streamflow)),5)
})

test_that('we get back some geojson when calling the getNWCWatershed function',{
  outData<-getNWCWatershed("031601030306",local=TRUE)
  expect_gt(length(outData$features[[1]]$geometry$coordinates[[1]][[1]]),100)
  expect_equal(outData$type,"FeatureCollection")
})

test_that("we get back some geojson for a huc08 watershed", {
  outData<-getNWCWatershed("03160103",local=TRUE)
  expect_gt(length(outData$features[[1]]$geometry$coordinates[[1]][[1]]),1000)
  expect_equal(outData$type,"FeatureCollection")
})
