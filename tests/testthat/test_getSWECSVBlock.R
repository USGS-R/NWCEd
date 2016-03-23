context("Test SWE CSV Data Parses")

test_that("Test that the function runs without errors", {
  outData<-getSWECSVBlock('data/SWECSVBlock_daymet_example.xml')
  expect_equal(length(outData),2)
  expect_equal(length(outData$data),12784)
  expect_equal(round(mean(outData$data),digits = 4) ,3.5134)
})
