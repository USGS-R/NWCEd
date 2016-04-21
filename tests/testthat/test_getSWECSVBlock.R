context("Test SWE CSV Data Parses")

test_that("Test that the function runs without errors", {
  outData<-getSWECSVBlock(system.file('extdata','SWECSVBlock_daymet_example.xml',package='NWCEd'))
  expect_equal(length(outData),2)
  expect_equal(length(outData$data),12784)
  expect_equal(round(mean(outData$data),digits = 4) ,3.5134)
})

test_that("xml exception is handled.", {
  expect_error(getSWECSVBlock(system.file('extdata','exception.xml',package='NWCEd')),
               'An invalid parameter error was encountered. The HUC may not exist.')
})
