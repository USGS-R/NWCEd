context("Test Daily to Annual Conversion")

test_that("Test that the function returns the right number of years.", {
  outData<-getSWECSVBlock('data/SWECSVBlock_daymet_example.xml')
  annual<-annualize(outData)
  expect_equal(length(annual),2)
  expect_equal(length(annual$data),35)
  expect_equal(names(annual) ,c("year","data"))
})
