context("Test Daily to Annual Conversion")

test_that("Test that the function returns the right number of years, structure, and default function.", {
  outData<-getSWECSVBlock('data/SWECSVBlock_daymet_example.xml')
  annual<-annualize(outData)
  expect_equal(length(annual),2)
  expect_equal(length(annual$data),35)
  expect_equal(names(annual) ,c("year","data"))
  expect_equal(signif(mean(annual$data),digits=4),3.515) # make sure things haven't changed over time
  expect_equal(annual$data[1],mean(outData$data[1:366]))
})

test_that("Test that the function returns sum when prompted.", {
  outData<-getSWECSVBlock('data/SWECSVBlock_daymet_example.xml')
  annual<-annualize(outData, method=sum)
  expect_equal(length(annual),2)
  expect_equal(length(annual$data),35)
  expect_equal(names(annual) ,c("year","data"))
  expect_equal(signif(mean(annual$data),digits=5),1283.3) # make sure things haven't changed over time
  expect_equal(annual$data[1],sum(outData$data[1:366]))
})
