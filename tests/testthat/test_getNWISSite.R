context("Test Getting an NWIS Site")

test_that("Test that the function returns the expected NWIS site.", {
  expect_equal(getNWISSite("121101110705"), "08211500")
})

test_that("Test that the function returns NULL otherwise.", {
  expect_null(getNWISSite("12110111070"))
})
