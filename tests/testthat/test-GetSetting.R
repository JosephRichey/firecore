test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Call GetSetting", {
  expect_equal(GetSetting(), 'Hello from GetSetting')
})
