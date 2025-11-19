test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Call GetSetting", {
  expect_equal(GetSetting('global', 'date_format'), '%m-%d-%Y')
  expect_equal(GetSetting('global', group = 'time'), c('America/Edmonton', '%m-%d-%Y', "%m-%d-%Y %H:%M"))
})
