library("wodds")

test_that("select_wodd_name_from_table(3)", {
  expect_equal(wodds::select_wodd_name_from_table(3L), c("M", "F", "E"))
})

test_that("wodds::select_wodd_name_from_table(300)", {
    expect_error( wodds::select_wodd_name_from_table(300L) )
})

test_that("wodds::get_depth_from_n(n=15734L, alpha = 0.05)", {
  expect_equal(wodds::get_depth_from_n(n=15734L, alpha = 0.05), 11L)
})

test_that("wodds::get_n_from_depth(d = 11L, conservative = TRUE))", {
  expect_equal(floor(wodds::get_n_from_depth(d = 11L, conservative = TRUE)),15734)
})
