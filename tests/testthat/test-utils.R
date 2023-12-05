# Test .extract_varclass internal -----------------------------------

test_that("test error messaging", {
  test <- ggoncoprint::alteration_data
  attr(test, "var_class") <- NULL

  expect_error(.extract_varclass(test))

  expect_error(.extract_varclass(ggoncoprint::alteration_data), NA)
})

test_that("test object returned", {
  obj <- .extract_varclass(ggoncoprint::alteration_data)

  expect_true("data.frame" %in% class(obj))
  expect_true(all(c("hugo_symbol", "sample_id", "var_class") %in% names(obj)))
  expect_true(all(obj$sample_id %in% ggoncoprint::alteration_data$sample_id))
})
