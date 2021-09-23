test_that("multiplication works", {

  #TODO: Missing tests with `shift_by` being negative.

  # DEBUG / TESTING
  #' assume that there are always 10 non zero elements, unless
  #' the shifting is beyond the 305 threshold
  shift_extend_inflation_scc(1:10, shift_by = 0)  ->
    result
  expect_equal(sum(result > 0), 10)
  expect_equal(result %>% length(), 305)
  shift_extend_inflation_scc(1:10, shift_by = 1)  ->
    result

  expect_equal(sum(result > 0), 10)
  expect_equal(result %>% length(), 305)
  shift_extend_inflation_scc(1:10, shift_by = 2)  ->
    result
  expect_equal(sum(result > 0), 10)
  expect_equal(result %>% length(), 305)

  shift_extend_inflation_scc(1:10, shift_by = 300)  ->
    result
  expect_equal(sum(result > 0), 5)
  expect_equal(result %>% length(), 305)


  #shifted beyond range
  shift_extend_inflation_scc(1:10, shift_by = 305)  ->
    result
  expect_equal(sum(result > 0), 0)
  expect_equal(result %>% length(), 305)

  shift_extend_inflation_scc(1:10, shift_by = 306)  ->
    result
  expect_equal(sum(result > 0), 0)
  expect_equal(result %>% length(), 305)
})
