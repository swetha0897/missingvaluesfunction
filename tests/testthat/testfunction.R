library(testthat)
test_that("count_all_missing_by_group works correctly", {

  # Test with a simple dataset
  test_data <- data.frame(
    group = c("A", "A", "B", "B", "C"),
    val1 = c(1, NA, 3, NA, 5),
    val2 = c(NA, 2, NA, 4, NA)
  )

  result <- count_all_missing_by_group(test_data, "group")

  # Check structure of result
  expect_equal(nrow(result),3)
  expect_equal(ncol(result),3)
  expect_equal(colnames(result), c("group", "val1", "val2"))

  # Check counts
  expect_equal(result$val1, c('1','1','0'))
  expect_equal(result$val2, c('1','1','1'))

  # Test with no missing values
  test_data2 <- data.frame(
    group = c("X", "X", "Y", "Y"),
    val = c(1, 2, 3, 4)
  )

  result2 <- count_all_missing_by_group(test_data2, "group")

  expect_equal(nrow(result2),2)
  expect_equal(ncol(result2), 2)
  expect_equal(result2$val, c('0','0'))

  # Test with all missing values in one group
  test_data3 <- data.frame(
    group = c("P", "P", "Q", "Q"),
    val = c(NA,NA,1,2)
  )

  result3 <- count_all_missing_by_group(test_data3, "group")

  expect_equal(result3$val, c('2','0'))

  # Test with tibble input
  if (requireNamespace("tibble", quietly = TRUE)) {
    test_tibble <- tibble::tibble(
      group = c("A", "A", "B", "B"),
      val = c(1,NA,3,NA)
    )

    result_tibble <- count_all_missing_by_group(test_tibble, "group")

    expect_s3_class(result_tibble, "data.frame")
    expect_equal(result_tibble$val, c('1','1'))
  }

})
