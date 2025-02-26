test_that("sum_rat correctly calculates counts and ratios", {
  # Sample data
  df <- tibble(category = c("A", "A", "B", "B", "B", "C"))

  # Expected result
  expected <- tibble(
    category = c("A", "B", "C"),
    Count = c(2, 3, 1),
    Ratio = c(2/6, 3/6, 1/6)
  )

  # Run function
  result <- df %>% group_by(category) %>% sum_rat(., threshold = NULL)

  # Check values
  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("sum_rat applies threshold filtering correctly", {
  df <- tibble(category = c("A", "A", "B", "B", "B", "C"))

  # Run function with a threshold of 0.2 (removes "C")
  result <- df %>% group_by(category) %>% sum_rat(., threshold = 0.2)

  # Expected result (without "C")
  expected <- tibble(
    category = c("A", "B"),
    Count = c(2, 3),
    Ratio = c(2/6, 3/6)
  )

  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("sum_rat handles empty data correctly", {
  df <- tibble(category = character())

  result <- result <- df %>% group_by(category) %>% sum_rat()

  expect_equal(nrow(result), 0)  # Should return an empty tibble
})

test_that("sum_rat works with multiple columns", {
  df <- tibble(group = c("X", "X", "Y", "Y", "Y", "Z"), category = c(1, 1, 2, 2, 2, 3))

  result <-  df %>% group_by(group, category) %>% sum_rat(., threshold = 0.1)

  expect_true("group" %in% colnames(result))
  expect_true("category" %in% colnames(result))
  expect_true("Count" %in% colnames(result))
  expect_true("Ratio" %in% colnames(result))
})
