# Create sample data for testing
df <- data.frame(
  Category = c("A", "A", "B", "B", "C", "C", "A"),
  Subcategory = c("X", "Y", "X", "Y", "X", "Y", "X"),
  Value = c(1, 2, 3, 4, 5, 6, 7)
)

# Test 1: Basic functionality
test_that("sum_rat calculates counts and ratios correctly", {
  result <- sum_rat(df, Category, Subcategory)

  # Check column names
  expect_true(all(c("Category", "Subcategory", "Count", "Sum", "Ratio") %in% colnames(result)))

  # Check if count sums to expected total per category
  expect_equal(result$Sum[result$Category == "A"][1], 3)  # "A" has 3 rows in total

  # Check ratio calculations
  ratio_check <- result$Count / result$Sum
  expect_equal(result$Ratio, ratio_check)  # Ratios should be correctly computed
})

# Test 2: Threshold filtering
test_that("sum_rat correctly filters rows based on threshold", {
  result <- sum_rat(df, Category, Subcategory, threshold = 0.5)

  # All ratios should be greater than 0.5
  expect_true(all(result$Ratio > 0.5))
})

# Test 3: Handling non-existent columns
test_that("sum_rat stops if an invalid column is provided", {
  expect_error(sum_rat(df, NonexistentColumn), "is not a column name")
})

# Test 4: Order of output
test_that("sum_rat sorts by first group and descending ratio", {
  result <- sum_rat(df, Category, Subcategory)

  # Check that it's sorted by Category first
  expect_true(is.unsorted(result$Category) == FALSE)

  # Check that within each category, Ratio is in descending order
  grouped <- split(result, result$Category)
  for (group in grouped) {
    expect_true(is.unsorted(-group$Ratio) == FALSE)
  }
})

# Test 5: Function works with quoted column names
test_that("sum_rat works with quoted column names", {
  result <- sum_rat(df, "Category", "Subcategory")

  expect_true(all(c("Category", "Subcategory", "Count", "Sum", "Ratio") %in% colnames(result)))
})
