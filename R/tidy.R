#' Summarize Count and Calculate Ratio, and Optionally Filter by Ratio
#'
#' This function takes a dataset and calculates the count of rows for each group,
#' then computes the ratio of the count to the total count across all groups.
#' The function also allows filtering the results based on a user-specified ratio threshold.
#'
#' @param data A data frame or tibble, typically grouped by one or more variables.
#' @param threshold Numeric value for filtering the ratio.
#'                        Default is 0.01
#'                        Only rows with `Ratio` greater than this value are returned.
#' @return A tibble with two columns:
#'   \item{Count}{The number of rows in each group.}
#'   \item{Ratio}{The ratio of the count to the total count of all groups.}
#' @import dplyr
#' @importFrom tibble tibble
#' @export
#' @examples
#' library(tibble)
#' library(dplyr)
#' data <- tibble(category = c("A", "A", "B", "B", "C"))
#' result <- data %>%
#'   group_by(category) %>%
#'   sum_rat()
#' print(result)
sum_rat <- function(data, threshold = 0.01) {
  Ratio <- Count <- NULL
  result <- data %>%
    summarise(Count = n()) %>%                # Count the rows
    mutate(Ratio = Count / sum(Count))         # Divide each count by the total sum of counts

  # Filter by ratio if threshold is provided
  if (!is.null(threshold)) {
    result <- result %>% filter(Ratio > threshold)
  }

  return(result)
}
