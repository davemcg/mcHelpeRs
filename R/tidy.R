#' Summarize Count and Calculate Ratio, and Optionally Filter by Ratio
#'
#' This function takes a data frame and calculates the count of rows for each
#' user given group, then computes the ratio of the count to the total count across all groups.
#' The function also allows filtering the results based on a user-specified ratio threshold.
#'
#' @param data A data frame or tibble, typically grouped by one or more variables.
#' @param ... One or more column names (unquoted or quoted) to group by.
#' @param threshold Numeric value for filtering the ratio.
#'                        Default is 0.01
#'                        Only rows with `Ratio` greater than this value are returned.
#' @return A tibble with two columns:
#'   \item{Count}{The number of rows in each group.}
#'   \item{Sum}{The number of rows for the first grouping value.}
#'   \item{Ratio}{The ratio of the count to the total count of all groups.}
#' @import dplyr
#' @importFrom tibble tibble
#' @export
#' @examples
#' df <- data.frame(
#'   Category = c("A", "A", "B", "B", "C", "C"),
#'   Subcategory = c("X", "Y", "X", "Y", "X", "Y"),
#'   Value = c(1, 2, 3, 4, 5, 6)
#' )
#'
#' sum_rat(df, Category, Subcategory, threshold = 0.2)
#' sum_rat(df, "Category", "Subcategory", threshold = 0.2)
#'
sum_rat <- function(data, ..., threshold = 0.01) {
  Sum <- Ratio <- Count <- NULL
  groupings <- ensyms(...)

  # Ensure all groupings exist in the dataset
  missing <- setdiff(as.character(groupings), colnames(data))
  if (length(missing) >= 1) {
    stop(paste(missing, "is not a column name."))
  }

  sum_vals <- data %>%
    group_by(!!groupings[[1]]) %>%
    summarise(Sum = n())

  result <- data %>%
    group_by(!!!groupings) %>%
    summarise(Count = n()) %>% # Count the rows
    left_join(sum_vals, by = as.character(groupings[[1]])) %>% # join by the sum values of the first grouping
    mutate(Ratio = Count / Sum) %>%
    arrange(!!groupings[[1]], desc(Ratio)) # Order by first grouping, then descending Ratio

  # Filter by ratio if threshold is provided
  if (!is.null(threshold)) {
    result <- result %>% filter(Ratio > threshold)
  }

  return(result)
}


#' Create a Custom Color or Fill Scale for ggplot2
#'
#' This function converts a specified column in a data frame to a factor, assigns custom colors
#' to its unique levels, and returns either a `scale_colour_manual()` or `scale_fill_manual()`
#' object for use in ggplot2.
#'
#' @param data A data frame containing the column to be factored.
#' @param column_name A string specifying the column name to be factored.
#' @param custom_colors A vector of colors to be assigned to the factor levels.
#'                      The number of colors must be at least as many as the unique levels.
#' @param scale_type A string specifying the type of scale to return.
#'                   Accepts `"color"`, `"colour"`, or `"fill"`.
#'
#' @return A `scale_colour_manual()` or `scale_fill_manual()` object for ggplot2.
#' @importFrom ggplot2 scale_colour_manual scale_fill_manual
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(Diff = c("Low", "Medium", "High"))
#' custom_colors <- c("#E5614CFF", "#8C57A2FF", "#358DB9FF")
#' col_scale <- create_col_scale(df, "Diff", custom_colors, "color")
#' ggplot(df, aes(x = Diff, y = 1, color = Diff)) +
#'   geom_point() +
#'   col_scale
#'
#' fill_scale <- create_col_scale(df, "Diff", custom_colors, "fill")
#' ggplot(df, aes(x = Diff, y = 1, fill = Diff)) +
#'   geom_bar(stat = "identity") +
#'   fill_scale
create_col_scale <- function(data, column_name, custom_colors, scale_type = "color") {
  data[[column_name]] <- factor(data[[column_name]], levels = unique(data[[column_name]]))
  factor_levels <- levels(data[[column_name]])

  if (length(custom_colors) < length(factor_levels)) {
    stop("Error: Not enough colors provided for the number of factor levels.")
  }

  names(custom_colors) <- factor_levels

  scale_type <- tolower(scale_type) # Normalize input

  if (scale_type %in% c("color", "colour")) {
    return(scale_colour_manual(name = column_name, values = custom_colors))
  } else if (scale_type == "fill") {
    return(scale_fill_manual(name = column_name, values = custom_colors))
  } else {
    stop('Error: Invalid scale_type. Use "color", "colour", or "fill".')
  }
}
