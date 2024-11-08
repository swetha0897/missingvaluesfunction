#' To count the missing values for all columns by group
#'
#' @description
#' Given a data frame `data` and a column `group`, this function creates
#' a new data frame with one row per level of `group`. The first column
#' contains the levels of `group`, and the rest of the columns contain
#' the number of missing values for all columns in `data` except `group`.
#'
#' @param data Data in data.frame or tibble format
#' @param group_col Column in `data` to group by (as a string)
#'
#' @return A data frame with the levels of `group`, and the number of NAs
#'   within each level of `group` for all columns in `data` except `group`.
#'
#' @examples
#' # Example 1: Count missing values by Month
#' count_all_missing_by_group(airquality, "Month")
#'
#' # Example 2: Count missing values by Wind (grouped by wind speeds)
#' # For this example, we first create a new column to categorize `Wind` speeds
#' airquality$Wind_category <- cut(airquality$Wind, breaks=c(0, 5, 10, 15, 20),
#'                                 labels=c("Low", "Medium", "High", "Very High"))
#' count_all_missing_by_group(airquality, "Wind_category")
#'
#' # Example 3: Count missing values by Temp (grouped by temperature ranges)
#' # For this example, we categorize `Temp` values
#' airquality$Temp_category <- cut(airquality$Temp, breaks=c(50, 70, 80, 90, 100),
#'                                 labels=c("Cool", "Moderate", "Warm", "Hot"))
#' count_all_missing_by_group(airquality, "Temp_category")
#'
#' @export
count_all_missing_by_group <- function(data, group_col) {
  if (!group_col %in% names(data)) {
    stop(paste("Column", group_col, "not found in data"))
  }
  # Ensure data is a data frame
  data <- as.data.frame(data)

  # Get unique groups
  groups <- unique(data[[group_col]])

  # Initialize results data frame
  result <- data.frame(matrix(ncol = ncol(data), nrow = length(groups)))
  colnames(result) <- c(group_col, names(data)[names(data) != group_col])

  # Count missing values for each group
  for (i in seq_along(groups)) {
    group_data <- data[data[[group_col]] == groups[i], ]
    missing_counts <- sapply(group_data, function(x) as.numeric(sum(is.na(x))))
    result[i, ] <- c(groups[i], missing_counts[names(missing_counts) != group_col])
  }

  return(result)
}
