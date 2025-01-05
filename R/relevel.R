#' Reassign Levels and Elements of a Factor Variable with Numeric Values
#'
#' This function reassigns the levels and corresponding elements of a factor variable to a sequential numeric range.
#' It is specifically designed to handle factor variables or vectors containing numeric values or values that can be
#' coerced to numeric. The new levels are assigned as a continuous sequence from 1 to the number of unique observed
#' values, and the factor elements are updated accordingly.
#'
#' Note that this function cannot handle character vectors that contain non-numeric values. The input must either be
#' numeric, a factor with numeric levels, or a character vector that can be coerced to numeric values.
#'
#' This transformation is useful for normalizing factor levels in cases where some levels are completely unobserved in
#' the data or where factor levels need to be remapped to a continuous numeric sequence.
#'
#' @param factor_var A factor variable, or a numeric vector, or a character vector that can be coerced to numeric values.
#' The input must contain numeric elements or be coercible to numeric values.
#'
#' @return A factor variable with updated levels and elements.
#' The levels are reassigned to a continuous numeric range starting from 1, corresponding to the unique observed values in the input.
#' @export
#' @examples
#' # Example: Factor with missing levels
#' set.seed(1)
#' gm_data_missing <- dgp(num_event = 10, num_site = 20, tau = 1, phi_S2S = 1,
#'                        phi_SS = 1, missing_percent = 0.9)
#'
#' # The 'site_id' variable has 20 levels but only 8 are observed
#' print(gm_data_missing$site_id)
#' # [1] 13 2 8 13 19 13 2 8 13 8 13 16 13 2 8 9 11 13 16 17
#' # Levels: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
#'
#' # Reassign the levels and elements of the 'site_id' variable
#' remap_elements_and_levels(gm_data_missing$site_id)
#' # [1] 5 1 2 5 8 5 1 2 5 2 5 6 5 1 2 3 4 5 6 7
#' # Levels: 1 2 3 4 5 6 7 8
remap_elements_and_levels <- function(factor_var) {

  # Check if input is NULL or empty
  if (is.null(factor_var) || length(factor_var) == 0) {
    stop("Input cannot be NULL or empty.")
  }

  # Convert to character for extracting the values of the elements instead of the levels of the factor variable
  factor_var <- as.character(factor_var)

  # Ensure the input can be coerced to a numeric value
  if (all(is.na(as.numeric(factor_var)))) {
    stop("The input must contain numeric elements or be coercible to numeric values.")
  }

  # Create a mapping for levels and elements
  current_elements <- sort(unique(as.numeric(factor_var)))
  new_range <- 1:length(current_elements)
  level_map <- setNames(new_range, current_elements)

  # Extract current elements as characters
  current_elements_as_char <- as.character(factor_var)

  # Remap the elements using the mapping
  new_elements <- level_map[current_elements_as_char]

  # Create a new factor with the new elements and new levels
  new_factor_var <- factor(new_elements, levels = new_range)

  # Remove the names of the new factor variable
  names(new_factor_var) <- NULL

  return(new_factor_var)
}
