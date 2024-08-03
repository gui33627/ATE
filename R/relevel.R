
#' Remap Elements and Levels of a Factor Variable
#'
#' This function remaps the elements and levels of a factor variable.
#' Given a factor variable, it reassigns the levels to a sequential range
#' starting from 1 to the number of unique levels.
#' The factor elements themselves are also reassigned accordingly.
#'
#' @param factor_var A factor variable whose elements and levels are to be remapped.
#'
#' @return A factor variable with remapped elements and levels.
#' The new levels will be a sequential range starting from 1.
remap_elements_and_levels <- function(factor_var) {
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
  # remove the names of the new factor variable
  names(new_factor_var) <- NULL

  return(new_factor_var)
}
