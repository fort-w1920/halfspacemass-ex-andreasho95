# Evaluate all halfspaces for a given data set
evaluate_depth <- function(data, halfspaces, metric = "mass") {
  checkmate::assert_class(halfspaces, "halfspaces")
  checkmate::assert(checkmate::test_matrix(data), checkmate::test_data_frame(data), combine = "or")
  checkmate::assert_character(metric)
  data <- as.matrix(data)
  metric <- match.arg(tolower(metric), c("mass", "depth"))

  # Halfspacemasses per halfspace per data row
  halfspacemasses <- sapply(halfspaces, evaluate_single, data)
  if (metric == "mass") {
    # Average halfspacemass per data row
    halfspacemass <- rowMeans(halfspacemasses)
    return(halfspacemass)
  }
  # Minimum halfspacemass per data row
  min_halfspacemass <- apply(halfspacemasses, 1, FUN = min)
  # Get absolute number of data points
  halfspacedepths <- min_halfspacemass * attr(halfspaces, "subsample_size")
  halfspacedepths
}

# Evaluate single halfspace
evaluate_single <- function(halfspaces, data) {
  data_projected <- project(data, halfspaces$rand_direction)
  split_point <- halfspaces$split_point
  this_halfmass <- ifelse(
    data_projected < split_point,
    halfspaces$mass_left,
    halfspaces$mass_right
  )
  this_halfmass
}
