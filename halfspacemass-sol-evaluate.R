# Evaluate all halfspaces for a given data set
evaluate_depth <- function(data, halfspaces){
  checkmate::assert_class(halfspaces, "halfspaces")
  checkmate::assert(checkmate::test_matrix(data), checkmate::test_data_frame(data), combine = "or")
  halfspacemasses <- apply(data, 1, evaluate, halfspaces)
  halfspacemasses / length(halfspacemasses)
}

# Evaluate all halfspaces for a given data row
evaluate <- function(data, halfspaces){
  eval_halfspaces <- sapply(halfspaces, evaluate_single, data)
  sum(eval_halfspaces)
}

# Evaluate one halfspace for a given data row
evaluate_single <- function(halfspaces, data){
  data_projected <- project(data, halfspaces$rand_direction)
  split_point <- halfspaces$split_point
  if (data_projected < split_point) {
    this_halfmass <- halfspaces$mass_left
  } else {
    this_halfmass <- halfspaces$mass_right
  }
  this_halfmass
}