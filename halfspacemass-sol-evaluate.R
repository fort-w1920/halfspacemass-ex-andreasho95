# Evaluate all halfspaces for a given data set
evaluate_depth <- function(data, halfspaces, metric = "mass"){
  checkmate::assert_class(halfspaces, "halfspaces")
  checkmate::assert(checkmate::test_matrix(data), checkmate::test_data_frame(data), combine = "or")
  checkmate::assert_character(metric)
  metric <- match.arg(tolower(metric), c("mass", "depth"))
  if (metric == "mass") {
    halfspacemasses <- apply(data, 1, evaluate_mass, halfspaces)
    res <- halfspacemasses / length(halfspacemasses)
  } else {
    halfspacedepths <- apply(data, 1, evaluate_density, halfspaces)
    res <- halfspacedepths * attr(halfspaces, "subsample_size")
  }
  res
}

# Evaluate all halfspaces for a given data row for halfspace MASS
evaluate_mass <- function(data, halfspaces){
  halfspacemasses <- sapply(halfspaces, evaluate_single_mass, data)
  sum(halfspacemasses)
}

# Evaluate one halfspace for a given data row for halfspace MASS
evaluate_single_mass <- function(halfspaces, data){
  data_projected <- project(data, halfspaces$rand_direction)
  split_point <- halfspaces$split_point
  if (data_projected < split_point) {
    this_halfmass <- halfspaces$mass_left
  } else {
    this_halfmass <- halfspaces$mass_right
  }
  this_halfmass
}


# Evaluate all halfspaces for a given data row for halfspace DENSITY
evaluate_density <- function(data, halfspaces){
  densities <- sapply(halfspaces, evaluate_single_density, data)
  min(densities)
}


# Evaluate one halfspace for a given data row for halfspace DENSITY
evaluate_single_density <- function(halfspaces, data){
  data_projected <- project(data, halfspaces$rand_direction)
  #return(min(halfspaces$mass_left, halfspaces$mass_right))
  split_point <- halfspaces$split_point
  if (data_projected < split_point) {
    this_density <- halfspaces$mass_left
  } else {
    this_density <- halfspaces$mass_right
  }
  this_density
}
