# - train_depth(data, n_halfspace, subsample, scope, seed):
# - data enthält die Trainingsdaten (zi,i=1,…,n, in der obigen Notation)
# - n_halfspace ist die Anzahl gezogener Halbräume (Notation im Paper: t)
# - subsample ist der Anteil der Daten der zur Berechnung für jeden Halbraum zufällig gezogen werden soll (im Paper: =ψ|D|, default sollte 1 sein)
# - scope ist im Paper λ (default sollte 1 sein)
# - seed für den RNG.

# For each i in t (n_halfspace)
#   - Create random direction l_i in R^n, the data space of D (n = ncol(data))
#   - Generate subsample D_i (wihtout replacement, size subsample*nrow(data))
#   - Project D_i onto l_i (each data point separate?)
#   - get max and min of projected D_i -> Calculate mid
#   - Randomly select s_i
#   - Get mass_left and mass_right
#   - Return list with l_i, s_i, mass_left and mass_right for each t (n_halfspace)

set.seed(123456789)
data <- matrix(runif(300, min = -20, max = 20), ncol = 3)

########
subsample = 1
subsample_size = subsample * nrow(data)
rand_dir <- get_rand_direction(data)
data_sub <- get_subsample(data)
data_projected <- project_all(data_sub, rand_dir)
#data_norms <- apply(data_projected, 1, norm_vec)
split_point <- select_split_point(data_projected, 1)
ml_i <- sum(data_projected < split_point)/nrow(data_sub)
mr_i <- sum(data_projected >= split_point)/nrow(data_sub)
ml_i
mr_i

halfspaces <- train_depth(data, 500)
halfspaces
#######

train_depth <- function(data, n_halfspace, subsample = 1, scope = 1, seed = 1){
  
  # Checks
  # TODO
  set.seed(seed)
  res_list <- list()
  subsample_size = subsample * nrow(data)
  for (i in 1:n_halfspace) {
    rand_direction <- get_rand_direction(data)
    data_sub <- get_subsample(data)
    data_projected <- project_all(data_sub, rand_direction)
    split_point <- select_split_point(data_projected, scope)
    mass_left <- sum(data_projected < split_point) / nrow(data_sub)
    mass_right <- sum(data_projected >= split_point) / nrow(data_sub)
    res_list[[i]] <- list(
      "rand_direction" = rand_direction, 
      "split_point" = split_point,
      "mass_left" = mass_left, 
      "mass_right" = mass_right)
  }
  class(res_list) <- "halfspaces"
  return(res_list)
}


get_rand_direction <- function(data){
  rand_vec <- rnorm(ncol(data))
  rand_vec
}

get_subsample <- function(data, subsample_size){
  data[sample(nrow(data), subsample_size), ]
}

project_all <- function(data, vec){
  data_projected <- apply(data, 1, project, vec)
  data_projected
}

project <- function(point, vec){
  (vec %*% point) / norm_vec(vec)
}



select_split_point <- function(data, scope){
  max_val <- max(data)
  min_val <- min(data)
  mid_val <- (max_val + min_val) / 2
  selected_point <- runif(1, mid_val - scope/2 * (max_val - min_val), 
                          mid_val + scope/2 * (max_val - min_val))
  selected_point
}

# Euclidean norm
norm_vec <- function(x) {
  sqrt(sum(x^2))
} 


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

set.seed(123456789)
data <- matrix(runif(300, min = -20, max = 20), ncol = 3)
halfspaces <- train_depth(data, 500)
depth_matrix <- evaluate_depth(data, halfspaces)
data <- data.frame(data)
halfspaces <- train_depth(data, 500)
depth_df <- evaluate_depth(data, halfspaces)
identical(depth_df, depth_matrix)

