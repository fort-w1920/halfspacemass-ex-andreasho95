train_depth <- function(data, n_halfspace, subsample = 1, scope = 1, seed = 1){
  
  # Checks
  checkmate::assert(checkmate::test_matrix(data), checkmate::test_data_frame(data), combine = "or")
  
  set.seed(seed)
  res_list <- list()
  subsample_size = subsample * nrow(data)
  for (i in 1:n_halfspace) {
    rand_direction <- get_rand_direction(data)
    data_sub <- get_subsample(data)
    data_projected <- project(data_sub, rand_direction)
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
  (point %*% vec) / norm_vec(vec)
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


