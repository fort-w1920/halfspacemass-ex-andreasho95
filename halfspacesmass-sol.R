train_depth <- function(data, n_halfspace, subsample = 1, scope = 1, seed = 1){
  
  # Checks
  # Can data be converted to data.frame?
  set.seed(seed)
  res_list <- list()
  subsample_size = subsample * nrow(data)
  for (i in 1:n_halfspace) {
    rand_direction <- get_rand_direction(data)
    data_sub <- get_subsample(data)
    data_projected <- project_on_vec(data_sub, rand_direction)
    data_norms <- apply(data_projected, 1, norm_vec)
    split_point <- select_split_point(data_norms, scope)
    mass_left <- sum(data_norms < split_point)/nrow(data_sub)
    mass_right <- sum(data_norms >= split_point)/nrow(data_sub)
    res_list[[i]] <- list(
      "rand_direction" = rand_direction, 
      "split_point" = split_point,
      "mass_left" = mass_left, 
      "mass_right" = mass_right)
  }
  
  return(res_list)
}


get_rand_direction <- function(data){
  rand_vec <- runif(ncol(data))
  rand_vec
}

get_subsample <- function(data, subsample_size){
  data[sample(nrow(data), subsample_size), ]
}

project_on_vec <- function(data, vec){
  data_projected <- as.data.frame(t(apply(data, 1, project, vec)))
  data_projected
}

project <- function(point, vec){
  vec + sum((point - vec) * vec) / sum(vec * vec) * vec
}


select_split_point <- function(data_norms, scope){
  max_val <- max(data_norms)
  min_val <- min(data_norms)
  mid_val <- (max_val + min_val) / 2
  selected_point <- runif(1, mid_val - scope/2 * (max_val - min_val), mid_val + scope/2 * (max_val - min_val))
  selected_point
}


norm_vec <- function(x) sqrt(sum(x^2))








