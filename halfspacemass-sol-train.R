train_depth <- function(data, n_halfspace, subsample = 1, scope = 1, seed = 1) {
  checkmate::assert(
    checkmate::test_matrix(data),
    checkmate::test_data_frame(data),
    combine = "or"
  )
  checkmate::assert_number(n_halfspace, finite = TRUE)
  checkmate::assert_number(subsample, lower = 0, upper = 1, finite = TRUE)
  checkmate::assert_number(scope, finite = TRUE)
  checkmate::assert_number(seed, finite = TRUE)

  data <- as.matrix(data)
  halfspaces <- list()
  subsample_size <- subsample * nrow(data)

  set.seed(seed)
  for (i in 1:n_halfspace) {
    # List elements contain training results for each halfspace
    halfspaces[[i]] <- train(data, subsample_size, scope)
  }
  class(halfspaces) <- "halfspaces"
  # Add subsample_size as attribute - used for evaluation
  attr(halfspaces, "subsample_size") <- subsample_size
  return(halfspaces)
}


train <- function(data, subsample_size, scope) {
  rand_direction <- get_rand_direction(data)
  data_sub <- get_subsample(data)
  data_projected <- project(data_sub, rand_direction)
  split_point <- select_split_point(data_projected, scope)
  mass_left <- sum(data_projected < split_point) / nrow(data_sub)
  mass_right <- sum(data_projected >= split_point) / nrow(data_sub)
  result <- list(
    "rand_direction" = rand_direction,
    "split_point" = split_point,
    "mass_left" = mass_left,
    "mass_right" = mass_right,
    "n_data" = nrow(data_sub)
  )
}

get_rand_direction <- function(data) {
  rand_vec <- rnorm(ncol(data))
  rand_vec
}

get_subsample <- function(data, subsample_size) {
  data[sample(nrow(data), subsample_size), ]
}

project <- function(point, vec) {
  (point %*% vec) / norm_vec(vec)
}

select_split_point <- function(data, scope) {
  max_val <- max(data)
  min_val <- min(data)
  mid_val <- (max_val + min_val) / 2
  selected_point <- runif(
    1, mid_val - scope / 2 * (max_val - min_val),
    mid_val + scope / 2 * (max_val - min_val)
  )
  selected_point
}

# Euclidean norm
norm_vec <- function(x) {
  sqrt(sum(x^2))
}
