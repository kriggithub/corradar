#' Calculate axes positions for corradar plots.
#'
#' @param data Data file.
#' @param vars Variables to calculate axis positions for.
#' @param max_iteration Maxmimum amount of iterations for algorithm to run.
#' @param min_degrees Minimum degrees between each variable for visual spacing.
#' @param return_diagnostics Logical. If `FALSE` (default), return only the
#'   optimal axis positions. If `TRUE`, also return diagnostic information
#'   including the iteration at which the optimum was found and the residual
#'   force value.
#'
#' @returns
#' If `return_diagnostics = FALSE` (default), a named numeric vector of axis
#' positions in radians. If `TRUE`, a list with elements:
#' \describe{
#'   \item{pos}{Named numeric vector of axis positions.}
#'   \item{best_iteration}{Iteration at which the best solution was found.}
#'   \item{best_resid_force}{Residual force at the best iteration.}
#' }
#' @export
#'
#' @examples
#' data <- iris
#'
#' iris_corradar_pos <- calc_axes_pos(data)
#'
#' iris_corradar_pos
calc_axes_pos <- function(data,
                          vars = NULL,
                          max_iteration = 1000,
                          min_degrees = 3,
                          return_diagnostics = FALSE){

  # Step 0: Pre-function data cleaning and setup

  # (a) Select numeric variables from data if not specified
  if (is.null(vars)) {
    num_cols <- vapply(data, is.numeric, logical(1))
    vars <- names(data)[num_cols]
  }

  # (b) Ensure data contains only numeric variables
  data <- data[, vars, drop = FALSE]
  num_cols <- vapply(data, is.numeric, logical(1))
  data <- data[, num_cols, drop = FALSE]

  # (c) Calculate correlation matrix for selected variables
  corr_mat <- stats::cor(data, use='complete.obs')

  # (d) Convert correlation matrix into matrix of "ideal" distances between variables
  ideal_mat <- (1-abs(corr_mat))*pi

  # (e) Initialize data frame to map positions (relative to origin) for all variables
  n_var <- nrow(corr_mat)
  names_var <- rownames(corr_mat)
  cur_pos_df <- data.frame(
    matrix(ncol = n_var, nrow = 1),
    row.names = NULL
  )
  colnames(cur_pos_df) <- names_var

  # (f) Set positions initially equidistant
  equal_pos <- (2*pi) / n_var
  cur_pos_df[1, ] <- cumsum(rep(equal_pos, n_var))

  # (g) Set 2*pi back as origin
  cur_pos_df[cur_pos_df == 2*pi] <- 0

  # (h) Set up matrix to store pair-wise differences in distance between axes
  dist_diff_mat <- matrix(
    0,
    ncol = n_var,
    nrow = n_var
  )
  colnames(dist_diff_mat) <- names_var
  rownames(dist_diff_mat) <- names_var

  # (i) Compute initial shortest distance between variables (maximum distance is pi)
  for(i in 1:n_var) {
    for (j in 1:n_var) {
      distance <- abs(cur_pos_df[1, i] - cur_pos_df[1, j])
      if (distance > pi) {
        dist_diff_mat[i, j] <- (2*pi) - distance
      } else {
        dist_diff_mat[i, j] <- distance
      }
    }
  }

  # (j) Set up additional loop intializers (iteration count, minimum distance in radians, annealing factor)
  iteration_count <- 1
  min_dist <- ((2*min_degrees)/360)*pi
  anneal_factor <- max_iteration
  # Also initializers to store best iteration and residual forces
  best_iteration <- NA_integer_
  best_resid_force <- Inf
  best_pos <- cur_pos_df





  # Step 1: Loop through algorithm to find best iteration
  while (iteration_count <= max_iteration) {
    # (a) Calculate attractive and repulsive forces for each axis in a pair of variables and store in matrix
    force_mat <- (dist_diff_mat - ideal_mat)/2

    # (b) Update best iteration, best residual force (lowest), and position of best iteration
    resid_force <- sum(abs(force_mat)) / 2
    if (resid_force < best_resid_force) {
      best_resid_force <- resid_force
      best_iteration <- iteration_count
      best_pos <- cur_pos_df
    }

    # (c) Use simulated annealing on force_mat to apply to iterations
    annealed_force_mat <- force_mat * (anneal_factor / max_iteration)

    # (d) Apply annealed attractive or repulsive forces between all axis pairs
    for (i in 1:n_var) {
      for (j in 1:n_var){

        # Algorithm:
        # First, determine if force between pairs is attractive (positive) or repulsive (negative)
        # Next, find out what the current distance between two variables, noting whether the short arc or long arc was calculated
        # Apply force to each axis

        # Pull out force between a pair
        force <- annealed_force_mat[i,j]

        # Skip pairs with no force
        if (force == 0) next

        # Process pairs only once, preventing applying same force twice between pairs, assuming i variable is further along radial axis than j
        if (!(cur_pos_df[1, i] > cur_pos_df[1, j])) next

        # Pull out raw radial distances between pairs and classify the distance as either the short arc or long arc to find true shortest distance between pairs
        short_arc <- (cur_pos_df[1, i] - cur_pos_df[1, j]) <= pi

        # Pull raw magnitude of the force to either add or subtract to axes
        mag_force <- abs(force)


        # For attractive force (pull i and j closer togeher):
        if (force > 0) {

          # For short arc (distance was already the shortest):
          if (short_arc) {
            # i moves backward
            cur_pos_df[1, i] <- cur_pos_df[1, i] - mag_force
            # j moves forward
            cur_pos_df[1, j] <- cur_pos_df[1, j] + mag_force

            # If long arc (distance was not the shortest):
          } else {
            # i moves forward
            cur_pos_df[1, i] <- cur_pos_df[1, i] + mag_force
            # j moves backward
            cur_pos_df[1, j] <- cur_pos_df[1, j] - mag_force
          }

        # For repulsive forces (push i and j farther apart)
        } else {

          # For short arc (distance was already the shortest):
          if (short_arc) {
            # i moves forward
            cur_pos_df[1, i] <- cur_pos_df[1, i] + mag_force
            # j moves backward
            cur_pos_df[1, j] <- cur_pos_df[1, j] - mag_force

            # If long arc (distance was not the shortest):
          } else {
            # i moves backward
            cur_pos_df[1, i] <- cur_pos_df[1, i] - mag_force
            # j moves forward
            cur_pos_df[1, j] <- cur_pos_df[1, j] + mag_force
          }
        }
      }
    }

    # Ensure positions are within 0 and 2pi (helper function)
    cur_pos_df <- wrap_0_2pi(cur_pos_df)

    # Recalculate difference in distance matrix
    for(i in 1:n_var) {
      for (j in 1:n_var) {
        distance <- abs(cur_pos_df[1, i] - cur_pos_df[1, j])
        if (distance > pi) {
          dist_diff_mat[i, j] <- (2*pi) - distance
        } else {
          dist_diff_mat[i, j] <- distance
        }
      }
    }

    # Decrease annealing factor, increase iteration
    anneal_factor <- anneal_factor - 1
    iteration_count <- iteration_count + 1
  }

  # Finalize best positions
  out <- as.numeric(best_pos[1, ])
  names(out) <- colnames(best_pos)

  # Return best positions (default)
  if (!return_diagnostics) {
    return(out)
  }

  # Return iteration and force info if diagnostic is set to TRUE
  list(
    pos = out,
    best_iteration = best_iteration,
    best_resid_force = best_resid_force
  )
}




