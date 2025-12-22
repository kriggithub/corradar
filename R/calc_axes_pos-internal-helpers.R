#' Wrap angles to the interval [0, 2π)
#'
#' @param theta A numeric vector of angles in radians. Values may be negative or exceed 2π
#'
#' @returns A numeric vector of the same length as `theta`, with all values wrapped into the interval [0, 2π).
#' @noRd
wrap_0_2pi <- function(theta) {
  # Set 2pi to 0
  theta[theta == 2*pi] <- 0

  # Fix negative angles by adding 2pi until non-negative
  while (any(theta < 0)) {
    theta[theta < 0] <- theta[theta < 0] + (2*pi)
  }

  # Fix angles over 2pi by subtracting 2pi until within 0 to 2pi
  while (any(theta > 2*pi)) {
    theta[theta > 2*pi] <- theta[theta > 2*pi] - (2*pi)
  }

  # Return adjusted angles
  theta
}






#' Rotate and sort a numeric vector back to origin.
#'
#' @param theta A numeric vector of angles in radians.
#'
#' @returns A numeric vector of the same length as `theta`, rotated so that first element is at the origin, and sorted in order of increasing distance.
#' @noRd
rotate_axis <- function(theta) {

  # Capture position of first angle
  first_pos <- theta[1]

  # Set first angle to be at the origin
  theta[1] <- 0

  # Rotate the other elements by the same amount that was subtracted from first element
  theta[-1] <- theta[-1] - first_pos

  # Wrap angles back into  interval [0, 2π) (helper function)
  theta <- wrap_0_2pi(theta)

  # Sort axes
  theta <- sort(theta)

  # Return theta
  theta
}




#' Enforce minimum radial distances on a circle.
#'
#' @param theta A numeric vector of angles in radians.
#' @param min_dist A numeric value for the minimum distance between each element.
#'
#' @returns  A numeric vector of the same length as `theta`, with all values exceeding `min_dist`
#' @noRd
adj_min_dist <- function(theta,
                         min_dist) {

  # Rotate and order variables
  theta <- rotate_axis(theta)

  # Create helper function to find vector of gaps between variables, including the last wrap around
  n_vars <- length(theta)
  gaps <- function (theta) {
    c(diff(theta), (2 * pi) - theta[n_vars] + theta[1])
  }

  # Compute gap vector
  gap_vec <- gaps(theta)

  # Create helper function to find the right axes of a gap
  # If last axis is left side of gap, then right side is the first axis
  next_axis <- function (i) {
    (i %% n_vars) + 1L
  }

  # Begin loop
  while (min(gap_vec) < min_dist) {

    # Identify the left (i) axis of the smallest gap (stays still)
    sm_gap_i <- which.min(gap_vec)

    # Identify size of the smallest gap
    sm_gap <- gap_vec[sm_gap_i]

    # Determine amount needed to move axes forward
    amt_move <- min_dist - sm_gap

    # Identify the right (j) axis of the largest gap (stays still)
    # If largest gap is the part that wraps, then first element is right side
    lg_gap_i <- which.max(gap_vec)
    lg_gap_j <- next_axis(lg_gap_i)

    # Identify the axis in between i & j that need to move forward
    # Includes i+1 up to (but not including) j
    # Loop through variables
    vars_to_move <- integer(0)
    cur_var <- next_axis(sm_gap_i)
    while (cur_var != lg_gap_j) {
      vars_to_move <- c(vars_to_move, cur_var)
      cur_var <- next_axis(cur_var)
    }

    # Move identified axes forward
    theta[vars_to_move] <- theta[vars_to_move] + amt_move

    # Rotate and reorder axes back
    theta <- rotate_axis(theta)

    # Recompute gap vector
    gap_vec <- gaps(theta)
  }

  # Return theta
  theta

}













