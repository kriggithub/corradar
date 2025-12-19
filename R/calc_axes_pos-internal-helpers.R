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






#' Enforce minimum radial distances on a circle.
#'
#' @param theta A numeric vector of angles in radians.
#' @param min_dist A numeric value for the minimum distance between each element.
#'
#' @returns  A numeric vector of the same length as `theta`, with all values exceeding `min_dist`
#' @noRD
adj_min_dist <- function(theta,
                         min_dist) {
  # Pull out number of variables
  n_vars <- length(theta)

  # Ensure that it is possible to space out a solution
  if (n_vars * min_dist > (2*pi) + 1e-12) {
    stop("min_dist is too large for the number of variables.")
  }

  # Store order of variables
  vars_order <- order(theta)

  # Sort the variables so that they are in increasing distance from origin
  vars_sorted <- theta[vars_order]

  # Build vector of gaps from sorted order (including gap between first and last because it's a circle)
  gap_vec <- c(diff(vars_sorted), (2*pi) - vars_sorted[n] + vars_sorted[1])

  # Begin loop increase the smallest gap to min_dist, and taking the difference from the largest gap
  while (min(gap_vec) < min_dist){

    # Identify smallest gap
    min_gap <- which.min(gap_vec)

    # Identify largest gap
    max_gap <- which.max(gap_vec)

    # Identify deficit needed to move
    deficit <- min_dist




  }


}




#' Rotate a numeric vector back to origin.
#'
#' @param theta A numeric vector of angles in radians.
#'
#' @returns A numeric vector of the same length as `theta`, rotated so that first element is at the origin.
#' @noRD
rotate_axis <- function(theta) {

  # Capture position of first angle
  first_pos <- theta[1]

  # Set first angle to be at the origin
  theta[1] <- 0

  # Rotate the other elements by the same amount that was subtracted from first element
  theta[-1] <- theta[-1] - first_pos

  # Wrap angles back into  interval [0, 2π) (helper function)
  theta <- wrap_0_2pi(theta)

  # Return theta
  theta
}


















