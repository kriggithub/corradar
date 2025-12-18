#' Wrap angles to the interval [0, 2π)
#'
#' @param x A numeric vector of angles in radians. Values may be negative or exceed 2π
#'
#' @returns A numeric vector of the same length as `x`, with all values wrapped into the interval [0, 2π).
#' @noRd
wrap_0_2pi <- function(x) {
  # Set 2pi to 0
  x[x == 2*pi] <- 0

  # Fix negative angles by adding 2pi until non-negative
  while (any(x < 0)) {
    x[x < 0] <- x[x < 0] + (2*pi)
  }

  # Fix angles over 2pi by subtracting 2pi until within 0 to 2pi
  while (any(x > 2*pi)) {
    x[x > 2*pi] <- x[x > 2*pi] - (2*pi)
  }

  x
}
