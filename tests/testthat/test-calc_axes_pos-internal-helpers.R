test_that("wrap_0_2pi() correctly sets positions within 0 and 2pi", {

  initial_pos <- c(2*pi, 3*pi, -(pi), 7.5*pi, -7.5*pi)

  out <- wrap_0_2pi(initial_pos)

  expected_pos <- c(0, pi, pi, 1.5*pi, 0.5*pi)

  expect_equal(out, expected_pos)

})
