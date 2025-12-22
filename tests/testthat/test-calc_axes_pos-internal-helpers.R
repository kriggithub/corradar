test_that("wrap_0_2pi() correctly sets positions within 0 and 2pi", {

  initial_pos <- c(2*pi, 3*pi, -(pi), 7.5*pi, -7.5*pi)

  expected_pos <- c(0, pi, pi, 1.5*pi, 0.5*pi)

  out <- wrap_0_2pi(initial_pos)

  expect_equal(out, expected_pos)

})




test_that("rotate_axis() correctly rotates positions", {

  initial_pos <- c(0.5, pi, 2*pi)

  expected_pos <- c(0, pi-0.5, 2*pi-0.5)

  out <- rotate_axis(initial_pos)

  expect_equal(out, expected_pos)

})



test_that("rotate_axis() correctly sorts axes", {

  initial_pos <- c(0, 0.75, 0.1, 0.88, 0.9, 0.3)

  expected_pos <- c(0, 0.1, 0.3, 0.75, 0.88, 0.9)

  out <- rotate_axis(initial_pos)

  expect_equal(out, expected_pos)

})


test_that("adj_min_dist() correctly adjusts distances", {

  initial_pos_1 <- c(0, 0.1, 0.2, 5)

  expected_pos_1 <- c(0, 0.2, 0.4, 5)

  out_1 <- adj_min_dist(initial_pos_1, min_dist = 0.2)

  expect_equal(out_1, expected_pos_1)

})
