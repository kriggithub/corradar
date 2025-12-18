test_that("calc_axes_pos() returns named numeric vector of correct length", {
  out <- calc_axes_pos(iris, max_iteration = 50)

  expect_type(out, "double")
  expect_named(out)

  num_vars <- names(iris)[vapply(iris, is.numeric, logical(1))]
  expect_equal(length(out), length(num_vars))
  expect_equal(sort(names(out)), sort(num_vars))
})



test_that("calc_axes_pos() returns positions are wrapped to [0, 2pi)", {
  out <- calc_axes_pos(iris, max_iteration = 50)

  expect_true(all(is.finite(out)))
  expect_true(all(out >= 0))
  expect_true(all(out < 2*pi))
})



test_that("calc_axes_pos() vars restricts output and preserves order", {
  vars <- c("Petal.Width", "Sepal.Length")
  out <- calc_axes_pos(iris, vars = vars, max_iteration = 50)

  expect_equal(names(out), vars)
  expect_equal(length(out), length(vars))
})



test_that("calc_axes_pos() drops non-numeric vars after selection", {
  out <- calc_axes_pos(iris, vars = c("Sepal.Length", "Species"), max_iteration = 50)

  expect_equal(names(out), "Sepal.Length")
  expect_equal(length(out), 1)
})
