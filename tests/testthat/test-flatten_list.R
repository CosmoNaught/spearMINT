test_that("flatten_list() flattens nested named lists with underscore separator", {
  x <- list(
    a = 1,
    sub = list(
      b = 2,
      deeper = list(c = 3)
    ),
    vec = c(10, 20)
  )
  out <- flatten_list(x)

  # Keys present
  expect_true(all(c("a", "sub_b", "sub_deeper_c", "vec") %in% names(out)))

  # Values preserved
  expect_identical(out$a, 1)
  expect_identical(out$sub_b, 2)
  expect_identical(out$sub_deeper_c, 3)
  expect_identical(out$vec, c(10, 20))
})

test_that("flatten_list() respects parent_key prefix", {
  x <- list(alpha = list(beta = 5))
  out <- flatten_list(x, parent_key = "root")
  expect_true("root_alpha_beta" %in% names(out))
  expect_identical(out$root_alpha_beta, 5)
})

test_that("flatten_list() leaves unnamed sublists as a single value", {
  x <- list(
    has_names = list(k = 1),
    no_names  = list(1, 2, 3)  # unnamed list
  )
  out <- flatten_list(x)
  expect_true("has_names_k" %in% names(out))
  expect_true("no_names" %in% names(out))
  expect_identical(out$no_names, list(1, 2, 3))  # unchanged as a list
})

test_that("flatten_list() handles empty list", {
  expect_identical(flatten_list(list()), list())
})
