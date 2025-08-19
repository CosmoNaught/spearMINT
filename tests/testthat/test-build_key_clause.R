skip_if_not_installed("DBI")
skip_if_not_installed("duckdb")

test_that("build_key_clause() errors when neither key is given", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir=":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

  expect_error(build_key_clause(con), "Provide either parameter_index OR global_index")
})

test_that("build_key_clause() prefers parameter_index when both are provided (warns)", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir=":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

  expect_warning(
    clause <- build_key_clause(con, parameter_index = 7, global_index = "file.rds"),
    "using parameter_index"
  )
  expect_identical(clause, "parameter_index = 7")
})

test_that("build_key_clause() formats parameter_index equality", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir=":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

  expect_identical(build_key_clause(con, parameter_index = 3L), "parameter_index = 3")
  expect_identical(build_key_clause(con, parameter_index = 3.9), "parameter_index = 3") # as.integer()
})

test_that("build_key_clause() quotes and escapes global_index correctly", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir=":memory:")
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)

  # simple case
  clause1 <- build_key_clause(con, global_index = "file.rds")
  expect_identical(clause1, "global_index = 'file.rds'")

  # with a single quote that should be doubled by SQL quoting
  clause2 <- build_key_clause(con, global_index = "file 'weird'.rds")
  expect_identical(clause2, "global_index = 'file ''weird''.rds'")
})
