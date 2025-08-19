skip_if_not_installed("DBI")
skip_if_not_installed("duckdb")

test_that("get_duck_connection() errors when neither con nor path is provided", {
  expect_error(get_duck_connection(con = NULL, raw_db_path = NULL),
               "Either con or raw_db_path must be provided")
})

test_that("get_duck_connection() errors when read_only=TRUE and file missing", {
  td <- withr::local_tempdir()
  missing_db <- file.path(td, "does_not_exist.duckdb")
  expect_error(
    get_duck_connection(raw_db_path = missing_db, read_only = TRUE),
    "Database file not found"
  )
})

test_that("get_duck_connection() can create an in-memory connection when read_only=FALSE", {
  gc <- get_duck_connection(raw_db_path = ":memory:", read_only = FALSE)
  on.exit(try(DBI::dbDisconnect(gc$con, shutdown = TRUE), silent = TRUE), add = TRUE)

  expect_true(is.list(gc))
  expect_true(DBI::dbIsValid(gc$con))
  expect_true(gc$should_close)

  # run a trivial query
  res <- DBI::dbGetQuery(gc$con, "SELECT 1 AS one")
  expect_identical(res$one, 1L)
})

test_that("get_duck_connection() creates a persistent DB file when read_only=FALSE", {
  td <- withr::local_tempdir()
  db_path <- file.path(td, "test.duckdb")

  gc <- get_duck_connection(raw_db_path = db_path, read_only = FALSE)
  on.exit(try(DBI::dbDisconnect(gc$con, shutdown = TRUE), silent = TRUE), add = TRUE)
  expect_true(DBI::dbIsValid(gc$con))

  # Touch the DB so the file is certainly created
  DBI::dbExecute(gc$con, "CREATE TABLE t(x INTEGER)")
  DBI::dbExecute(gc$con, "INSERT INTO t VALUES (1)")
  res <- DBI::dbGetQuery(gc$con, "SELECT COUNT(*) AS n FROM t")
  expect_identical(res$n, 1)

  # The file should exist on disk
  expect_true(file.exists(db_path))
})

test_that("get_duck_connection() reuses an existing connection and should_close=FALSE", {
  base_con <- DBI::dbConnect(duckdb::duckdb(), dbdir=":memory:", read_only = FALSE)
  on.exit(try(DBI::dbDisconnect(base_con, shutdown = TRUE), silent = TRUE), add = TRUE)

  gc <- get_duck_connection(con = base_con, raw_db_path = "ignored.duckdb", read_only = TRUE)
  expect_identical(gc$con, base_con)
  expect_false(gc$should_close)

  DBI::dbExecute(gc$con, "CREATE TABLE t2(x INTEGER)")
  res <- DBI::dbGetQuery(gc$con, "SELECT COUNT(*) AS n FROM t2")
  # Empty table exists and returns 0 rows
  expect_identical(res$n, 0)
})

test_that("get_duck_connection() connects read-only to an existing DB", {
  td <- withr::local_tempdir()
  db_path <- file.path(td, "readonly.duckdb")

  # First, create it
  gc_w <- get_duck_connection(raw_db_path = db_path, read_only = FALSE)
  DBI::dbExecute(gc_w$con, "CREATE TABLE t3(x INTEGER)")
  DBI::dbDisconnect(gc_w$con, shutdown = TRUE)

  # Now open read-only
  gc_r <- get_duck_connection(raw_db_path = db_path, read_only = TRUE)
  on.exit(try(DBI::dbDisconnect(gc_r$con, shutdown = TRUE), silent = TRUE), add = TRUE)
  expect_true(DBI::dbIsValid(gc_r$con))
  # Read works
  res <- DBI::dbGetQuery(gc_r$con, "SELECT COUNT(*) AS n FROM t3")
  expect_identical(res$n, 0)
})
