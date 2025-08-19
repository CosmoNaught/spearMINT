# spearMINT

An R package of **shared utilities** used across the MINTverse (e.g., **segMINT**, **MINTed**).  
spearMINT centralises low-level helpers—data loading, list flattening, DuckDB connection management,
SQL clause building, lightweight dependency checks, and batch job wrappers—so downstream packages can stay lean.

> This is primarily an **internal support library**. Most users will interact with segMINT/MINTed directly.
> The examples below are for completeness and for developers extending the MINTverse.

---

## Features

- **ITN data helpers** — load, label, combine, and save ITN parameter CSVs
- **Parameter naming** — generate compact names (e.g., `AA1`) from integer indices
- **List flattening** — deterministic underscore-delimited key paths (`a_b_c`)
- **DuckDB connections** — consistent creation/reuse with a `should_close` flag
- **SQL key filters** — safe WHERE-clause snippets for `parameter_index` / `global_index`
- **Dependency audit** — scan `src/` for `library()/require()` vs `pkgdepends.txt`
- **Hipercow integration** — interactive helper to append missing deps & reprovision
- **Orderly batch checks** — run `orderly2::orderly_metadata_extract()` over indices, with optional parallelism

---

## Installation

```r
# From a local checkout
devtools::install_local("/path/to/spearMINT", force = TRUE)

# Or from GitHub (adjust if your remote differs)
devtools::install_github("CosmoNaught/spearMINT")
```

### Dependencies

- **DBI**, **duckdb** — connection management
- **dplyr**, **stringr** — small data/regex helpers used internally
- **utils** — base I/O
- **hipercow** (optional) — provisioning helper
- **orderly2** (optional) — metadata extraction for batch checks
- Base R **parallel** — optional parallel execution

---

## Quick Start (developer-oriented)

```r
library(spearMINT)
library(DBI)
library(duckdb)
library(dplyr)

# =============================================================================
# 1) ITN CSVs → combined RDS
# =============================================================================
files <- list(
  pyrethroid = "/path/to/pyrethroid.csv",
  piperonyl  = "/path/to/pbo.csv"
)

itn <- combine_itn_data(files)
# Inspect a few rows
head(itn)

# Save for downstream use
save_itn_data(itn, "/path/to/itn_params.rds")

# =============================================================================
# 2) DuckDB connection (create or reuse)
# =============================================================================
dc <- get_duck_connection(raw_db_path = ":memory:", read_only = FALSE)
con <- dc$con
DBI::dbExecute(con, "CREATE TABLE demo(x INTEGER)")
DBI::dbDisconnect(con, shutdown = TRUE)

# =============================================================================
# 3) Build a safe WHERE clause for keys
# =============================================================================
con <- DBI::dbConnect(duckdb::duckdb(), dbdir=":memory:", read_only = FALSE)
build_key_clause(con, parameter_index = 42)
build_key_clause(con, global_index = "simulation_results_7.rds")
DBI::dbDisconnect(con, shutdown = TRUE)

# =============================================================================
# 4) Flatten nested lists for easy tabular binding
# =============================================================================
x <- list(alpha = list(beta = 5, gamma = list(delta = 9)), z = 1)
flatten_list(x)
# -> list(alpha_beta = 5, alpha_gamma_delta = 9, z = 1)

# =============================================================================
# 5) Dependency audit for a project with src/ and pkgdepends.txt
# =============================================================================
# Non-interactive audit:
missing <- find_missing_packages(src_dir = "/project/src",
                                 pkg_file = "/project/pkgdepends.txt")
missing

# Interactive helper (prompts to append & reprovision):
# hipercow_check_pkgdepends()

# =============================================================================
# 6) Orderly batch checks (sequential demo)
# =============================================================================
# Safest for examples; set parallel = TRUE for large runs
orderly_prod(indices = 1:50, verbose = TRUE, parallel = FALSE, store_output = FALSE)
```

---

## Core Functions

### `load_itn_data()`

Load and label a single ITN CSV.

```r
load_itn_data(file_path, net_type)
```

- Adds a `net_type` column from the provided label
- Renames `ERG_d_ITN0 -> dn0`, `ERG_r_ITN0 -> rn0`
- Returns selected columns: `dn0, rn0, gamman, bioassay_surv, net_type`

---

### `combine_itn_data()`

Concatenate multiple ITN CSVs, each tagged by its name in the list.

```r
combine_itn_data(file_paths)
# file_paths: named list, e.g. list(pyrethroid="pyre.csv", pbo="pbo.csv")
```

---

### `save_itn_data()`

Persist any ITN dataframe to an `.rds`.

```r
save_itn_data(data, output_path)
```

---

### `generate_param_name()`

Generate compact parameter names from an integer index, rolling through letters and thousands.

```r
generate_param_name(index)
# Examples:
# generate_param_name(1)     -> "A1"
# generate_param_name(1001)  -> "B1"
# generate_param_name(27001) -> "AA1"
```

---

### `flatten_list()`

Flatten a nested, **named** list into a single-level list using underscore-delimited keys.

```r
flatten_list(x, parent_key = "")
# list(a = list(b = list(c = 1))) -> list(a_b_c = 1)
```

Unnamed sublists are kept as-is.

---

### `get_duck_connection()`

Create or reuse a DuckDB connection with a standardised return shape.

```r
get_duck_connection(con = NULL, raw_db_path = NULL, read_only = TRUE)
# Returns: list(con = <DBI connection>, should_close = <logical>)
```

Rules:
- If `con` is provided, it’s reused (`should_close = FALSE`).
- If `con = NULL`, `raw_db_path` is required.
- When `read_only = TRUE`, the file must exist (unless dbdir = ":memory:" with read_only = FALSE).

---

### `build_key_clause()`

Build the SQL fragment to filter by **either** `parameter_index` **or** `global_index`.

```r
build_key_clause(con, parameter_index = 7)
# "parameter_index = 7"

build_key_clause(con, global_index = "simulation_results_7.rds")
# "global_index = 'simulation_results_7.rds'"
```

If both keys are supplied, `parameter_index` wins with a warning.

---

### Dependency utilities

```r
list_r_files(dir)                      # Recursively list *.R files
extract_packages(file)                 # Parse library()/require() calls
check_package_in_file(pkg, pkg_file)   # Check if pkg listed in pkgdepends.txt
find_missing_packages(src_dir, pkg_file)
```

> `hipercow_check_pkgdepends()` is interactive: it prints missing packages, offers to append them to `pkgdepends.txt`, and can call `hipercow::hipercow_provision()`.

---

### Orderly helpers

```r
check_parameter_set(i)            # Try orderly2::orderly_metadata_extract(latest(...))
execute_checks(indices, parallel) # Sequential or parallel over 'indices'
orderly_prod(indices = 1:10000,
             verbose = TRUE,
             parallel = TRUE,
             store_output = FALSE)
```

`orderly_prod()` prints success/error summaries and timing; optionally returns lists when `store_output = TRUE`.

---

## Advanced Notes & Tips

- **Piping**: The internal pipe is imported from **dplyr**; you don’t need **magrittr** explicitly.
- **Quoting**: `build_key_clause()` uses `DBI::dbQuoteString()` to avoid SQL injection and handle embedded quotes.
- **Parallelism**: When `parallel = TRUE`, one core is kept free (`detectCores() - 1`). Cluster lifecycle is managed automatically.
- **Non-interactive runs**: Prefer `find_missing_packages()` over `hipercow_check_pkgdepends()` in CI or scripts.

---

## Troubleshooting

**“Database file not found …”**  
Open in write mode (`read_only = FALSE`) or create the file first.

**“Provide either parameter_index OR global_index”**  
Call `build_key_clause()` with exactly one of the two.

**Dependency scan finds nothing**  
Ensure your project uses `library()`/`require()` calls (not only `::`), or extend the regex if needed.

**orderly errors per parameter set**  
`check_parameter_set()` wraps errors and records `error` strings. Inspect `orderly_prod(..., store_output = TRUE)` to capture IDs and failing indices.

---

## Author

**CosmoNaught**  
GitHub: https://github.com/CosmoNaught

## License

MIT License