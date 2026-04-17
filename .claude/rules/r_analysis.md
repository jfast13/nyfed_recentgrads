## Guidance for data analysis in R

### General conventions

- Use tidyverse-friendly code
- Make plots using ggplot
- Use base pipe `|>` instead of `%>%`

### Development workflow

To run R code, use `Rscript main.R` or `Rscript -e "some_expression"`. Run code you write or edit in order to verify that it works and fix any problems.

### Syntax and structure

#### File names

Use lower case for file names. Delimit words with `_` or `-`. Avoid spaces.

#### Packages

If the complete data analysis is a single script, load all packages with `library()` calls at the beginning of the file. For multi-file workflows, source a separate `packages.R` file that contains all `library()` calls.

Use the `conflicted` package to resolve conflicts.

Use `renv` only if explicitly requested or already initialized.

#### Object names

Use snake case (lower case with underscores `_`) for variable and function names. Prefer verbs for function names.

#### Function calls

Never partially match function arguments with a unique prefix; use the full argument name instead.

For legibility purposes you may omit names of very common arguments, like a `data` argument.

Never omit argument names in a `switch()` statement.

#### Control flow

Never use `&` and `|` inside of an if clause because they can unexpectedly return vectors; always use `&&` and `||` instead.

#### Returned values in functions

Only use `return()` for early returns. Otherwise, rely on R to return the result of the last evaluated expression.

#### Comments

Use comments to explain the "why", and not the "what" or "how".

#### Style preferences

In dplyr-based joins use `by = join_by()` syntax, such as `by = join_by(a == b)` instead of `by = c("a" = "b")`.

When possible avoid `group_by` and use the `.by` argument.

Use `map_*()` instead of `sapply`.

#### Column-wise operations

Use `across()` for column-wise operations within a single `mutate()` call rather than a `for` loop:

```r
# Avoid
for (col in income_cols) {
  result <- result |> mutate(!!paste0("total_", col) := .data[[col]] * n)
}

# Prefer
result_scaled <- result |>
  mutate(across(all_of(income_cols), \(x) x * n, .names = "total_{.col}"))
```

When operations are more complex and `across()` becomes unwieldy, prefer pivoting to long format, computing, then pivoting back:

```r
result_scaled <- result |>
  pivot_longer(all_of(income_cols), names_to = "col", values_to = "avg") |>
  mutate(total = avg * n) |>
  pivot_wider(names_from = col, values_from = c(avg, total))
```

#### Naming intermediate objects

Give each distinct stage of an analysis a descriptive name. Do not reuse the same object name for different things; doing so makes it hard to inspect intermediate results and debug errors.

```r
# Avoid
result <- compute_elderly(result)

# Prefer: distinct names
result_elderly  <- compute_elderly(result)
```
