# Tmisc 0.1.16

- New functions `%like%` and `%nlike%` work like `%in%` and `%nin%`, but where the second argument is a regular expression to match.
- New function `gt2refalt()` creates a two-letter genotype from a GT field from a VCF file.
- Installs an RStudio add-in to insert `%in%`.
- Added aliases for low-level manipulation in a dplyr pipeline:
    - `brackets()`
    - `dollar()`
    - `is_in()`
    - `and()`
    - `or()`
    - `equals()`
    - `is_gt()`
    - `is_geq()`
    - `is_lt()`
    - `is_leq()`
    - `not()`
    - `set_colnames()`
    - `set_rownames()`
    - `set_names()`
    - `set_attributes()`
    - `set_attr()`

# Tmisc 0.1.15

- New function `mat2df()` creates a pairwise distance data frame from a square matrix.

# Tmisc 0.1.14

- `keep_top_n()` thanks to [Jeroen Janssens](https://gist.github.com/jeroenjanssens/1c628c7e07429e5f0f2245e8598ec8e9).
- `deseqresult2tbl()` now lets you name the column extracted from the DESeq result's `rownames` anything you want.
- Added the [Datasaurus Dozen](https://www.autodeskresearch.com/publications/samestats) built-in dataset (`datasaurus`).
- Small updates to enable documentation with [pkgdown](https://github.com/hadley/pkgdown).
- Inconsequential update to `%nin%`.

# Tmisc 0.1.13

- `Cs()` (from [Hmisc](https://cran.r-project.org/package=Hmisc)): Convenience function to create a quoted character vector from unquoted strings.

# Tmisc 0.1.12

- Minor bugfixes

# Tmisc 0.1.11

- `convert_to_NA()` converts string values to true `NA` values (adapted from [sfirke/janitor](https://cran.r-project.org/package=janitor))
- `clean_names()` cleans up the names in a data frame. Resulting names are unique and consist only of the `_` character, lowercase letters, and numbers (taken from [sfirke/janitor](https://cran.r-project.org/package=janitor)).
- `remove_empty_rows()` and `remove_empty_cols()` removes any rows or columns composed entirely of `NA` values (taken from [sfirke/janitor](https://cran.r-project.org/package=janitor)).

# Tmisc 0.1.10

- `Tpairs()` displays better pairwise scatterplot matrices than `pairs()`.
- `Thist()` plots a histogram with either a normal distribution or density plot overlay.
- `corner()` displays the first few rows and columns of a data frame or matrix.
- `beep()` plays a short beep to alert when something's done (from the beepr package).

# Tmisc 0.1.9

- Minor bugfixes.

# Tmisc 0.1.8

- `jsd()` compute the Jensen-Shannon divergence from a matrix of probabilities.
- `Mode()` returns the mode of a vector.
- `gg_na()` produces a plot showing the missing values in a data frame.
- `are_all_equal()` assess whether all the components of a numeric vector are (approximately) equal.

# Tmisc 0.1.7

- `gghues()` generates color palette emulating ggplot2's default hues.

# Tmisc 0.1.6

- `fisherp()` uses Fisher's method to combine p-values.

# Tmisc 0.1.5

- `dokuwiki()` will convert a data.frame to a dokuwiki-formatted table, and will copy said table to the system clipboard if you're on Mac OS X.

# Tmisc 0.1.4

- `strSort()` alphabetically sorts characters in a string. 
