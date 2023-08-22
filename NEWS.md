# Tmisc 1.0.1

- Remove functions deprecated >2 years ago.
- Fix package level documentation.

# Tmisc 1.0.0

- Bugfixes for R devel checks.
- Move orphan functions to utils.
- Deprecated many long superseded functions.
- Deprecated some no longer useful gene expression functions.
- New PDF template.

# Tmisc 0.1.22

- Bugfixes for R 4.0.0 compatibility.

# Tmisc 0.1.20

- Deprecated several functions in favor of tidyverse alternatives.
- Modified PDF template.

# Tmisc 0.1.19 

This is primarily a cleanup release. Several functions were included in the Tmisc package early on while packages such as [janitor](https://CRAN.R-project.org/package=janitor) were in development. This release removes (note, _removes_, doesn't deprecate) functions that were previously copied from other packages, introducing a few potentially breaking changes.

- Removed `set_names()` as this is exported from rlang.
- Removed `beep()`. Use the **[beepr](https://CRAN.R-project.org/package=beepr)** package instead.
- Removed `remove_empty_cols()`, `remove_empty_rows()`, `convert_to_NA()` and `clean_names()`. Use `remove_empty()`, etc., from the **[janitor](https://CRAN.R-project.org/package=janitor)** package instead.
- Removed `unrowname()`. Use `magrittr::set_rownames()` (loaded with tidyverse).

# Tmisc 0.1.18

- Added an RMarkdown template.

# Tmisc 0.1.17

- Add in to insert ` = `. Optionally map the keyboard shortcut to Alt+`-` to insert `=` instead of the traditional `<-`.

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
- Added the Datasaurus Dozen.
- Small updates to enable documentation with [pkgdown](https://github.com/r-lib/pkgdown).
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
