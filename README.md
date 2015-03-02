# Tmisc

The `Tmisc` package is where I keep miscellaneous functions. Most of them were pulled from my `.Rprofile`, and many were stolen from other packages or random places around the web. I tried to give credit where credit is due in the documentation. Here's what the package currently does:

* `addRawFC`: adds raw fold change to limma `topTable` output
* `datename`: add today's date to a string. E.g. `datename(output.csv)` returns `2014-03-15-output.csv` if today were the Ides of March
* `dfclass`: prints the class of all data frame columns in a friendly format
* `ipak`: shortcut to `install.packages`
* `lsa`: lists all the objects in the environment, their type, size, and dimensions
* `lsp`: lists functions and how to call them for any package.
* `mergett`: merges a limma `topTable` with the original expression data in the `ExpressionSet`
* `%nin%`: opposite of `%in%`
* `o`: opens the current working directory in finder on a Mac
* `propmiss`: get missingness statistics on columns in a data.frame
* `read.cb`: reads data from the clipboard
* `rownames_to_symprobes`: don't worry about it. Used in my microarray analysis pipeline. See `?rownames_to_symprobes` if you're that interested.
* `sicb`: writes the output of `sessionInfo()` to the clipboard (only works on Mac)
* `unfactor`: change all factor variables to character variables in a data frame
* `unrowname`: removes row names from a data.frame


## Installation

This package requires a Bioconductor installation:

```coffee
source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")
```

Download the zip or tarball and install with `R CMD INSTALL`, or better yet, use [devtools](https://github.com/hadley/devtools) to install directly from GitHub:

```coffee
# Install the package
library(devtools)
install_github("stephenturner/Tmisc")

# Load the package (once)
library(Tmisc)
```
