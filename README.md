# Tmisc

The `Tmisc` package is where I keep miscellaneous functions. Most of them were pulled from my `.Rprofile`, and many were stolen from other packages or random places around the web. I tried to give credit where credit is due in the documentation. Here's what the package currently does:

* `addRawFC`: adds raw fold change to limma `topTable` output
* `datename`: add today's date to a string. E.g. `datename(output.csv)` returns `2014-03-15-output.csv` if today were the Ides of March
* `h`: a shortcut for `head`
* `ht`: `rbind`s the output of `head` and `tail`
* `lsa`: lists all the objects in the environment, their type, size, and dimensions
* `mergett`: merges a limma `topTable` with the original expression data in the `ExpressionSet`
* `%nin%`: opposite of `%in%`
* `o`: opens the current working directory in finder on a Mac
* `propmiss`: get missingness statistics on columns in a data.frame
* `read.cb`: reads data from the clipboard
* `read.gist`: reads data from a GitHub gist
* `rownames_to_symprobes`: don't worry about it. Used in my microarray analysis pipeline. See `?rownames_to_symprobes` if you're that interested.
* `unfactor`: change factors to character variables
* `unrowname`: removes row names from a data.frame


## Installation

Download the zip or tarball and install with `R CMD INSTALL`, or better yet, use [devtools](https://github.com/hadley/devtools) to install directly from GitHub:

```coffee
# Install the package
library(devtools)
install_github("stephenturner/Tmisc")

# Load the package (once)
library(Tmisc)
```
