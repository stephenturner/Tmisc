% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{corner}
\alias{corner}
\title{Print the top left corner of a data frame}
\usage{
corner(x, n = 5)
}
\arguments{
\item{x}{A data.frame.}

\item{n}{The number of rows/columns to print.}
}
\value{
The corner of the data frame
}
\description{
Prints the first n rows and columns of a data frame or matrix.
}
\examples{
corner(mtcars)
corner(iris, n=4)

}
