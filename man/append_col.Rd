% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{append_col}
\alias{append_col}
\title{Append column using position.}
\usage{
append_col(x, cols, after = length(x))
}
\arguments{
\item{x}{data frame.}

\item{cols}{column to insert into data frame.}

\item{after}{the inserted column would be located after `(i)` position. `If \code{after=1}, the column will be located in the 2nd position.}
}
\description{
Insert column between columns based on positions.
}
