% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/canberra_distance.R
\name{canberra_distance}
\alias{canberra_distance}
\title{Canberra Distance}
\usage{
canberra_distance(x, y, scale = TRUE)
}
\arguments{
\item{x}{a numeric vector}

\item{y}{a numeric vector}

\item{scale}{logical; if \code{TRUE} then the canberra distance is scaled by the 1 - (maximum possible distance)
to give a value between 0 and 1. 1 = vectors are identical, 0 = no similiarity}
}
\value{
a numeric value for the canberra distance
}
\description{
Calculate the Canberra Distance between two vectors of feature ranks. Input vectors must both be numeric and have equal
cardinality.
}
\references{
Jurman, G., Merler, S., Barla, A., Paoli, S., Galea, A., Furlanello, C., 2008. \emph{Algebraic
stability indicators for ranked lists in molecular profiling}. Bioinformatics 24 (2):258-264
}
