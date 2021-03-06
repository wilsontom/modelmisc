% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_kappa.R
\name{get_kappa}
\alias{get_kappa}
\title{Cohen's Kappa}
\usage{
get_kappa(train_model, test_data, test_class)
}
\arguments{
\item{train_model}{a valid model object}

\item{test_data}{a \code{data.frame} to be used for prediction}

\item{test_class}{a vector of class lables for \code{test_data}}
}
\value{
a numeric value for Kappa
}
\description{
Get the overall interate agreement rate (Cohen's Kappa)
}
