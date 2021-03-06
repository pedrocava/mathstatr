% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vector_norm.R
\name{norm}
\alias{norm}
\title{N-Norm of a vector}
\usage{
norm(v, n = 2)
}
\arguments{
\item{v}{must be a numerical vector}

\item{n}{must be an integer. Defaults to 2.}
}
\value{
The vector's norm
}
\description{
Calculates the n-norm of a vector
}
\examples{

v = rnorm(n = 200)
norm(v)

}
