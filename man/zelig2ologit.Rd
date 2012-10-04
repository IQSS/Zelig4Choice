\name{zelig2ologit}
\alias{zelig2ologit}
\title{Interface between \code{ologit} model and Zelig}
\usage{
  zelig2ologit(formula, weights = 1, ..., data)
}
\arguments{
  \item{formula}{a \code{formula}}

  \item{weights}{a numeric vector}

  \item{...}{ignored parameters}

  \item{data}{a \code{data.frame}}
}
\value{
  a list to be coerced into a zelig.call object
}
\description{
  Interface between \code{ologit} model and Zelig
}
\note{
  This function is exclusively for use by the \code{zelig}
  function
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

