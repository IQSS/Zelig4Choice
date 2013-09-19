\name{zelig2oprobit}
\alias{zelig2oprobit}
\title{Interface between \code{oprobit} model and Zelig}
\usage{
  zelig2oprobit(formula, weights=NULL, repweights=NULL, ..., data)
}
\arguments{
  \item{formula}{a \code{formula}}
\item{weights}{A set of non-negative value weights.  Overrides repweights if defined.}
\item{repweights}{A set of whole number (non-negative integer) weights.  Useful if 
   weights are just for making copies of or deleting certain observations or for
   frequency weights.}

  \item{...}{ignored parameters}

  \item{data}{a \code{data.frame}}
}
\value{
  a list to be coerced into a zelig.call object
}
\description{
  Interface between \code{oprobit} model and Zelig
}
\note{
  This function is exclusively for use by the \code{zelig}
  function
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

