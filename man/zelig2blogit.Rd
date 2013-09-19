\name{zelig2blogit}
\alias{zelig2blogit}
\title{Interface between \code{blogit} model and Zelig
This function is exclusively for use by the \code{zelig} function}
\usage{
  zelig2blogit(formula, weights=NULL, repweights=NULL, ..., data)
}
\arguments{
  \item{formula}{a formula}
\item{weights}{A set of non-negative value weights.  Overrides repweights if defined.}
\item{repweights}{A set of whole number (non-negative integer) weights.  Useful if 
   weights are just for making copies of or deleting certain observations or for
   frequency weights.}
  \item{...}{ignored parameters}
  \item{data}{a data.frame}
}
\value{
  a list to be coerced into a zelig.call object
}
\description{
  Interface between \code{blogit} model and Zelig This
  function is exclusively for use by the \code{zelig}
  function
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

