\name{zelig2mlogit}
\alias{zelig2mlogit}
\title{interface between the Zelig model mlogit and the pre-existing function}
\usage{
  zelig2mlogit(formula, weights=NULL, repweights=NULL, ..., data)
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
  a list specifying '.function'
}
\description{
  interface between the Zelig model mlogit and the
  pre-existing function
}

