\name{UPDATE.CALL.FORMULA}
\alias{UPDATE.CALL.FORMULA}
\title{Hook to Clean-up the Function Signature}
\usage{
  UPDATE.CALL.FORMULA(obj, zelig.call, call, ...)
}
\arguments{
  \item{obj}{a \code{zelig} object}

  \item{zelig.call}{the call to the \code{zelig} function}

  \item{call}{a function call}

  \item{...}{ignored aprameters}
}
\value{
  the \code{obj} parameter with a modified call slot
}
\description{
  Hook to Clean-up the Function Signature
}
\note{
  This is _NOT_ cosmetic! MNP evaluates choiceX by looking
  at the call signature. This is necessary to make the
  model work with the function predict.mnp
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

