\name{plot.sim.bprobit}
\alias{plot.sim.bprobit}
\title{Plot Simulated Quantities of Interest for the \code{bprobit} Zelig Model}
\usage{
  \method{plot}{sim.bprobit}(x, xlab="", user.par=FALSE,
    alt.col="red", ...)
}
\arguments{
  \item{x}{a \code{sim.bprobit} object containing
  quantities of interest}

  \item{xlab}{x-axis label}

  \item{user.par}{a \code{par} specifying the output
  layout}

  \item{alt.col}{the color of the histograms}

  \item{...}{additional parameters to pass to the contour
  plot of expected values}
}
\value{
  the original \code{par} object
}
\description{
  Plot Simulated Quantities of Interest for the
  \code{bprobit} Zelig Model
}
\author{
  Matthew Owen \email{mowen@iq.harvard.edu}
}

