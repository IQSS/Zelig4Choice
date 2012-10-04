\name{plot.sim.blogit}
\alias{plot.sim.blogit}
\title{Plot Simulated Quantities of Interest for the \code{blogit} Zelig Model}
\usage{
  \method{plot}{sim.blogit}(x, xlab="", user.par=FALSE,
    alt.col="red", ...)
}
\arguments{
  \item{x}{a \code{sim.blogit} object containing quantities
  of interest}

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
  \code{blogit} Zelig Model
}
\author{
  Matthew Owen \email{mowen@iq.harvard.edu}
}

