#' Plot Simulated Quantities of Interest for the \code{bprobit} Zelig Model
#' @usage
#' \method{plot}{sim.bprobit}(x, xlab="", user.par=FALSE, alt.col="red", ...)
#' @S3method plot sim.bprobit
#' @param x a \code{sim.bprobit} object containing quantities of interest
#' @param xlab x-axis label
#' @param user.par a \code{par} specifying the output layout
#' @param alt.col the color of the histograms
#' @param ... additional parameters to pass to the contour plot of expected
#'   values
#' @return the original \code{par} object
#' @author Matthew Owen \email{mowen@@iq.harvard.edu}
plot.sim.bprobit <- plot.sim.blogit
