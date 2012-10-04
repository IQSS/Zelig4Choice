#' plot.sim.ologit
#' @param x a \code{sim.ologit} object
#' @param ... ignored parameters
#' @param alt.color list of colors to print with
#' @return function is used for its side-effects
#' @export
plot.sim.ologit <- function(x, ..., alt.color=NULL) {
  # num.factors <- length(
  original.par <- par(no.readonly=TRUE)

  # the following variables are:
  #  quantities of interest
  #  summarized data
  #  number of simulations
  qi <- x$qi
  summ <- x$stats
  num <- x$iterations

  # number of factors
  num.factors <- ncol(qi$ev1)
  alt.colors <- rainbow(num.factors)

  if (is.null(x$x))
    # if there is no x, then something is probably wrong
    # and we just quit
    return(par(original.par))

  else if (is.null(x$x1) || is.na(x$x1)) {
    panels <- matrix(c(1, 2), nrow=2)
  }
    # the plotting device:
    #
    # +--------+
    # |   1    |
    # +--------+
    # |   2    |
    # +--------+


  else {
    panels <- matrix(c(1, 3, 5, 2, 4, 5), ncol=2)
    # the plotting device:
    #
    # +-----------+
    # |  1  |  2  |
    # +-----+-----+
    # |  3  |  4  |
    # +-----+-----+
    # |   5       |
    # +-----------+
  }

  # layout graphing device
  layout(panels)

  # Predicted Values for X
  .plot.pv(summ, label="Predicted Values (for X): Y=j|X", alt.colors=alt.colors)

  # Predicted Values for X
  .plot.pv(summ, label="Predicted Values (for X1): Y=j|X1", alt.colors=alt.colors)

  # Expected Values for X
  .plot.ev(qi$ev1, label="Expected Values (for X): Pr(Y=j|X)", alt.colors=alt.colors)
  
  # Expected Values for X1
  .plot.ev(qi$ev2, label="Expected Values (for X1): Pr(Y=j|X1)", alt.colors=alt.colors)

  # First Differences
  .plot.ev(
           qi$fd,
           label="First Differences: Pr(Y=j|X1) - Pr(Y=j|X)",
           alt.colors=alt.colors
           )

  # ignore risk ratios
  #

  # restore, and return invisibility
  par(original.par)
}


#' Plot Predicted Values
#' @param summ a matrix 
#' @param label a character-string specifying which QI to extract
.plot.pv <- function(summ, label="", alt.colors=NULL) {
  if (is.null(summ[[label]]) || is.na(summ[[label]]) || is.null(summ[[label]]))
    return()

  bars <- 100 * summ[[label]][,1]
  labels <- rownames(summ[[label]])
  barplot(
          bars,
          horiz = TRUE,
          col = alt.colors,
          names.arg = labels,
          las = 1,
          main = label,
          xlab = "Percent of Simulations",
          xlim = c(0, min(100, 1.10 * max(bars)))
          )
}

#' Plot Expected Values
#' @param ev a matrix containing information about simulated expected values
#' @param label a character-string specifying the plot's title
.plot.ev <- function(ev, label="", alt.colors=NULL) {
  if (is.na(ev) || is.null(ev))
    return()

  dense <- apply(ev, 2, density)
  left <- min(0, ev)
  right <- max(ev)
  top <- max(unlist(Map(function (x) max(x$y), dense)))

  plot(dense[[1]], col = alt.colors,
       main = label,
       xlim = c(left, right),
       ylim = c(0, top)
       )
  Map(lines, dense[-1], col = alt.colors[-1])
  legend("topright", colnames(ev), fill=alt.colors)
}
