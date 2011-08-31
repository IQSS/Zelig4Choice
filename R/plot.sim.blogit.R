#' Plot Simulated Quantities of Interest for the \code{blogit} Zelig Model
#' @usage
#' \method{plot}{sim.blogit}(x, xlab="", user.par=FALSE, alt.col="red", ...)
#' @S3method plot sim.blogit
#' @param x a \code{sim.blogit} object containing quantities of interest
#' @param xlab x-axis label
#' @param user.par a \code{par} specifying the output layout
#' @param alt.col the color of the histograms
#' @param ... additional parameters to pass to the contour plot of expected
#'   values
#' @return the original \code{par} object
#' @author Matthew Owen \email{mowen@@iq.harvard.edu}
plot.sim.blogit <- function(x, xlab = "",
                            user.par = FALSE, alt.col = "red",
                            ...) {
  # number of quantities of interest
  k <- length(x$qi)

  # get original object
  op <- par(no.readonly = TRUE)

  # user parameters (??)
  if (!user.par)
    par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))

  # split panes correctly
  par(mfrow = c(k, 1))

  # remove risk ratios
  if ("Risk Ratios: Pr(Y=k|X1) / Pr(Y=k|X)" %in% names(x$qi))
    k <- k - 1

  #
  main <- names(x$qi)

  # set panels
  par(mfrow = c(k, 1))


  qi <- x$qi
  labels <- names(attr(qi, '.index'))
  k <- 0
  size <- length(labels)

  for (key in labels) {
    val <- qi[[key]]

    if (key == "Predicted Values: Y=k|X") {
      total <- sum(as.integer(val))

      # 
      y00 <- 100 * sum(as.integer(val[,1]))/total
      y01 <- 100 * sum(as.integer(val[,2]))/total
      y10 <- 100 * sum(as.integer(val[,3]))/total
      y11 <- 100 * sum(as.integer(val[,4]))/total

      # 
      xmax <- max(y00, y01, y10, y11)
      labels <- c("(0,0)", "(0,1)","(1,0)", "(1,1)")

      # act
      barplot(c(y00, y01, y10, y11),
              horiz = TRUE,
              col = alt.col,
              names.arg = labels,
              xpd = TRUE,
              main = key,
              xlim = c(0, min(100, 1.25*xmax)),
              xlab = "Percentage of Simulations"
              )
    }
    else if (key == "Risk Ratios: Pr(Y=k|X1) / Pr(Y=k|X)") {
    }
    else if (is.numeric(val)) {
      y1 <- val[, 3] + val[, 4]
      y2 <- val[, 2] + val[, 4]

      #
      contour(kde2d(y1, y2),
              xlab = "Pr(Y1 = 1)", 
              ylab = "Pr(Y2 = 1)",
              main = key,
              ...
              )
    }

  }

  # return original plotting stream
  par(op)
}
