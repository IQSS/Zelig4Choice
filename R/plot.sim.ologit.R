#' @S3method plot sim.ologit
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
  if(is.null(alt.color)){
    alt.color <- rainbow(num.factors)
  }
 
  # These are the current color defaults
  # Need to add argument to overwrite
  color.x <- rgb(242, 122, 94, maxColorValue=255)
  color.x1 <- rgb(100, 149, 237, maxColorValue=255)

  if (is.null(x$x)){
    # if there is no x, then something is probably wrong
    # and we just quit
    return(par(original.par))

  }else if (is.null(x$x1) || is.na(x$x1)) {
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
    panels <- matrix(1:6, nrow=3, ncol=2, byrow=TRUE)
    # the plotting device:
    #
    # +-----------+
    # |  1  |  2  |
    # +-----+-----+
    # |  3  |  4  |
    # +-----+-----+
    # |  5  |  6  |
    # +-----------+
  }

  # layout graphing device
  layout(panels)

  is.one.na<-function(x){
  	if (length(x)==1) 
  	  if (is.na(x))
  	    return(TRUE)
  	    
  	return(FALSE)
  }


  pr<-x$qi$pv1
  if(!is.null(pr)) pr<-as.character(pr)
  pr1<-x$qi$pv2
  if(!is.null(pr1) & !is.one.na(pr1)) pr1<-as.character(pr1)

  # Predicted Values for X
  if(!is.null(pr))  Zelig:::simulations.plot( pr, main = "Predicted Values: Y|X", col=color.x,  line.col = "black")

  # Predicted Values for X1
  if(!is.null(pr1)) Zelig:::simulations.plot( pr1, main = "Predicted Values: Y|X1", col=color.x1, line.col = "black")

  # Expected Values for X
  .plot.ev(qi$ev1, main="Expected Values: Pr(Y=j|X)", alt.colors=alt.color)
  
  # Expected Values for X1
  .plot.ev(qi$ev2, main="Expected Values: Pr(Y=j|X1)", alt.colors=alt.color)

  # Compare Predicted Values Distributions

    if(!is.null(pr) & !is.null(pr1) & !is.one.na(pr) & !is.one.na(pr1) )
	  Zelig:::simulations.plot(
                      y=pr, y1=pr1,
                      xlab="", ylab="",
      				  main = "Comparison of Y|X and Y|X1",
      				  # Note that we are adding transparency to this
      				  col = paste(c(color.x, color.x1), "80", sep=""),
      				  line.col = "black")

  # First Differences
  .plot.ev(
           qi$fd,
           main="First Differences: Pr(Y=j|X1) - Pr(Y=j|X)",
           alt.colors=alt.color
           )

  # ignore risk ratios
  #

  # restore, and return invisibility
  par(original.par)
}

# Currently Replaced by Zelig:::simulations.plot()
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
.plot.ev <- function(ev, xlab="", ylab="", main="", alt.colors=NULL ) {
  if (is.na(ev) || is.null(ev))
    return()

  dense <- apply(ev, 2, density)
  left <- min(0, ev)
  right <- max(ev)
  top <- max(unlist(Map(function (x) max(x$y), dense)))

  plot(dense[[1]], col = alt.colors,
       xlab=xlab, ylab=ylab, main=main,       
       xlim = c(left, right),
       ylim = c(0, top)
       )
  Map(lines, dense[-1], col = alt.colors[-1])
  legend("topright", colnames(ev), fill=alt.colors)
}
