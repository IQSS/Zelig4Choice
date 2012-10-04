#' summarize.ologit
#' summarizes simulations of quantities of interest
#' @param qi a quantity of interest object
summarize.oprobit <- function(qi) {
  # number of rows is based on the number
  # of columns returned from the qi function
  q <- qi

  nrows <- ncol <- ncol(q$ev1)


  ev1.matrix <- .summarize.ev(q$ev1)
  ev2.matrix <- .summarize.ev(q$ev2)

  # Predicted Values for X and X1
  # -----------------------------
  pr1 <- as.matrix(table(q$pv1)/length(q$pv1))
  rownames(pr1) <- paste("Y=", colnames(q$ev1), "|X", sep="")
  colnames(pr1) <- ""

  pr2 <- fd.matrix <- rr.matrix <- NA

  if (!all(is.na(ev2.matrix))) {
    #
    pr2 <- as.matrix(table(q$pv2)/length(q$pv2))
    rownames(pr2) <- paste("Y=", colnames(q$ev2), "|X1", sep="")
    colnames(pr2) <- ""
    
    # First Differences
    # -----------------
    # construct the matrix
    fd.matrix <- matrix(NA, ncol=3, nrow=ncol)
    colnames(fd.matrix) <- c("mean", "sd", "E[y=j|X1] - E[y=j|X]")
    rownames(fd.matrix) <- paste(
                                 "Pr(Y=", colnames(q$ev2),"|X1) - ",
                                 "Pr(Y=", colnames(q$ev1),"|X)",
                                 sep=""
                                 )

  
    for (k in 1:ncol)
      fd.matrix[k, ] <- c(
                          mean(q$fd[k,]),
                          sd(q$fd[k,]),
                          ev1.matrix[k,1] - ev2.matrix[k,1]
                          )


    # Risk Ratios
    # -----------
    rr.matrix <- matrix(NA, ncol=5, nrow=ncol)
    rownames(rr.matrix) <- paste(
                                 "Pr(Y=", colnames(q$ev2),"|X1) / ",
                                 "Pr(Y=", colnames(q$ev1),"|X)",
                                 sep=""
                                 )
    colnames(rr.matrix) <- c("mean", "sd", ".5", ".025", ".975")

    for (k in 1:ncol) {
      rr.matrix[k, ] <- c(
                          mean(q$rr[k, ]),
                          sd(q$rr[k, ]),
                          quantile(q$rr[k, ], c(.5, .025, .975))
                         )
    }
  }

  # return values
  # NOTE: this is very similar to qi.ologit
  summarized <- list(
       "Expected Values (for X): Pr(Y=j|X)" = ev1.matrix,
       "Expected Values (for X1): Pr(Y=j|X1)" = ev2.matrix,
       "Predicted Values (for X): Y=j|X" = pr1,
       "Predicted Values (for X1): Y=j|X1" = pr2,
       "First Differences: Pr(Y=j|X1) - Pr(Y=j|X)" = fd.matrix,
       "Risk Ratios: Pr(Y=k|X1) / Pr(Y=j|X)" = rr.matrix
       )

  class(summarized) <- "summarized.qi"
  summarized
}


# .summarize.ev
.summarize.ev <- function(ev, names=NULL) {
  if (!is.matrix(ev) || is.na(ev) || is.null(ev))
    return(NA)

  # initialize an empty matrix
  ev.matrix <- matrix(NA, ncol=5, nrow=ncol(ev))

  # name columns and rows appropriately
  rownames(ev.matrix) <- paste("Pr(Y=", colnames(ev), "|X)", sep="")
  colnames(ev.matrix) <- c("mean", "sd", ".5", ".025", ".975")

  # actually compute the statistics
  for (k in 1:nrow(ev.matrix))
    ev.matrix[k,] <- c(
                       mean(ev[,k]), 
                       sd(ev[,k]),
                       quantile(ev[,k], c(.5, .025, .975))
                       )

  ev.matrix
}


# .compute.fd
.summarize.fd <- function(ev, names=NULL) {


}
