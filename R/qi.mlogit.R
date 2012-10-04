#' compute quantities of interest for the Zelig model mlogit
#' @usage \method{qi}{mlogit}(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi mlogit
#' @param obj a zelig object
#' @param x a setx object
#' @param x1 an optional setx object
#' @param y ...
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of
#'         quantities of interest with their simulations
qi.mlogit <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {
  # get fitted model object
  fitted <- GetObject(obj)

  # get constraints from fitted model
  constraints <- fitted@constraints
  coef <- coef(param)
  ndim <- ncol(fitted@y) - 1
  all.coef <- NULL

  # ...
  v <- .construct.v(constraints, ndim)

  # put all indexed lists in the appropriate section
  for (i in 1:ndim)
    all.coef <- c(all.coef, list(coef[, v[[i]]]))

  #
  cnames <- ynames <- if (is.null(colnames(fitted@y)))
    1:(ndim+1)
  else
    colnames(fitted@y)

  # ...
  cnames <- paste('Pr(Y=', cnames, ')', sep='')

  #
  ev1 <- .compute.ev(fitted, constraints, all.coef, x, ndim, cnames)
  ev2 <- .compute.ev(fitted, constraints, all.coef, x1, ndim, cnames)
  pv1 <- .compute.pv(fitted, ev1, ynames)
  pv2 <- .compute.pv(fitted, ev2, ynames)

  list(
       "Expected Values: E(Y|X)" = ev1,
       "Expected Values: E(Y|X1)" = ev2,
       "Predicted Values: Y|X" = pv1,
       "Predicted Values: Y|X1" = pv2,
       "First Differences: E(Y|X1) - E(Y|X)" = ev2 - ev1,
       "Risk Ratios: E(Y|X1)/E(Y|X)" = ev2/ev1
       )
}


#' Split Names of Vectors into N-vectors
#' This function is used to organize how variables are spread
#' across the list of formulas
#' @param constraints a constraints object
#' @param ndim 
#' @return a list of character-vectors
#' @author Matt Owen and Olivia Lau and Kosuke Imai
.construct.v <- function(constraints, ndim) {
  v <- rep(list(NULL), ndim)

  names <- names(constraints)

  for (i in 1:length(constraints)) {
    cm <- constraints[[i]]

    for (j in 1:ndim) {
      if (sum(cm[j,]) == 1) {

        v[[j]] <- if (ncol(cm) == 1)
          c(v[[j]], names[i])
        else
          c(v[[j]], paste(names[i], ':', j, sep=""))
      }
    }
  }

  v
}

#' Simulate Expected Value for Multinomial Logit
#' @param fitted a fitted model object
#' @param constraints a constraints object
#' @param all.coef all the coeficients
#' @param x a setx object
#' @param ndim an integer specifying the number of dimensions
#' @param cnames a character-vector specifying the names of the columns
#' @return a matrix of simulated values
#' @author Matt Owen and Olivia Lau and Kosuke Imai \email{mowen@@iq.harvard.edu}
.compute.ev <- function (fitted, constraints, all.coef, x, ndim, cnames) {
  if (is.null(x))
    return(NA)

  linkinv <- fitted@family@linkinv

  xm <- rep(list(NULL), ndim)
  sim.eta <- NULL
  x <- as.matrix(x)

  # ...
  for (i in 1:length(constraints)) {
    for (j in 1:ndim)
      if (sum(constraints[[i]][j,]) == 1)
        xm[[j]] <- c(xm[[j]], x[, names(constraints)[i]])
  }

  # ...

  for (i in 1:ndim)
    sim.eta <- cbind(sim.eta, all.coef[[i]] %*% as.matrix(xm[[i]]))

  # ...
  ev <- linkinv(sim.eta)
  colnames(ev) <- cnames

  # return expected values
  ev
}


#' Simulate Predicted Values
#' @param fitted a fitted model object
#" @param ev the simulated expected values
#' @param ynames ???
#' @return a vector of simulated values
#' @author Matt Owen and Olivia Lau and Kosuke Imai \email{mowen@@iq.harvard.edu}
.compute.pv <- function (fitted, ev, ynames) {
  if (all(is.na(ev)))
    return(NA)

  # initialize predicted values and a matrix
  pr <- NULL
  Ipr <- sim.cut <- matrix(NA, nrow=nrow(ev), ncol(ev))

  # ...
  k <- ncol(ev)
  colnames(Ipr) <- colnames(sim.cut) <- colnames(ev)

  sim.cut[, 1] <- ev[, 1]

  for (j in 2:k)
    sim.cut[, j] <- sim.cut[ , j-1] + ev[, j]

  #
  tmp <- runif(nrow(ev), min=0, max=1)

  # ...
  for (j in 1:k)
    Ipr[, j] <- tmp > sim.cut[, j]

  # ...
  for (j in 1:nrow(Ipr))
    pr[j] <- 1 + sum(Ipr[j, ])

  pr <- factor(pr, levels = sort(unique(pr)), labels = ynames)
  pr <- factor(pr, ordered = FALSE)
  pr.matrix <- matrix(pr, nrow = dim(ev)[1])
  levels(pr.matrix) <- levels(pr)
  pr.matrix
}
