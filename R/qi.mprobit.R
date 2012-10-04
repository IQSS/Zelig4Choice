#' Compute Quantities of Interest for the Zelig Mmodel "mprobit"
#' @usage \method{qi}{mprobit}(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi mprobit
#' @param obj a zelig object
#' @param x a setx object
#' @param x1 an optional setx object
#' @param y ...
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#' interest with their simulations
qi.mprobit <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  # get fitted model
  fitted <- GetObject(obj)

  # get correct data frames
  frame1 <- ZeligChoice:::prepare.frame(obj, x)
  frame2 <- ZeligChoice:::prepare.frame(obj, x1)

  # init
  ev1 <- ev2 <- NA
  pv1 <- pv2 <- NA

  # if both are null, just get a prediction
  if (is.null(x) && is.null(x1)) {

    # simple predictions
    simulations <- predict(fitted, n.draws = num)

    ev1 <- ZeligChoice:::compute.ev.mnp(simulations, TRUE, num)
    pv1 <- ZeligChoice:::compute.pv.mnp(simulations, TRUE, num)
  }

  # if x is not null, get predictions using x's setx data
  if (! is.null(x)) {

    # submit a hypothetical situation based on frame1
    # which contains the values of setx 'x'
    simulations1 <- predict(fitted, newdata = frame1, n.draws = num)

    ev1 <- ZeligChoice:::compute.ev.mnp(simulations1, x, num)
    pv1 <- ZeligChoice:::compute.pv.mnp(simulations1, x, num)
  }

  # if x1 is not null, get predictions using the x1's setx data
  if (! is.null(x1)) {
    simulations2 <- predict(fitted, newdata = frame2, n.draws = num)

    # submit a hypothetical situation based on frame2
    # which contains the values of setx 'x1'
    ev2 <- ZeligChoice:::compute.ev.mnp(simulations2, x1, num)
    pv2 <- ZeligChoice:::compute.pv.mnp(simulations2, x1, num)
  }


  


  list(
       "Expected Values: E(Y|X)"  = ev1,
       "Expected Values: E(Y|X1)" = ev2,
       "Predicted Values: Y=k|X"    = pv1,
       "Predicted Values: Y=k|X1"   = pv2,
       "First Differences: E(Y|X1) - E(Y|X)" = ev2 - ev1,
       "Risk Ratios: E(Y|X1) / E(Y|X)" = ev2 / ev1
       )
}

# Correctly Prepare a data.frame to Be Used with predict.mnp
# param obj a zelig object
# param x a setx object
# value a single-row data.frame with NA's for predicted variables
# and the correct values for explanatory variables (set via
# the setx object)
prepare.frame <- function (obj, x) {
  # if either obj or x is NULL, return NULL
  if (is.null(obj) || is.null(x))
    return(NULL)

  # get a single-row matrix with the correct values
  d <- x$updated

  # these variables aren't used to make predictions
  d[, x$explan] <- NA

  # these variables are
  d[, names(x$values)] <- x$values
  d
}

compute.ev.mnp <- function(simulations, x, num) {
  if (is.null(x))
    return(NA)

  ev <- simulations$p[1, , ]
  ev <- t(ev)

  ev
}

compute.pv.mnp <- function(simulations, x, num) {
  if (is.null(x))
    return(NA)

  pv <- simulations$o[1, , ] * num
  pv <- t(pv)
  pv <- apply(pv, 2, factor)

  levels(pv) <- 1:ncol(pv)

  pv
}
