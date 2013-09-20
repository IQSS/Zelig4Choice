#' Simulate Quantities of Interest for \code{ologit} Model
#' @S3method qi ologit
#' @usage \method{qi}{ologit}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @note This function is paraphrased from Zelig v3.4.0-1
#' @param obj zelig object
#' @param x setx object
#' @param x1 setx object
#' @param y ATT variable
#' @param num implicitly called by sim - number of simulations to run
#' @param param param object contains: link, link-inverse, simulations, ancillary parameters
#' @return a list containing simulated quantities of interest
qi.ologit <- function(obj, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  z <- obj

  # startup work
  simulations <- coef(param)
  coef <- coef(z)
  linkinv <- linkinv(param)

  # simulations on coefficients
  sim.coef <- simulations[,1:length(coef), drop=FALSE]

  # remove (Intercept), make sure matrix is numeric
  mat1 <- as.numeric(as.matrix(x)[,-1])
  mat2 <- if (is.null(x1))
    NULL
  else
    as.numeric(as.matrix(x1)[,-1])

  # compute eta
  eta1 <- t(mat1 %*% t(sim.coef))
  eta2 <- if (is.null(x1))
    NULL
  else
    t(mat2 %*% t(sim.coef))

  # simultations on zeta, and define theta
  sim.zeta <- sim.theta <- simulations[,(length(coef)+1):ncol(simulations), drop=FALSE]
  sim.zeta[,-1] <- exp(sim.theta[,-1])
  sim.zeta <- t(apply(sim.zeta, 1, cumsum))

# compute expected values, etc.
# CRAN policy to avoid self referential :::, even if harmless and clarifying
# because "almost never need[ed]"
#  ev1 <- ZeligChoice:::.compute.ev(z, x, num, param, eta1, sim.zeta)
#  pr1 <- ZeligChoice:::.compute.pr(z, x, num, param, eta1, sim.zeta)
#  ev2 <- ZeligChoice:::.compute.ev(z, x1, num, param, eta2, sim.zeta)
#  pr2 <- ZeligChoice:::.compute.pr(z, x1, num, param, eta2, sim.zeta)
#  so:
  ev1 <- .compute.ev(z, x, num, param, eta1, sim.zeta)
  pr1 <- .compute.pr(z, x, num, param, eta1, sim.zeta)
  ev2 <- .compute.ev(z, x1, num, param, eta2, sim.zeta)
  pr2 <- .compute.pr(z, x1, num, param, eta2, sim.zeta)


  # return value
  list(
       "Expected Values: P(Y=j|X)" = ev1,
       "Expected Values: P(Y=j|X1)" = ev2,
       "Predicted Values: Y|X" = pr1,
       "Predicted Values: Y|X1" = pr2,
       "First Differences: P(Y=j|X1) - P(Y=j|X)" = ev2 - ev1,
       "Average Treatment Effect for Expected Values: " = NA,
       "Risk Ratios" = ev2 / ev1
       )
}


# .compute.ev
# computes expected values
# @param z zelig object
# @param x setx object
# @param num number of simulations
# @param param param object
.compute.ev <- function(z, x, num, param, eta, sim.zeta) {
  if (is.null(x))
    return(NA)

  simulations <- coef(param)
  coef <- coef(z)
  linkinv <- linkinv(param)

  # simulations on coefficients
  sim.coef <- simulations[,1:length(coef), drop=FALSE]

  #
  k <- length(z$result$zeta)+1

  # remove (Intercept), make sure matrix is numeric
  mat <- as.numeric(as.matrix(x)[,-1])

  # compute eta
  eta <- t(mat %*% t(sim.coef))
  #eta <- sim.coef %*% t(mat)

  lev <- z$result$lev

  rows <- as.matrix(x)


  Ipr <- cuts <- tmp0 <- {
    array(
          0, dim = c(num, k, nrow(rows)),
          dimnames = list(1:num, lev, rownames(rows))
          )
  }

  for (i in 1:num) {
    cuts[i,,] <- t(linkinv(eta[i,], sim.zeta[i,]))
  }

  # NOTE:
  #  2:k => 2, 3, 4, ..., k
  #  2:k-1 => 1, 2, 3, 4, ..., k-1
  tmp0[,2:k,] <- cuts[,2:k-1,]
  
  # why not. this was copied over from uncommented
  # code, so part of me has no clue what's going on here
  ev <- cuts - tmp0

  dimnames(ev) <- list(1:num, z$result$lev, rownames(x))

  # remove unnecessary dimensions
  if (dim(ev)[3] == 1)
    ev <- ev[,,1]

  colnames(ev) <- z$result$lev

  # return expected values
  ev
}


#' .compute.pr
#' computes predicted values
#' @param z zelig object
#' @param x setx object
#' @param num number of simulations
#' @param param param object
.compute.pr <- function(z, x, num, param, eta, sim.zeta) {
  if (is.null(x))
    return(NA)

  x <- as.matrix(x)
  rows <- x

  k <- length(z$result$zeta) + 1

  lev <- z$result$lev

  Ipr <- cuts <- tmp0 <- {
    array(
          0, dim = c(num, k, nrow(rows)),
          dimnames = list(1:num, lev, rownames(rows))
          )
  }
  linkinv <- linkinv(param)
  for (i in 1:num) {
    cuts[i,,] <- t(linkinv(eta[i,], sim.zeta[i,]))
  }

  pr <- matrix(NA, nrow=num, ncol=nrow(as.matrix(x)))
  tmp <- matrix(
                runif(length(cuts[,1,]), 0, 1),
                nrow = num,
                ncol = nrow(x)
                )


  k <- length(z$result$zeta)+1
  for (j in 1:k) {
    Ipr[,j,] <- as.integer(tmp > cuts[,j,])
  }

  for (j in 1:nrow(x)) {
    pr[,j] <- 1 + rowSums(Ipr[,,j,drop=FALSE])
  }

  # ugh, what does this even do?
  factors <- factor(
                   pr, labels=lev[1:length(lev) %in%
                          sort(unique(pr))],
                    ordered = TRUE
                    )

  # 
  pr
}
