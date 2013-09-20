#' Param Method for the \code{oprobit} Zelig Model
#' @note This method is used by the \code{oprobit} Zelig model
#' @S3method param oprobit
#' @usage \method{param}{oprobit}(obj, num=1000, ...)
#' @param obj a \code{zelig} object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a \code{parameters} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.oprobit <- function(obj, num=1000, ...) {
  z <- obj

  coef <- coef(z)
  zeta <- z$result$zeta
  theta <- zeta[1]

  for (k in 2:length(zeta))
    theta[k] <- log(zeta[k]-zeta[k-1])

  m <- matrix(
              mvrnorm(num, mu=c(coef,theta), Sigma=vcov(z)),
              nrow = num
              )

  lengthcoef <- length(coef)

  list(
       simulations =
       matrix(
         mvrnorm(num, mu=c(coef,theta), Sigma=vcov(z)),
         nrow = num
         ),

       # as.matrix(m[,1:lengthcoef]),
       # alpha = as.matrix(m[,-1:-lengthcoef])
       
       alpha = NULL,
       link = NULL,
       # linkinv = ZeligChoice:::oprobit.linkinverse
       # CRAN policy on self referential :::
       linkinv = oprobit.linkinverse
       )
}


# @zeta:
# @eta:
oprobit.linkinverse <- function(eta, zeta) {
    tmp1 <- matrix(1, nrow = length(eta), ncol = length(zeta) + 1)
    tmp1[, 1:length(zeta)] <- pnorm(zeta - eta)
    tmp1
}
