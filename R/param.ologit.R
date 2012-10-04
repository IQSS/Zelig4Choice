#' Param Method for the 'ologit' Zelig Model
#' @note This method is used by the 'ologit' Zelig model
#' @S3method param ologit
#' @usage \method{param}{ologit}(obj, num=1000, ...)
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.ologit <- function(obj, num=1000, ...) {
  z <- obj

  coef <- coef(z)
  zeta <- z$result$zeta
  theta <- zeta[1]

  for (k in 2:length(zeta))
    theta[k] <- log(zeta[k]-zeta[k-1])

  m <- matrix(
              mvrnorm(num, mu=c(coef, theta), Sigma=vcov(z)),
              nrow = num
              )

  lengthcoef <- length(coef)

  list(
       simulations =
       matrix(
         mvrnorm(num, mu=c(coef, theta), Sigma=vcov(z)),
         nrow = num
         ),

       # as.matrix(m[,1:lengthcoef]),
       # alpha = as.matrix(m[,-1:-lengthcoef])
       
       alpha = NULL,
       link = NULL,
       linkinv = ZeligChoice:::ologit.linkinverse
       )
}

ologit.linkinverse <- function(eta, zeta) {
  tmp1 <- matrix(1, nrow = length(eta), ncol = length(zeta) + 1)

  # ilogit <- function(e, z) {
  #   exp(z - e) / (1 + exp(z - e))
  # }

  tmp1[, 1:length(zeta)] <- exp(zeta - eta) / (1 + exp(zeta - eta))

  # sapply(zeta, ilogit, e = eta)

  tmp1
}
