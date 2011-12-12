#' Param Method for the 'bprobit' Zelig Model
#' @note This method is used by the 'bprobit' Zelig model
#' @S3method param bprobit
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.bprobit <- function(obj, num=1000, ...) {
  cov <- vcov(obj)
  res <- coef(obj)
  
  list(
       simulations = mvrnorm(n=num, mu=res, Sigma=cov),
       alpha = NULL,
       linkinv = binom2.rho()@linkinv
       )
}

