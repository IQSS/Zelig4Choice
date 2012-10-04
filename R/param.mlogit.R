#' extract samples from a distribution in order to pass them to the qi function
#' (this is primarily a helper function for the mlogit model)
#' @usage \method{param}{mlogit}(obj, num=1000, ...)
#' @S3method param mlogit
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... ignored parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary parameters
param.mlogit <- function(obj, num=1000, ...) {
  cov <- vcov(obj)
  res <- coef(obj)

  list(
       coef = mvrnorm(num, mu = res, Sigma=cov),
       alpha = res,
       linkinv = NULL
       )
}
