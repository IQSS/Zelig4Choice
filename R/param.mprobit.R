#' extract samples from a distribution in order to pass them to the qi function
#' (this is primarily a helper function for the mprobit model)
#' @usage \method{param}{mprobit}(obj, num=1000, ...)
#' @S3method param mprobit
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... ignored parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary parameters
param.mprobit <- function(obj, num=1000, ...) {
  list(
       coef = NULL,
       linkinv = NULL
       )
}
