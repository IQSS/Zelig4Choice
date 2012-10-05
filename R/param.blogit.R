#' @S3method param blogit
param.blogit <- function(obj, num=1000, ...) {
  cov <- vcov(obj)
  res <- coef(obj)

  list(
       simulations = mvrnorm(n=num, mu=res, Sigma=cov),
       alpha = NULL,
       linkinv = binom2.or()@linkinv
       )
}
