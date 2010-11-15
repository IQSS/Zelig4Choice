param.blogit <- function(object, num=1000, bootstrap=FALSE) {
  cov <- vcov(object)
  res <- coef(object)
  list(
       simulations = mvrnorm(n=num, mu=res, Sigma=cov),
       alpha = NULL,
       fam = binom2.or()
       )
}
