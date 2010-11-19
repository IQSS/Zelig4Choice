zelig2weibull <- function(model,
                          formula,
                          ...,
                          robust = F,
                          cluster = NULL,
                          data) {
  # 
  str <- deparse(formula[[2]])
  str <- paste(". ~ . + cluster(1:nrow(", str, "))", sep="")

  formula <- update(formula, paste(". ~ . + ", paste("cluster(1:nrow(",deparse(formula[[2]]),"))")))

  # return
  list("survreg",
       formula = formula,
       dist    = "weibull",
       robust  = robust,
       cluster = cluster,
       "data"
       )
}
