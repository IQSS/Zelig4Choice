#' interface between the Zelig model mlogit and the pre-existing function
#' @param formula a formula
#' @param ... ignored parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2mlogit <- function (formula, weights=NULL, repweights=NULL, ..., data) {

  .formula <- parse.formula(formula, "mlogit", data)
  .tt <- terms(.formula)
  .fact <- attr(.tt, "depFactors")$depFactorVar
  ndim <- length(attr(.tt, "depFactors")$depLevels)

  cmv <- cmvglm(.formula, "mlogit", ndim, data, .fact)
  built<-zeligBuildWeights(weights=weights, repweights=repweights, zeros="epsilon", data=data)


  list(
       .function = "vglm",

       formula = cmv$formula,
       constraints = cmv$constraints,
       weights=built$weights,

       family = VGAM::multinomial,
       data = data
       )
}
