#' interface between the Zelig model mlogit and the pre-existing function
#' @param formula a formula
#' @param ... ignored parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2mlogit <- function (formula, ..., data) {

  .formula <- parse.formula(formula, "mlogit", data)
  .tt <- terms(.formula)
  .fact <- attr(.tt, "depFactors")$depFactorVar
  ndim <- length(attr(.tt, "depFactors")$depLevels)

  cmv <- cmvglm(.formula, "mlogit", ndim, data, .fact)

  list(
       .function = "vglm",

       formula = cmv$formula,
       constraints = cmv$constraints,

       family = VGAM::multinomial,
       data = data
       )
}
