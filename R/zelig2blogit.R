#' Interface between \code{blogit} model and Zelig
#' This function is exclusively for use by the \code{zelig} function
#' @param formula a formula
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author James Honaker \email{zelig-zee@@iq.harvard.edu}
zelig2blogit <- function(formula, weights=NULL, repweights=NULL, ..., data) {
  formula <- parse.formula(formula, "blogit")
  tmp <- cmvglm(formula, "blogit", 3)
  built<-zeligBuildWeights(weights=weights, repweights=repweights, zeros="epsilon", data=data)

  
  # return list
  list(
       .function = "vglm",
       
       formula = tmp$formula,
       weights = built$weights,
       family  = blogit,
       data = data,
       constraints = tmp$constraints
       )
}
