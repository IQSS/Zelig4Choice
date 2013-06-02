#' Interface between \code{bprobit} model and Zelig
#' This function is exclusively for use by the \code{zelig} function
#' @param formula a formula
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author James Honaker \email{zelig-zee@@iq.harvard.edu}
zelig2bprobit <- function(formula, weights=NULL, repweights=NULL, ..., data) {
  formula <- parse.formula(formula, "bprobit")
  tmp <- cmvglm(formula, "bprobit", 3)
  built<-zeligBuildWeights(weights=weights, repweights=repweights, zeros="epsilon", data=data)


  
  # return list
  list(
       .function = "vglm",
       
       formula = tmp$formula,
       family  = bprobit,
       weights = built$weights,
       data = data,
       constraints = tmp$constraints
       )
}
