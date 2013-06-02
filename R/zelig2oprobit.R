#' Interface between \code{oprobit} model and Zelig
#' @note This function is exclusively for use by the \code{zelig} function
#' @param formula a \code{formula}
#' @param weights a numeric vector
#' @param ... ignored parameters
#' @param data a \code{data.frame}
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author James Honaker \email{zelig-zee@@iq.harvard.edu}
zelig2oprobit <- function(formula, weights=NULL, repweights=NULL, ..., data) {

  built<-zeligBuildWeights(weights=weights, repweights=repweights, allowweights=FALSE, data=data)

  list(
       .function = "polr",

       formula = update(formula, as.factor(.) ~ .),
       method  = "probit",
       weights = built$weights,
       Hess = TRUE,
       data = data
       )
}

