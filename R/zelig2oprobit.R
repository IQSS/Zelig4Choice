#' Interface between \code{oprobit} model and Zelig
#' @note This function is exclusively for use by the \code{zelig} function
#' @param formula a \code{formula}
#' @param weights a numeric vector
#' @param ... ignored parameters
#' @param data a \code{data.frame}
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2oprobit <- function(formula, weights=1, ..., data) {
  list(
       .function = "polr",

       formula = update(formula, as.factor(.) ~ .),
       method  = "probit",
       # add weights + ignore, if empty, class later
       # weights = weights,
       Hess = TRUE,
       data = data
       )
}

