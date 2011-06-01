#' Interface between \code{bprobit} model and Zelig
#' This function is exclusively for use by the \code{zelig} function
#' @param formula a formula
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2bprobit <- function(formula, ..., data) {
  formula <- parse.formula(formula, "bprobit")
  tmp <- cmvglm(formula, "bprobit", 3)

  
  # return list
  list(
       .function = "vglm",
       
       formula = tmp$formula,
       family  = bprobit,
       data = data,
       constraints = tmp$constraints
       )
}
