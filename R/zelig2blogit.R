#' Interface between \code{blogit} model and Zelig
#' This function is exclusively for use by the \code{zelig} function
#' @param formula a formula
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2blogit <- function(formula, ..., data) {
  formula <- parse.formula(formula, "blogit")
  tmp <- cmvglm(formula, "blogit", 3)

  
  # return list
  list(
       .function = "vglm",
       
       formula = tmp$formula,
       family  = blogit,
       data = data,
       constraints = tmp$constraints
       )
}
