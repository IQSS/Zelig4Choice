#' Interface between the Zelig Model "mprobit" and the Pre-existing Function
#' @param formula a formula
#' @param choiceX an optional list specifying the choice-specific covariates
#' @param cXnames a vector of names for the choice-specific covariates
#' @param ... parameters to be forwarded to the \code{mnp} function
#' @param data a \code{data.frame}
#' @return a list specifying '.function'
#' @export
zelig2mprobit <- function (formula, choiceX = NULL, cXnames = NULL, ..., data) {

  # if choiceX is set, then evaluate the result
  # along with an attached data.frame
  if (!missing(choiceX))
    choiceX <- eval.in(choiceX, data)


  list(
       .function = "mnp",
       .hook     = "UPDATE.CALL.FORMULA",

       formula = formula,
       data = data,

       choiceX = choiceX,
       cXnames = cXnames,

       ...
       )
}

#' Hook to Clean-up the Function Signature
#' @note This is _NOT_ cosmetic! MNP evaluates choiceX by looking at the call
#'   signature. This is necessary to make the model work with the function
#'   predict.mnp
#' @param obj a \code{zelig} object
#' @param zelig.call the call to the \code{zelig} function
#' @param call a function call
#' @param ...  ignored aprameters
#' @return the \code{obj} parameter with a modified call slot
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
UPDATE.CALL.FORMULA <- function(obj, zelig.call, call, ...) {
  obj$call$formula <- as.formula(call$formula)
  obj$call$choiceX <- call$choiceX
  obj
}
