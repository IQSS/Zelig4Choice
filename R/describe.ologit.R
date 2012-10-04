#' describe a \code{ologit} model to zelig
#' @param ... ignored parameters
#' @return a list to be processed by \code{as.description}
#' @author matt owen \email{mowen@@iq.harvard.edu}
#' @export
describe.ologit <- function (...) {
  list(
       author = c("Matt Owen", "Kosuke Imai", "Olivia Lau", "Gary King"),
       model = "ologit",
       title = "Ordinal Probit Regression for Ordered Categorical Dependent Variables",
       year = 2011
      )
}
