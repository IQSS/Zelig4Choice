#' Describe the Citation of the \code{blogit} Model
#' @S3method describe blogit
#' @export
#' @param ... dummy parameters
#' @return a \code{description} object used to cite the \code{blogit} model
#' @author Matt Owen, Olivia Lau, and Kosuke Imai
describe.blogit <- function(...) {
  text <- "Bivariate Logistic Regression for Dichotomous Dependent Variables"


  parameters <- list()

  # mu
  parameters$mu <- list(equations=c(2, 2),
                        tagsAllowed=TRUE,
                        depVar=TRUE,
                        expVar=TRUE
                        )

  # phi
  parameters$phi <- list(equations=c(1, 1),
                         tagsAllowed=FALSE,
                         depVar=FALSE,
                         expVar=TRUE
                         )

  # return
  list(category = "dichotomous",
       authors = c("Olivia Lau", "Kosuke Imai", "Gary King"),
       year = 2007,
       text = text,
       package = list(name = "VGAM", version = "0.6"),
       parameters = parameters
       )
}
