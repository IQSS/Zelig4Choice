#' @S3method describe blogit
#' @export
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

#' @S3method describe bprobit
#' @export
describe.bprobit <- function(...) {
  text <- "Bivariate Probit Regression for Dichotomous Dependent Variables"


  parameters <- list()

  # mu
  parameters$mu <- list(equations=c(2, 2),
                        tagsAllowed=TRUE,
                        depVar=TRUE,
                        expVar=TRUE
                        )

  # phi
  parameters$rho <- list(equations=c(1, 1),
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

#' @S3method describe mlogit
#' @export
describe.mlogit <- function(...) {
  category <- "multinomial"
  description  <- "Multinomial Logistic Regression for Dependent Variables with Unordered Categorical Values"
  authors <- c('Matthew Owen', 'Olivia Lau', 'Kosuke Imai', 'Gary King')
  year <- 2007
  package <-list(
                 name = "VGAM",
                 version = "0.6"
                 )
  parameters<-list(mu="mu")
  parameters$mu<-list(
                      equations=c(1,Inf),
                      tagsAllowed=FALSE,
                      depVar=TRUE,
                      expVar=TRUE,
                      specialFunction="as.factor",
                      varInSpecialFunction=c(1,1)
                      )
  list(
       category = category,
       authors = authors,
       year = year,
       text = description,
       description=description,
       package=package,
       parameters=parameters
       )
}

#' @S3method describe ologit
describe.ologit <- function (...) {
  list(
       author = c("Matt Owen", "Kosuke Imai", "Olivia Lau", "Gary King"),
       model = "ologit",
       title = "Ordinal Probit Regression for Ordered Categorical Dependent Variables",
       year = 2011
      )
}

#' @S3method describe oprobit
describe.oprobit <- function (...) {
  list(
       author = c("Matt Owen", "Kosuke Imai", "Olivia Lau", "Gary King"),
       model = "oprobit",
       title = "Ordinal Logit Regression for Ordered Categorical Dependent Variables",
       year = 2011
      )
}

