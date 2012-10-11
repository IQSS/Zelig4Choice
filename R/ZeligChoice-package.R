#' Zelig Regressions for Discrete Choices
#'
#' ZeligChoice extends the Zelig Software Suite with five models used to analyze
#' discrete outcomes.
#'
#' \tabular{ll}{
#' Package: \tab ZeligChoice\cr
#' Version: \tab 0.7-0\cr
#' Date: \tab 2012-10-05\cr
#' Depends: Zelig (>= 4.0-11), VGAM (>= 0.8-4)\cr
#' License: \tab GPL version 2 or newer\cr
#' URL: \tab http://projects.iq.harvard.edu/zelig\cr
#' }
#'
#' This package contains the following models:
#' \tabular{ll}{
#' blogit: \tab Bivariate Logist Model, implemented by ``vglm'' \cr
#' bprobit: \tab Bivariate Probit Model, implemented by ``vglm'' \cr
#' mlogit: \tab Multinomial Logit Model, implemented by ``vglm'' \cr
#' ologit: \tab Ordinal Logit Model, implemented by ``polr'' \cr
#' oprobit: \tab Ordinal Probit Model, implemented by ``polr'' \cr
#' }
#' 
#' @name ZeligChoice-package.R
#' @aliases ZeligChoice
#' @docType package
#' @author
#' Matt Owen \email{mowen@@iq.harvard.edu},
#' Kosuke Imai \email{kimai@@princeton.edu},
#' Olivia Lau \email{olivia.lau@@post.harvard.edu} and
#' Gary King \email{gking@harvard.edu}
#' @examples
#' demo("blogit")
#' demo("bprobit")
#' demo("mlogit")
#' demo("ologit")
#' demo("oprobit")
#'
#' @keywords package logit probit ordinal categorical multinomial bivariate multivariate
#' @importFrom Zelig describe param qi
NULL
