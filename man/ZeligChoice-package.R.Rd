\docType{package}
\name{ZeligChoice-package.R}
\alias{ZeligChoice}
\alias{ZeligChoice-package.R}
\title{Zelig Regressions for Discrete Choices}
\description{
  ZeligChoice extends the Zelig Software Suite with five
  models used to analyze models discrete outcomes.
}
\details{
  \tabular{ll}{ Package: \tab ZeligChoice\cr Version: \tab
  0.7-0\cr Date: \tab 2012-10-05\cr Depends: Zelig (>=
  4.0-11), VGAM (>= 0.8-4)\cr License: \tab GPL version 2
  or newer\cr URL: \tab http://gking.harvard.edu/zelig\cr }

  Add-on pack for Zelig, containing models: \tabular{ll}{
  blogit: \tab Bivariate Logist Model, implemented by
  ``vglm'' \cr bprobit: \tab Bivariate Probit Model,
  implemented by ``vglm'' \cr mlogit: \tab Multinomial
  Logit Model, implemented by ``vglm'' \cr ologit: \tab
  Ordinal Logit Model, implemented by ``polr'' \cr oprobit:
  \tab Ordinal Probit Model, implemented by ``polr'' \cr }
}
\examples{
demo("blogit")
demo("bprobit")
demo("mlogit")
demo("ologit")
demo("oprobit")
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}, Kosuke Imai,
  Olivia Lau and Gary King Maintainer: Matt Owen
  \email{mowen@iq.harvard.edu}
}
\keyword{bivariate}
\keyword{categorical}
\keyword{logit}
\keyword{multinomial}
\keyword{multivariate}
\keyword{ordinal}
\keyword{package}
\keyword{probit}

