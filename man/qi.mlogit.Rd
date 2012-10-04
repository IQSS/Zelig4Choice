\name{qi.mlogit}
\alias{qi.mlogit}
\title{compute quantities of interest for the Zelig model mlogit}
\usage{
  \method{qi}{mlogit}(obj, x=NULL, x1=NULL, y=NULL,
    num=1000, param=NULL)
}
\arguments{
  \item{obj}{a zelig object}

  \item{x}{a setx object}

  \item{x1}{an optional setx object}

  \item{y}{...}

  \item{num}{an integer specifying the number of
  simulations to compute}

  \item{param}{a parameters object}
}
\value{
  a list of key-value pairs specifying pairing titles of
  quantities of interest with their simulations
}
\description{
  compute quantities of interest for the Zelig model mlogit
}

