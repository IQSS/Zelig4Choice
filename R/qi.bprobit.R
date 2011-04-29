#' Compute quantities of interest for 'bprobit' Zelig models
#' @usage \method{qi}{bprobit}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi bprobit
#' @param obj a 'zelig' object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#'   though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#'   interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
# exact copy of blogit
qi.bprobit <- qi.blogit
