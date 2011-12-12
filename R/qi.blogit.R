#' Compute quantities of interest for 'blogit' Zelig models
#' @usage \method{qi}{blogit}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi blogit
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
qi.blogit <- function(obj, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  # go back and comment on this later
  #   (pretty straightforward though)
  s4object <- GetObject(obj)
  v <- list()
  constr <- s4object@constraints
  all.coef <- NULL

  coefs <- coef(param)

  # string formatting to make sure
  # we index the list correctly later
  # ...
##   for (k in 1:3) {
##     # if the formula is f ~ x + y + z
##     # constr.names should be:
##     #   x, y, z, (Intercept)
##     #
##     # NOTE: this is a pretty circuitous way
##     #   of doing this, but there might be
##     #   a good reason, so I am maintaing
##     #   this at least for now
##     #
##     constr.names <- NULL

##     # get correct names from constraints
##     # NOTE: this is a little unelegant
##     #  (fix later)
##     for (j in 1:length(constr))
##       if (sum(constr[[j]][k,]) == 1)
##         constr.names <- c(constr.names, names(constr)[j])

##     # assign v[[k]]
##     v[[k]] <- if (ncol(constr[[k]]) > 1) {
##       # formats it like:
##       #  v[1] <- x:1, y:1, z:1, ...
##       #  v[n] <- x:n, y:n, z:n, ...
##       paste(constr.names, k, sep=":")
##     }
    
##     else
##       # and:
##       #  v[1] <- x, y, z, ...
##       #  v[n] <- x, y, z, ...
##       constr.names
##   }

  cm <- constr
  v <- rep(list(NULL), 3)
  for(i in 1:length(cm)) {
    if(ncol(cm[[i]])==1){
      for(j in 1:3)
        if(sum(cm[[i]][j,])==1)
          v[[j]] <- c(v[[j]], names(cm)[i])
    }
    else {
      for (j in 1:3)
        if (sum(cm[[i]][j,])==1)
          v[[j]] <- c(v[[j]], paste(names(cm)[i], ":", j, sep=""))
    }
  }

  # get coefs
  for(i in 1:3)
    all.coef[[i]] <- coefs[ , unlist(v[i]) ]

  # column names
  col.names <- c("Pr(Y1=0, Y2=0)",
                 "Pr(Y1=0, Y2=1)",
                 "Pr(Y1=1, Y2=0)",
                 "Pr(Y1=1, Y2=1)"
                 )

  #
  ev <- .pp(s4object, constr, all.coef, as.matrix(x))
  pr <- .pr(ev)
  
  #
  qi <- list("Predicted Probabilities: Pr(Y1=k|X)" = ev,
             "Predicted Values: Y=k|X" = pr 
             )

  # compute first-differences and risk ratios
  if (!is.null(x1)) {
    # evaluate probabilities using x1
    ev1 <- .pp(s4object, constr, all.coef, as.matrix(x1))

    # compute first differences
    fd <- ev1-ev

    # ... risk ratio
    rr <- ev1/ev

    # add to qi object
    qi$"First Differences: Pr(Y=k|X1)-Pr(Y=k|X)" <- fd
    qi$"Risk Ratios: Pr(Y=k|X1) / Pr(Y=k|X)" <- rr
  }


  # average treatment effect code
#   if (!is.null(y)) {
#     warning("The `Average Treatment Effect` for this model is ",
#             "currently under construction, and may give unreliable",
#             "results.  Sorry for any inconvenience."
#             )
#     
#     # initialize temp variables
#     tmp.ev <- tmp.pr <- array(NA, dim=dim(qi[[1]]))
# 
#     # initialize average treatment effect variables
#     att.ev <- att.pr <- matrix(NA, nrow=nrow(ev), ncol=ncol(ev))
# 
#     #
#     yvar <- .make.truth.table(y)
#     pr.idx <- matrix(NA, nrow=nrow(pr), 4)
#     
#     #
#     pr.idx[,1] <- as.integer(pr[,1])
#     pr.idx[,2] <- as.integer(pr[,2])
#     pr.idx[,3] <- as.integer(pr[,3])
#     pr.idx[,4] <- as.integer(pr[,4])
# 
#     # name columns in the correct fashion
#     colnames(att.ev) <- colname(att.pr) <- c("(Y1=0, Y2=0)",
#                                              "(Y1=0, Y2=1)",
#                                              "(Y1=1, Y2=0)",
#                                              "(Y1=1, Y2=1)"
#                                              )
# 
#     #
#     for (k in 1:3) {
#       for (j in 1:nrow(ev)) {
#         tmp.ev[j,k] <- yvar[,k] - ev[j,k]
#         tmp.pr[j,k] <- yvar[,k] - pr.idx[j,k]
#       }
# 
#       att.ev <- apply(temp.ev[,k], 1, mean)
#       att.pr <- apply(temp.pr[,k], 1, mean)
#     }
# 
#     #
#     qi$"Average Treatment Effect for the Treated: Y - E[...]" <- att.ev
#     qi$"Average Treatment Effect for the Treated: Y - Pr[...]" <- att.pr
#   }

  # return
  qi
}


# @object: the bivariate logit statistical model
# @constr: "constraints" of model
# @all.coef:
# @x: a matrix derived from a setx class
# return: 
.pp <- function(object, constr, all.coef, x) {
  # xm will hold individual columns
  # of the setx object
  xm <- list()
  xm <- rep(list(NULL), 3)
  sim.eta <- NULL


  # again this can definitely be written better
  for (i in 1:length(constr))
    for (j in 1:3)
      if (sum(constr[[i]][j,]) == 1)
        xm[[j]] <- c(xm[[j]], x[,names(constr)[i]])


  sim.eta <- cbind(
               all.coef[[1]] %*% as.matrix( xm[[1]] ),
               all.coef[[2]] %*% as.matrix( xm[[2]] ),
               all.coef[[3]] %*% as.matrix( xm[[3]] )
               )


  # compute inverse (theta)
  ev <- object@family@linkinv(sim.eta)

  # assign correct column names
  colnames(ev) <- c("Pr(Y1=0, Y2=0)",
                    "Pr(Y1=0, Y2=1)",
                    "Pr(Y1=1, Y2=0)",
                    "Pr(Y1=1, Y2=1)"
                    )

  # return
  ev
}


.pr <- function(ev) {
  # (???)
  mpr <- cbind(ev[,3]+ev[,4], ev[,2]+ev[,4])

  #
  index <- matrix(NA, ncol=2, nrow=nrow(mpr))

  # 
  index[,1] <- rbinom(n=nrow(ev), size=1, prob=mpr[,1])
  index[,2] <- rbinom(n=nrow(ev), size=1, prob=mpr[,2])

  # set matrix size
  pr <- matrix(NA, nrow=nrow(ev), ncol=4)

  # count instances correct
  pr[,1] <- as.integer(index[,1] == 0 & index[,2] == 0)
  pr[,2] <- as.integer(index[,1] == 0 & index[,2] == 1)
  pr[,3] <- as.integer(index[,1] == 1 & index[,2] == 0)
  pr[,4] <- as.integer(index[,1] == 1 & index[,2] == 1)

  #pr <- .make.match.table(index)

  # title columns
  colnames(pr) <- c(
                    "(Y1=0, Y2=0)",
                    "(Y1=0, Y2=1)",
                    "(Y1=1, Y2=0)",
                    "(Y1=1, Y2=1)"
                    )
  pr <- apply(pr, 2, as.character)
  levels(pr) <- c("0","1")
  # return
  pr
}


# @index: matrix containing two columns with 0 or 1 values
# @cols: character-vector containing of length 4
# return: n by 4 matrix
.make.match.table <- function(index, cols=NULL) {
  # a matrix with:
  #  same number of rows
  #  4 columns (combinations of 2 independent values being TRUE/FALSE)
  pr <- matrix(0, nrow=nrow(index), ncol=4)

  # assigns values by the rule:
  #   pr[j,1] = 1 iff index[j,1] == 0 && index[j,2] == 0
  #   pr[j,2] = 1 iff index[j,1] == 0 && index[j,2] == 1
  #   pr[j,3] = 1 iff index[j,1] == 1 && index[j,2] == 0
  #   pr[j,4] = 1 iff index[j,1] == 1 && index[j,2] == 1
  #
  # NOTE: only one column can be true at a time, so as a result
  #       we can do a much more elegant one liner, that I'll code
  #       later.  In this current form, I don't think this actually
  #       explains what is going on.
  pr[,1] <- as.integer(index[,1] == 0 & index[,2] == 0)
  pr[,2] <- as.integer(index[,1] == 0 & index[,2] == 1)
  pr[,3] <- as.integer(index[,1] == 1 & index[,2] == 0)
  pr[,4] <- as.integer(index[,1] == 1 & index[,2] == 1)

  # convert to binary (ha!)
  # k <- 2*as.integer(index[,1] == 1) + as.integer(index[,2] == 1)

  # assign to column based on this
  # pr[,k+1] <- 1

  # assign column names
  colnames(pr) <- if (is.character(cols) && length(cols)==4)
    cols
  else
    # default values
    c("(Y1=0, Y2=0)",
      "(Y1=0, Y2=1)",
      "(Y1=1, Y2=0)",
      "(Y1=1, Y2=1)"
      )

  # return
  pr
}
