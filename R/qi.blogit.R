qi.blogit <- function(object, x, x1=NULL, y=NULL, param, num=1000) {
  # go back and comment on this later
  #   (pretty straightforward though)
  s4object <- GetObject(object)
  v <- list()
  constr <- s4object@constraints
  all.coef <- NULL

  coefs <- coef(param)


  # string formatting to make sure
  # we index the list correctly later
  # ...
  for (k in 1:3) {
    # if the formula is f ~ x + y + z
    # constr.names should be:
    #   x, y, z, (Intercept)
    #
    # NOTE: this is a pretty circuitous way
    #   of doing this, but there might be
    #   a good reason, so I am maintaing
    #   this at least for now
    #
    constr.names <- NULL

    # get correct names from constraints
    # NOTE: this is a little unelegant
    #  (fix later)
    for (j in 1:length(constr))
      if (sum(constr[[j]][k,]) == 1)
        constr.names <- c(constr.names, names(constr)[j])

    # assign v[[k]]
    v[[k]] <- if (ncol(constr[[k]]) > 1) {
      # formats it like:
      #  v[1] <- x:1, y:1, z:1, ...
      #  v[n] <- x:n, y:n, z:n, ...
      paste(constr.names, k, sep=":")
    }
    
    else
      # and:
      #  v[1] <- x, y, z, ...
      #  v[n] <- x, y, z, ...
      constr.names
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




  ev <- .pp(s4object, constr, all.coef, as.matrix(x), col.names)
  pr <- .pr(ev)

  print(ev)
  print(pr)
  
  # ...
  stop()
}


# predicted probability?
# this function is a little crazy
.pp <- function(object, constr, all.coef, x, col.names) {
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
  ev <- object@family@inverse(sim.eta)

  # assign correct column names
  colnames(ev) <- col.names

  # return
  ev
}


.pr <- function(ev) {
  # (???)
  mpr <- cbind(ev[,3]+ev[,4], ev[,2]+ev[,4])

  #
  index <- matrix(NA, ncol=2, nrow=nrow(mpr))

  # 
  index[,1] <- rbinom(n=length(mpr[,1]), size=1, prob=mpr[,1])
  index[,2] <- rbinom(n=length(mpr[,2]), size=1, prob=mpr[,2])

  # set matrix size
  pr <- matrix(NA, nrow=length(mpr[,1]), ncol=4)

  # count instances correct
  pr[,1] <- as.integer(index[,1] == 0 & index[,2] == 0)
  pr[,2] <- as.integer(index[,1] == 0 & index[,2] == 1)
  pr[,3] <- as.integer(index[,1] == 1 & index[,2] == 0)
  pr[,4] <- as.integer(index[,1] == 1 & index[,2] == 1)

  # title columns
  colnames(pr) <- c(
                    "(Y1=0, Y2=0)",
                    "(Y1=0, Y2=1)",
                    "(Y1=1, Y2=0)",
                    "(Y1=1, Y2=1)"
                    )

  # return
  pr
}
