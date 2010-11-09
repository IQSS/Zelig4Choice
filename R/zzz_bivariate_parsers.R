# @form: a formula object
# return: a list of formulas
split.formula <- function(form) {
  if (!inherits(form, "formula"))
    stop("`form` must be a formula data-type")

  lhs <- form[[2]]
  rhs <- form[[3]]

  # error-catching
  if (!inherits(lhs, "call"))
    stop("left hand side of `form` does not follow the proper syntax")

  if (lhs[[1]] != "cbind")
    stop("left hand side of `form` must be a call to cbind")

  if (length(lhs) != 3)
    stop("left hand side of `form` is not bivariate")

  # left and right side
  var1 <- lhs[[2]]
  var2 <- lhs[[3]]

  # return parsed formula
  res <- list(mu1 = update(form, paste(var1, "~ .")),
              mu2 = update(form, paste(var2, "~ .")),
              phi = ~ 1
              )

  # return
  class(res) <- c("multiple", "list")
  res
}


# @formula: a formula object
# return: constraints built from the formula
parse.formula <- function(formula) {
  # split formula
  formulas <- split.formula(formula)

  # termlabels
  termlabels <- list()

  # get termlabels foreach
  for (key in names(formulas)) {
    #
    form <- formulas[[key]]

    # get the terms of each formula
    # specials are id and tag, but will not be
    # implemented yet (10/15/2010)
    tt <- terms(form, specials=c("id", "tag"))

    # dependent variables for each
    termlabels[[key]] <- c("(Intercept)", attr(tt, "term.labels"))
  }

  # the ancillary parameters
  # that is, the parameters that only have intercepts
  fixed <- names(which(lapply(termlabels, length) == 0))

  # the systematic component
  # apparently it's everything... including the ancillary
  systematic <- names(formulas)

  # contrained variables are FALSE for now
  constraints <- FALSE

  # all the variables within each equation
  all.labels <- unique(unlist(termlabels))

  # setup parameter matrix
  pars <- matrix(NA, ncol=length(systematic), nrow=length(all.labels))

  # name the dimensions
  colnames(pars) <- systematic
  rownames(pars) <- all.labels

  # fils parameters matrix correctly
  for (row in rownames(pars))
    for(col in colnames(pars))
      if (row %in% termlabels[[col]])
        pars[[row,col]] <- paste(row, col, sep=":")

  # compute constraints
  matrix.list <- vector("list", length(all.labels))
  names(matrix.list) <- all.labels

  # 
  for (key in names(matrix.list))
    matrix.list[[key]] <- diag(1, 3)

  
  # do some kind of weird diagnalization thing
  for (row in rownames(pars))
    for (col in 1:ncol(pars))
      if (is.na(pars[[row,col]]))
        matrix.list[[row]][col,col] <- 0

  #
  for (key in names(matrix.list)) {
    # simplify
    m <- matrix.list[[key]]
    
    # get the sums of each row
    row.sums <- apply(m, 2, sum)

    # get non-zero rows
    m <- as.matrix(m[, row.sums != 0])

    # re-assign to matrix.list
    matrix.list[[key]] <- m
  }


  # return constraints
  matrix.list
}
