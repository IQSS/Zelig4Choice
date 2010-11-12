summarize.blogit <- function(q) {
  # error-catching
  if (is.null(q$titles) || is.null(q$stats))
    stop("qi function missing qi's or their titles")


  #
  i <- iter(q)
  res <- list()


  repeat {
    item <- try(nextElem(i), silent=T)

    if (inherits(item, "try-error"))
      break

    # for code clarity
    key <- item$key
    val <- item$value

    
    # make a matrix that is data-friendly
    m <- if (is.numeric(val))
      matrix(NA, nrow=ncol(val), ncol=5)
    else if (is.character(val) || is.factor(val)) {
      matrix(NA, nrow=ncol(val), ncol=length(unique(c(val))))
    }

    #
    for (k in 1:ncol(val)) {
      if (is.numeric(val[,k])) {
        m[k,] <- c(mean(val[,k]),
                   sd(val[,k]),
                   quantile(val[,k], c(.5, .025, .975))
                   )

        #
        colnames(m) <- c("mean", "sd", "50%", "2.5%", "97.5%")
      }
    
      else if (is.character(val[,k]) || is.factor(val[,k])) {
        result.table <- c(table(val[,k])/length(val[,k]))
        result.table <- result.table[sort(names(result.table))]

        m[k,] <- result.table
        colnames(m) <- names(result.table)

      }

      else
        m[k,] <- NA

      rownames(m) <- colnames(val)
    }

    # add to list
    res[[key]] <- m
  }

  # cast as class - for some reason - then return
  class(res) <- "summarized.qi"
  res
}
