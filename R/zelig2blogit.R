zelig2.blogit <- function(model, formula, ..., data) {
  # construct
  splitted <- split.formula(formula)
  constraints <- parse.formula(formula)

  #
  list(vglm,
       formula = formula,
       constraints = constraints,
       family = as.name("blogit"),
       "data"
       )
}
