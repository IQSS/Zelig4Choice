zelig2blogit <- function(model, formula, ..., data) {
  # ...
  formula <- parse.formula(formula, model)
  tmp <- cmvglm(formula, model, 3)
  
  # return list
  list(vglm,
       formula = tmp$formula,
       constraints = tmp$constraints,
       family = as.name("blogit"),
       "data"
       )
}
