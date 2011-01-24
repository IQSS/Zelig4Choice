zelig2blogit <- function(model, formula, ..., data) {
  formula <- parse.formula(formula, model)
  tmp <- cmvglm(formula, model, 3)

  
  # return list
  list(
       .function = "vglm",
       
       formula = tmp$formula,
       family  = blogit,
       data = data,
       constraints = tmp$constraints
       )
}
