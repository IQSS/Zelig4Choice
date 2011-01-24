zelig2bprobit <- function(model, formula, ..., data) {
  formula <- parse.formula(formula, model)
  tmp <- cmvglm(formula, model, 3)

  
  # return list
  list(
       .function = "vglm",
       
       formula = tmp$formula,
       family  = bprobit,
       data = data,
       constraints = tmp$constraints
       )
}
