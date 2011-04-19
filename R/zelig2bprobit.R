zelig2bprobit <- function(formula, ..., data) {
  formula <- parse.formula(formula, "bprobit")
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
