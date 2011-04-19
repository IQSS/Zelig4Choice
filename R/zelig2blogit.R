zelig2blogit <- function(formula, ..., data) {
  formula <- parse.formula(formula, "blogit")
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
