# TESTS
library(ZeligChoice)

# TEST 1
data(japan)

z.out <- zelig(
               cbind(LDP, NFP, SKG, JCP) ~ gender + education + age,
               data = japan,
               verbose = TRUE,
               model = "mprobit"
               )

x.out <- setx(z.out)
s.out <- sim(z.out, x=x.out)
