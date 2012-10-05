# demo for mlogit
library(multinomial.zelig)

data(mexico)

# fit model
z.out1 <- zelig(as.factor(vote88) ~ pristr + othcok + othsocok, model = "mlogit", 
               data = mexico)


# set explanatory variables
x.weak <- setx(z.out1, pristr = 1)
x.strong <- setx(z.out1, pristr = 3)

# simulated quantities of interest
s.out1 <- sim(z.out1, x = x.strong, x1 = x.weak)

# summary of fitted model
summary(z.out1)

# summary of simulated quantities of interest
summary(s.out1)
