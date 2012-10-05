# Load 1988 Mexican Presidential Election Data
data(mexico)

# Fit model
z.out <- zelig(as.factor(vote88) ~ pristr + othcok + othsocok, model = "mlogit", 
               data = mexico)


# Set explanatory variables
x.weak <- setx(z.out, pristr = 1)
x.strong <- setx(z.out, pristr = 3)

# Simulated quantities of interest
s.out <- sim(z.out, x = x.strong, x1 = x.weak)

# Summary of simulated quantities of interest
summary(s.out)

# Plot results
plot(s.out)
