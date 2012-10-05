# Load data
data(sanction)

# Fit model
z.out <- zelig(
               cbind(import, export) ~ coop + cost,
               model = "blogit",
               data = sanction
               )

# Set explanatory variables
x.low <- setx(z.out, cost = 1)
x.high <- setx(z.out, cost = 4)

# Simulate quantities of interest
s.out <- sim(z.out, x = x.low, x1 = x.high)

# Summarize simulation results
summary(s.out)

# Plot results
plot(s.out)
