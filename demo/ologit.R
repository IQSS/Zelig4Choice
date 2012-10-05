data(sanction)

# Fit statistical model
z.out <- zelig(
                cost ~ mil + coop,
                model = "ologit", 
                data = sanction
                )



# Set the explanatory variables to their means, with 'mil' set
# to 0 (no military action in addition to sanctions) in the baseline
# case and set to 1 (military action in addition to sanctions) in the
# alternative case:
x.low <- setx(z.out, coop = 1)
x.high <- setx(z.out, coop = 4)

# Simulate quantities of interest
s.out <- sim(z.out, x = x.low, x1 = x.high)

# Summary of simulated quantities of interest
summary(s.out)

# Plot of simulated quantities of interest
plot(s.out)
