# Load the sample data: 
data(sanction)

# Estimate the empirical model and returning the coefficients:
z.out <- zelig(
               as.factor(cost) ~ mil + coop,
               model = "oprobit", 
               data = sanction
               )

# Set the explanatory variables to their means, with 'mil' set
# to 0 (no military action in addition to sanctions) in the baseline
# case and set to 1 (military action in addition to sanctions) in the
# alternative case:
x.low <- setx(z.out, mil = 0)
x.high <- setx(z.out, mil = 1)

# Generate simulated fitted values and first differences, and view 
# the results:
s.out <- sim(z.out, x = x.low, x1 = x.high)

# Summary of simulated quantities of interest
summary(s.out)

# Plot of simulated quantities of interest
plot(s.out)
