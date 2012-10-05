# Load the sample data: 
data(sanction)

# Estimate the empirical model and returning the coefficients:
z.out1 <- zelig(as.factor(cost) ~ mil + coop, model = "oprobit", 
                    data = sanction)

# Set the explanatory variables to their means, with 'mil' set
# to 0 (no military action in addition to sanctions) in the baseline
# case and set to 1 (military action in addition to sanctions) in the
# alternative case:
x.low <- setx(z.out1, mil = 0)
x.high <- setx(z.out1, mil = 1)

# Generate simulated fitted values and first differences, and view 
# the results:
s.out1 <- sim(z.out1, x = x.low, x1 = x.high)

# Summary of fitted statistical model

summary(z.out1)

# Summary of simulated quantities of interest

summary(s.out1)

# Plot of simulated quantities of interest

plot(s.out1)

##### Example 2: Creating An Ordered Dependent Variable #####

# Create an ordered dependent variable: 
sanction$ncost <- factor(sanction$ncost, ordered = TRUE,
                         levels = c("net gain", "little effect", 
                         "modest loss", "major loss"))

# Z.Out the model:
z.out2 <- zelig(ncost ~ mil + coop, model = "oprobit", data = sanction)
summary(z.out2)

# Set the explanatory variables to their observed values:  
x.out2 <- setx(z.out2, fn = NULL)

# Simulate fitted values given Xval and view the results:
s.out2 <- sim(z.out2, x = x.out2)

summary(s.out2)

plot(s.out2)










